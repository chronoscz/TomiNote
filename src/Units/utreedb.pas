unit utreedb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3dyn, sqlite3conn, sqldb, LazUTF8, RegExpr, IniFiles, FileUtil, ucommon;

type

  // 从 ComCtrls 中复制过来的，元素顺序必须与 ComCtrls 中保持一直
  TAttachMode = (
    naAdd,
    naAddFirst,
    naAddChild,
    naAddChildFirst,
    naInsert,
    naInsertBehind
    );

  { TBackupThread }

  TBackupThread = class(TThread)
    FDatabaseName: string;
    FBackupFile: string;
  protected
    procedure Execute; override;
  public
    constructor Create(FromFile, ToFile: string);
  end;

  { TDBConfig }

  TDBConfig = class(TObject)
    DatabaseVersion: string;
    LastNodeID: Integer;
    AutoBackupRemaining: integer;
    ChangedAfterBackup: boolean;
    public
      procedure Load(AData: string);
      function  Save: string;
  end;

  { TTreeDB }

  TTreeDB = class(TObject)
  private
    FSQLConn: TSQLite3Connection;
    FSQLQuery: TSQLQuery;
    FSQLTran: TSQLTransaction;
    FAutoOpenTran: boolean;

    FDatabaseName: string;
    FActive: boolean;
    FChanged: boolean;
    FActiveChanged: TNotifyEvent;
    FConfig: TDBConfig;

    procedure CreateTable;
    function  TableExists(TableName: string): boolean;
    procedure InitTable;
    // 从储备库(rowid=3)中取出一个节点重新使用
    function  ReuseNode: Integer;
    // 向数据库中添加一个节点
    function  CreateNewNode(Name, Note: string): Integer;
    // 将一个节点从树上脱离
    procedure DetachNode(ID: Integer);
    // 将一个节点停靠在树上
    procedure AttachNode(ID: Integer; ToID: Integer; mode: TAttachMode);
    procedure SetParent(ID: Integer; ParentID: Integer);

    procedure DataChanged(AChanged: boolean);
    procedure ActiveChanged(AActive: boolean);
  public
    constructor Create;
    destructor Destroy; override;

    // 数据库文件
    function  OpenDB(FileName: string): boolean;
    procedure SaveDB;
    function  CloseDB(Save: boolean): boolean;
    function  BackupDB(BackupDir: string; Count: integer): boolean;

    // 基本读写
    function  AddNode(Name, Note: string; ToID: Integer; mode: TAttachMode): Integer;
    procedure DelNode(ID: Integer);
    procedure RecycleNode(ID: Integer);
    procedure EmptyRecycler;
    procedure MoveNode(FromID, ToID: Integer; Mode: TAttachMode);
    procedure UpDownNode(ID: Integer; Up: boolean);
    function  CopyNode(FromID, ToID: Integer; Mode: TAttachMode): Integer;
    function  GetParent(ID: Integer): Integer;
    function  GetChildren(ID: Integer): TBoundArray;
    procedure SetChildren(ID: Integer; Children: TBoundArray);
    function  GetName(ID: Integer): string;
    procedure SetName(ID: Integer; Name: string);
    function  GetNote(ID: Integer): string;
    procedure SetNote(ID: Integer; Note: string);

    // 导入和导出
    function  ImportFile(FileName: string; IncludeExt: boolean; ToID: Integer; mode: TAttachMode): Integer;
    function  ImportDir(FromDir: string; IncludeRoot: boolean; IncludeExt: boolean;
      ToID: Integer; mode: TAttachMode): Integer;
    function  ImportDB(FromDBFile: string; FromID, ToID: Integer; mode: TAttachMode): Integer;

    procedure ExportToDir(ID: Integer; ToDir: string; Ext: string; Depth: integer);
    procedure ExportToFile(ID: Integer; ToFile, Splitter: string; Depth: integer);
    function  ExportToDB(ID, ToID: Integer; ToDBFile: string; Depth: integer): boolean;

    // 搜索和替换
    procedure Search(ID: Integer; ASearchText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; IgnoreCase: boolean; var Rst: TSearchResult);
    procedure Replace(ID: Integer; ASearchText, AReplaceText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; IgnoreCase: boolean; var Rst: TSearchResult);
    procedure RegSearch(ID: Integer; ASearchText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; var Rst: TSearchResult);
    procedure RegReplace(ID: Integer; ASearchText, AReplaceText: string; IncludeName, IncludeNote: boolean;
      Depth: integer; var Rst: TSearchResult);

    // 属性
    property  AutoOpenTran: boolean read FAutoOpenTran write FAutoOpenTran;
    property  DatabaseName: string read FDatabaseName;
    property  Config: TDBConfig read FConfig write FConfig;
    property  Active: boolean read FActive;
    property  Changed: boolean read FChanged;
    property  OnActiveChanged: TNotifyEvent read FActiveChanged write FActiveChanged;
  end;

const
  // 数据库版本
  Version       = '1.0';

  // 数据表名称和字段名称
  NoteTable     = 'tnote';
  ParentField   = 'parent';
  ChildrenField = 'children';
  NameField     = 'name';
  NoteField     = 'note';

  AllDepth      = 0;

const
  // 固定 ID
  FreeID      = 0;
  RootID      = 1;
  RecyclerID  = 2;
  ReserveID   = 3;
  ConfigID    = 4;
  SpareID     = 5;

  // 在导出节点时，如果一个节点含有子节点，则将其内容保存在指定名称的文件中
  DirNode = '$DirectoryNode$';

implementation

var
  // SQLite3 的动态链接库文件
  LibFile: string;

{ TBackupThread }

constructor TBackupThread.Create(FromFile, ToFile: string);
begin
  FDatabaseName := FromFile;
  FBackupFile := ToFile;
  inherited Create(False);
end;

procedure TBackupThread.Execute;
var
  ATreeDB: TTreeDB;
begin
  Self.FreeOnTerminate := True;
  ATreeDB := TTreeDB.Create;
  ATreeDB.AutoOpenTran := False;
  try
    ATreeDB.OpenDB(FDatabaseName);
    ATreeDB.ExportToDB(RootID, RootID, FBackupFile, AllDepth);
    ATreeDB.ExportToDB(RecyclerID, RecyclerID, FBackupFile, AllDepth);
    ATreeDB.CloseDB(False);
  finally
    ATreeDB.Free;
  end;
end;

{ TDBConfig }

procedure TDBConfig.Load(AData: string);
var
  AStream: TStringStream;
  IniFile: TIniFile;
begin
  AStream := TStringStream.Create(AData);
  try
    IniFile := TIniFile.Create(TStream(AStream));
    try
      DatabaseVersion     := IniFile.ReadString ('Config', 'Version', Version);
      LastNodeID          := IniFile.ReadInteger('Config', 'LastNodeID', 0);
      AutoBackupRemaining := IniFile.ReadInteger('Config', 'AutoBackupRemaining', -1);
      ChangedAfterBackup  := IniFile.ReadBool   ('Config', 'ChangedAfterBackup', False);
    finally
      IniFile.Free;
    end;
  finally
    AStream.Free;
  end;
end;

function TDBConfig.Save: string;
var
  AStream: TStringStream;
  IniFile: TIniFile;
begin
  AStream := TStringStream.Create('');
  try
    IniFile := TIniFile.Create(TStream(AStream));
    try
      IniFile.WriteString ('Config', 'Version', DatabaseVersion);
      IniFile.WriteInteger('Config', 'LastNodeID', LastNodeID);
      IniFile.WriteInteger('Config', 'AutoBackupRemaining', AutoBackupRemaining);
      IniFile.WriteBool   ('Config', 'ChangedAfterBackup', ChangedAfterBackup);
    finally
      IniFile.Free;
    end;
    Result := AStream.DataString;
  finally
    AStream.Free;
  end;
end;

{ 数据库文件 }

constructor TTreeDB.Create;
begin
  inherited Create;

  FConfig := TDBConfig.Create;

  FDatabaseName := '';
  FActive := False;
  FChanged := False;
  FAutoOpenTran := True;

  FSQLConn := TSQLite3Connection.Create(nil);
  FSQLTran := TSQLTransaction.Create(nil);
  FSQLQuery := TSQLQuery.Create(nil);

  FSQLConn.Transaction := FSQLTran;
  FSQLTran.DataBase := FSQLConn;
  FSQLQuery.DataBase := FSQLConn;
end;

destructor TTreeDB.Destroy;
begin
  FreeAndNil(FSQLQuery);
  FreeAndNil(FSQLTran);
  FreeAndNil(FSQLConn);

  FreeAndNil(FConfig);

  inherited Destroy;
end;

function TTreeDB.OpenDB(FileName: string): boolean;
begin
  Result := False;

  if FActive or (FileName = '') then Exit;

  FDatabaseName := FileName;
  FSQLConn.DatabaseName := FDatabaseName;
  FSQLConn.Open;

  if not FSQLConn.Connected then begin
    FDataBaseName := '';
    Exit;
  end;

  CreateTable;

  if FAutoOpenTran then FSQLTran.Active := True;

  FConfig.Load(GetNote(ConfigID));

  ActiveChanged(FSQLConn.Connected);

  Result := True;
end;

procedure TTreeDB.SaveDB;
begin
  if FSQLTran.Active then begin
    if FChanged then Config.ChangedAfterBackup := True;
    SetNote(ConfigID, FConfig.Save);

    FSQLTran.Commit;
  end;

  // 根据需要自动开启事务，方便后面的写入
  if FAutoOpenTran then FSQLTran.Active := True;

  DataChanged(False);
end;

function TTreeDB.CloseDB(Save: boolean): boolean;
begin
  Result := False;

  if FSQLTran.Active then begin
    if Save then
      SaveDB
    else begin
      FSQLTran.Rollback;

      // 即使不保存数据，也要保存数据库配置信息
      if FAutoOpenTran then begin
        FSQLTran.Active := True;
        SaveDB;
      end;
    end;
  end;

  FSQLConn.Close;
  if FSQLConn.Connected then Exit;

  FDataBaseName := '';

  ActiveChanged(FSQLConn.Connected);

  Result := True;
end;

function TTreeDB.BackupDB(BackupDir: string; Count: integer): boolean;
var
  i: integer;
  BackupName: string;
  BackupFile, BackupFile2: string;
  MaxCount: integer;
begin
  Result := False;

  if not FileExists(FDatabaseName) then Exit;

  BackupName := ExtractFileName(FDatabaseName) + '.bak';
  ForceDirectories(BackupDir);

  // 如果设置 MaxCount=99，则可以删除序号大于 Count 的备份文件
  MaxCount := Count;
  for i := MaxCount downto Count do
  begin
    BackupFile := ChangeFileExt(BackupName, '.bak_' + Format('%.2d', [i]));
    BackupFile := ConcatPaths([BackupDir, BackupFile]);

    if FileExists(BackupFile) and not DeleteFile(BackupFile) then
      Exit;
  end;

  BackupFile := '';

  for i := Count - 1 downto 1 do
  begin
    BackupFile := ChangeFileExt(BackupName, '.bak_' + Format('%.2d', [i]));
    BackupFile := ConcatPaths([BackupDir, BackupFile]);

    BackupFile2 := ChangeFileExt(BackupName, '.bak_' + Format('%.2d', [i + 1]));
    BackupFile2 := ConcatPaths([BackupDir, BackupFile2]);

    if FileExists(BackupFile) and not RenameFile(BackupFile, BackupFile2) then
      Exit;
  end;

  TBackupThread.Create(FDatabaseName, BackupFile);

  Config.ChangedAfterBackup := False;

  Result := True;
end;

procedure TTreeDB.CreateTable;
var
  OldChanged: Boolean;
begin
  if not TableExists(NoteTable) then
  begin
    OldChanged := FChanged;
    FSQLTran.Active := True;

    FSQLConn.ExecuteDirect(Format('Create Table "%s"(' +
      ' "%s" Integer Not Null,' +
      ' "%s" Blob,' +
      ' "%s" Text,' +
      ' "%s" Text);',
      [NoteTable, ParentField, ChildrenField, NameField, NoteField]));
    InitTable;

    FSQLTran.Commit;
    DataChanged(OldChanged);
  end;
end;

function TTreeDB.TableExists(TableName: string): boolean;
var
  TableNames: TStringList;
begin
  Result := False;

  TableNames := TStringList.Create;
  try
    FSQLConn.GetTableNames(TableNames);
    Result := TableNames.IndexOf(TableName) >= 0;
  finally
    TableNames.Free;
  end;
end;

procedure TTreeDB.InitTable;
begin
  CreateNewNode('Root', '');
  CreateNewNode('Recycler', '');
  CreateNewNode('Reserve', '');
  CreateNewNode('Config', '');
  CreateNewNode('Spare', '');
end;

{ 基本读写 }

function TTreeDB.AddNode(Name, Note: string; ToID: Integer; mode: TAttachMode): Integer;
begin
  // 从储备库中取出一条记录重新使用
  Result := ReuseNode;
  if Result = -1 then
    // 如果储备库为空，则创建一条新记录
    Result := CreateNewNode(Name, Note)
  else
  begin
    // 如果重用记录成功，则填写记录信息
    SetName(Result, Name);
    SetNote(Result, Note);
  end;

  AttachNode(Result, ToID, Mode);
end;

function TTreeDB.ReuseNode: Integer;
var
  Children: TBoundArray;
begin
  Children := GetChildren(ReserveID);

  if Assigned(Children) then
  begin
    // 从储备库中取出最后一个节点重新使用
    Result := Children[High(Children)];
    // 复位节点信息
    SetParent(Result, FreeID);
    SetChildren(Result, nil);
    SetName(Result, '');
    SetNote(Result, '');
    // 将剩下的节点写入数据库
    SetLength(Children, High(Children));
    SetChildren(ReserveID, Children);
  end
  else
    // 储备库中没有记录
    Result := -1;
end;

function TTreeDB.CreateNewNode(Name, Note: string): Integer;
begin
  // 插入一条新记录
  FSQLQuery.SQL.Text :=
    Format('Insert Into %s (%s, %s, %s) Values (:parent, :name, :note)',
      [NoteTable, ParentField, NameField, NoteField]);
  FSQLQuery.Params.ParamByName('parent').AsInteger := FreeID;
  FSQLQuery.Params.ParamByName('name').AsString := Name;
  FSQLQuery.Params.ParamByName('note').AsString := Note;
  FSQLQuery.ExecSQL;

  // 获取最近插入的记录的 ID
  FSQLQuery.SQL.Text := Format('Select last_insert_rowid() From %s', [NoteTable]);
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsInteger;
  FSQLQuery.Close;

  DataChanged(True);
end;

procedure TTreeDB.DelNode(ID: Integer);
var
  Buf, OldChildren: TBoundArray;
  Count: integer;

  // 用于递归的子函数
  procedure DoDelNode(ID: Integer);
  var
    Child: Integer;
  begin
    if Count > High(Buf) then
      SetLength(Buf, Length(Buf) * 2);
    Buf[Count] := ID;
    Inc(Count);
    for Child in GetChildren(ID) do
      DoDelNode(Child);
  end;

begin
  DetachNode(ID);

  // 收集将要被删除的节点的 ID
  SetLength(Buf, 8);
  Count := 0;
  DoDelNode(ID);

  // 获取储备库中的节点，然后与要删除的节点合并
  OldChildren := GetChildren(ReserveID);
  if Length(OldChildren) > 0 then
  begin
    // 新删除的节点放在子节点列表的前面，这样可以保证最新删除的节点最后被重用，
    // 即最新删除的节点尽可能长时间的留在数据中，便于需要的时候进行数据恢复
    SetLength(Buf, Count + Length(OldChildren));
    Move(OldChildren[0], Buf[Count], Length(OldChildren) * Sizeof(Integer));
  end
  else
    SetLength(Buf, Count);

  SetChildren(ReserveID, Buf);
end;

procedure TTreeDB.RecycleNode(ID: integer);
begin
  MoveNode(ID, RecyclerID, naAddChild);
end;

procedure TTreeDB.EmptyRecycler;
var
  Child: integer;
begin
  for Child in GetChildren(RecyclerID) do
    DelNode(Child);
end;

procedure TTreeDB.MoveNode(FromID, ToID: integer; Mode: TAttachMode);
begin
  DetachNode(FromID);
  AttachNode(FromID, ToID, Mode);
end;

function InsertIntElem(Index: integer; Data: integer; IntArr: TBoundArray; Mode: TAttachMode): TBoundArray;
var
  i: integer;
begin
  Result := nil;

  if Assigned(IntArr) and not (Index in [Low(IntArr)..High(IntArr)]) then
    Exit;

  SetLength(IntArr, Length(IntArr) + 1);

  case mode of
    naInsert:
    begin
      for i := High(IntArr) downto index + 1 do
        IntArr[i] := IntArr[i - 1];
      IntArr[index] := Data;
    end;

    naInsertBehind:
    begin
      for i := High(IntArr) downto index + 2 do
        IntArr[i] := IntArr[i - 1];
      IntArr[index + 1] := Data;
    end;

    naAddFirst, naAddChildFirst:
    begin
      for i := High(IntArr) downto 1 do
        IntArr[i] := IntArr[i - 1];
      IntArr[0] := Data;
    end;

    naAdd, naAddChild:
      IntArr[High(IntArr)] := Data;
  end;

  Result := IntArr;
end;

function RemoveIntElem(Data: integer; IntArr: TBoundArray): TBoundArray;
var
  Index, i: integer;
begin
  Result := nil;

  if not Assigned(IntArr) then
    Exit;

  for Index := 0 to High(IntArr) do
  begin
    if IntArr[Index] = Data then
    begin
      for i := Index to High(IntArr) - 1 do
        IntArr[i] := IntArr[i + 1];
      SetLength(IntArr, Length(IntArr) - 1);

      Break;
    end;
  end;

  Result := IntArr;
end;

procedure TTreeDB.DetachNode(ID: integer);
var
  ParentID: integer;
  Children: TBoundArray;
begin
  // 从父节点中删除自身
  ParentID := GetParent(ID);
  Children := GetChildren(ParentID);
  Children := RemoveIntElem(ID, Children);
  SetChildren(ParentID, Children);

  // 设置自身为自由节点
  SetParent(ID, FreeID);
end;

procedure TTreeDB.AttachNode(ID: integer; ToID: integer; mode: TAttachMode);
var
  Index: integer;
  ParentID: integer;
  Children: TBoundArray;
begin
  Index := 0;
  ParentID := 0;
  Children := nil;

  // 不能停靠在根节点的前后
  if ToID in [RootID, RecyclerID, ReserveID] then
  begin
    // 如果停靠目标是根节点，则停靠模式将自动转换为子节点模式
    case mode of
      naAddFirst, naInsert: mode := naAddChildFirst;
      naAdd, naInsertBehind: mode := naAddChild;
    end;
  end;

  case mode of
    naInsert, naInsertBehind:
    begin
      ParentID := GetParent(ToID);
      Children := GetChildren(ParentID);
      // 获取目标节点的序号
      for Index := 0 to High(Children) do
        if Children[Index] = ToID then
          Break;
    end;

    naAdd, naAddFirst:
    begin
      ParentID := GetParent(ToID);
      Children := GetChildren(ParentID);
    end;

    naAddChild, naAddChildFirst:
    begin
      ParentID := ToID;
      Children := GetChildren(ParentID);
    end;
  end;

  // 为自由节点设置父节点
  SetParent(ID, ParentID);

  // 将自由节点添加到父节点中
  Children := InsertIntElem(Index, ID, Children, mode);
  SetChildren(ParentID, Children);
end;

procedure TTreeDB.UpDownNode(ID: integer; Up: boolean);
var
  ParentID: integer;
  Siblings: TBoundArray;
  Index: integer;
  i: integer;
begin
  ParentID := GetParent(ID);
  Siblings := GetChildren(ParentID);

  if Length(Siblings) = 1 then Exit;

  for Index := 0 to High(Siblings) do
    if Siblings[Index] = ID then
      Break;

  if Up then begin
    if Index = 0 then begin
      // 首节点前移，将其移动到最后
      for i := 0 to High(Siblings) - 1 do
        Siblings[i] := Siblings[i + 1];
      Siblings[High(Siblings)] := ID;
    end else begin
      // 非首节点前移，与前一个节点交换位置
      Siblings[Index] := Siblings[Index - 1];
      Siblings[Index - 1] := ID;
    end;
  end else begin
    if Index = High(Siblings) then begin
      // 尾节点后移，将其移动到最前
      for i := High(Siblings) downto 1 do
        Siblings[i] := Siblings[i - 1];
      Siblings[0] := ID;
    end else begin
      // 非尾节点后移，与后一个节点交换位置
      Siblings[Index] := Siblings[Index + 1];
      Siblings[Index + 1] := ID;
    end;
  end;

  SetChildren(ParentID, Siblings);
end;

function TTreeDB.CopyNode(FromID, ToID: integer; Mode: TAttachMode): integer;
var
  Child: integer;
begin
  Result := AddNode(GetName(FromID), GetNote(FromID), ToID, Mode);

  for Child in GetChildren(FromID) do
    CopyNode(Child, Result, naAddChild);
end;

function TTreeDB.GetParent(ID: integer): integer;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [ParentField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsInteger;
  FSQLQuery.Close;
end;

procedure TTreeDB.SetParent(ID: integer; ParentID: integer);
begin
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:parent Where rowid=:rowid', [NoteTable, ParentField]);
  FSQLQuery.Params.ParamByName('parent').AsInteger := ParentID;
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;

  DataChanged(True);
end;

// 获取数组的长度字段（隐藏字段）的地址
function GetArrHigh(P: Pointer): PDynArrayIndex; inline;
begin
  Result := P - SizeOf(IntPtr);  // See TDynArray in dynarr.inc
end;

// 将整数数组转换为字节数组
function IntsToBytes(const Ints: TBoundArray): TBytes;
begin
  if Assigned(Ints) then
    GetArrHigh(@Ints[0])^ := Length(Ints) * SizeOf(Ints[0]) - 1;
  Result := TBytes(Ints);
end;

// 将字节数组转换为整数数组
function BytesToInts(const Bytes: TBytes): TBoundArray;
begin
  if Assigned(Bytes) then
    GetArrHigh(@Bytes[0])^ := Length(Bytes) div SizeOf(TBoundArray[0]) - 1;
  Result := TBoundArray(Bytes);
end;

function TTreeDB.GetChildren(ID: integer): TBoundArray;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [ChildrenField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := BytesToInts(FSQLQuery.Fields[0].AsBytes);
  FSQLQuery.Close;
end;

procedure TTreeDB.SetChildren(ID: integer; Children: TBoundArray);
begin
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:children Where rowid=:rowid', [NoteTable, ChildrenField]);
  FSQLQuery.Params.ParamByName('children').AsBytes := IntsToBytes(Children);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;

  DataChanged(True);
end;

procedure TTreeDB.DataChanged(AChanged: boolean);
begin
  FChanged := AChanged;
end;

procedure TTreeDB.ActiveChanged(AActive: boolean);
begin
  FActive := AActive;

  if Assigned(FActiveChanged) then
    FActiveChanged(Self);
end;

function TTreeDB.GetName(ID: integer): string;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [NameField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsString;
  FSQLQuery.Close;
end;

procedure TTreeDB.SetName(ID: integer; Name: string);
begin
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:name Where rowid=:rowid', [NoteTable, NameField]);
  FSQLQuery.Params.ParamByName('name').AsString := Name;
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;

  DataChanged(True);
end;

function TTreeDB.GetNote(ID: integer): string;
begin
  FSQLQuery.SQL.Text := Format('Select %s From %s where rowid=:rowid', [NoteField, NoteTable]);
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.Open;
  Result := FSQLQuery.Fields[0].AsString;
  FSQLQuery.Close;
end;

procedure TTreeDB.SetNote(ID: integer; Note: string);
begin
  FSQLQuery.SQL.Text := Format('Update %s Set %s=:note Where rowid=:rowid', [NoteTable, NoteField]);
  FSQLQuery.Params.ParamByName('note').AsString := Note;
  FSQLQuery.Params.ParamByName('rowid').AsInteger := ID;
  FSQLQuery.ExecSQL;

  DataChanged(True);
end;

{ 导入和导出 }

// FileName  ：要导入的文件
// IncludeExt：导入时是否包含文件的扩展名
// ToID      ：导入的目标节点
// Mode      ：导入后的停靠模式
function TTreeDB.ImportFile(FileName: string; IncludeExt: boolean; ToID: integer; mode: TAttachMode): integer;
var
  Name, Note: string;
begin
  Result := ToID;

  if not FileExists(FileName) then
    Exit;

  Name := ExtractFileName(FileName);
  if not IncludeExt then
    Name := ChangeFileExt(Name, '');

  Note := ReadFile(FileName);

  Result := AddNode(Name, Note, ToID, Mode);
end;

// DirName    ：要导入的目录名（入口目录）
// IncludeRoot：是否将入口目录也作为一个节点导入
// IncludeExt ：导入时是否包含文件的扩展名
// ToID       ：导入的目标节点
// Mode       ：导入后的停靠模式
function TTreeDB.ImportDir(FromDir: string; IncludeRoot: boolean; IncludeExt: boolean;
  ToID: integer; mode: TAttachMode): integer;

  // 用于递归的子函数
  procedure DoImportDir(FromDir: string; ToID: integer; mode: TAttachMode);
  var
    i: integer;
    FullPath: string;
    Name, Note: string;
    NewID: integer;
    FindRst: TStringList;
  begin
    FindRst := TStringList.Create;
    try
      FindRst.Clear;
      FindFiles(FromDir, FindRst);
      FindRst.Sort;

      for i := 0 to FindRst.Count - 1 do begin
        Name := FindRst[i];
        FullPath := ConcatPaths([FromDir, Name]);

        if DirectoryExists(FullPath) then begin
          NewID := AddNode(Name, '', ToID, Mode);
          DoImportDir(FullPath, NewID, naAddChild);
        end else begin
          if not IncludeExt then
            Name := ChangeFileExt(Name, '');
          Note := ReadFile(FullPath);

          if (FindRst[i] = DirNode) and not (ToID in [RootID, RecyclerID, ReserveID]) then
            SetNote(ToID, Note)
          else
            NewID := AddNode(Name, Note, ToID, Mode);
        end;

        // 第一个节点使用参数中的停靠模式，其它节点则停靠在前一个节点的后面
        if Mode in [naAddFirst, naAddChildFirst, naInsertBehind] then begin
          ToID := NewID;
          Mode := naInsertBehind;
        end;
      end;
    finally
      FindRst.Free;
    end;
  end;

begin
  Result := ToID;

  if not DirectoryExists(FromDir) then
    Exit;

  if IncludeRoot then
  begin
    // 如果包含入口目录，则将入口目录作为一个节点添加到树中
    FromDir := ExcludeTrailingPathDelimiter(FromDir);
    Result := AddNode(ExtractFileName(FromDir), '', ToID, Mode);

    Mode := naAddChild;
  end
  else
    Result := ToID;

  DoImportDir(FromDir, Result, Mode);
end;

// FromDBFile：要导入的数据库文件
// FromID    ：要导入的入口节点（将导入该节点及其所有子节点）
// ToID      ：要导入的目标节点
// mode      ：导入后的停靠模式
function TTreeDB.ImportDB(FromDBFile: string; FromID, ToID: integer; mode: TAttachMode): integer;
var
  Child: integer;
  TreeDB: TTreeDB;

  // 用于递归的子函数
  procedure DoImportDB(FromID, ToID: integer; mode: TAttachMode);
  var
    Child, NewID: integer;
  begin
    // 将数据从源数据库中拷贝到新数据库中
    NewID := AddNode(TreeDB.GetName(FromID), TreeDB.GetNote(FromID), ToID, Mode);

    for Child in TreeDB.GetChildren(FromID) do
      DoImportDB(Child, NewID, naAddChild);

    // 第一个节点使用参数中的停靠模式，其它节点则停靠在前一个节点的后面
    if Mode in [naAddFirst, naAddChildFirst, naInsertBehind] then
    begin
      ToID := NewID;
      Mode := naInsertBehind;
    end;
  end;

begin
  TreeDB := TTreeDB.Create;
  try
    TreeDB.OpenDB(FromDBFile);
    if not TreeDB.Active then Exit;

    Result := ToID;

    if FromID in [RootID, RecyclerID] then
      // 如果入口节点是根节点，则将其所有子节点导入
      for Child in TreeDB.GetChildren(FromID) do
        DoImportDB(Child, ToID, Mode)
    else
    begin
      // 如果入口节点不是根节点，则将该节点导入
      Result := AddNode(TreeDB.GetName(FromID), TreeDB.GetNote(FromID), ToID, Mode);
      for Child in TreeDB.GetChildren(FromID) do
        DoImportDB(Child, Result, naAddChild);
    end;

    TreeDB.CloseDB(True);
  finally
    TreeDB.Free;
  end;
end;

// ID   ：要导出的节点
// ToDir：存放导出结果的目录
// Ext  ：导出到文本文件将要添加的扩展名（可以为空）
// Depth： 要导出的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
procedure TTreeDB.ExportToDir(ID: integer; ToDir: string; Ext: string; Depth: integer);
var
  Child: integer;

  // 用于递归的子函数
  procedure DoExportTree(ID: integer; ToDir: string; Depth: integer);
  var
    Children: TBoundArray;
    Name, Note: string;
    i: integer;
  begin
    Dec(Depth);

    Name := FixFileName(GetName(ID));
    Note := GetNote(ID);
    Children := GetChildren(ID);

    // 最后一层深度的节点将被导出为文件，无论其是否含有子节点
    if Assigned(Children) and (Depth <> 0) then
    begin
      // 将节点导出为目录
      ToDir := GetNotExistsPath(ToDir, Name, '', 3);
      ForceDirectories(ToDir);

      // 保存节点的内容到文本文件
      if Note <> '' then
        WriteFile(ConcatPaths([ToDir, DirNode + Ext]), Note);

      for i := 0 to High(Children) do
        DoExportTree(Children[i], ToDir, Depth);
    end
    else
    begin
      // 将节点导出为文本文件
      WriteFile(GetNotExistsPath(ToDir, Name, Ext, 3), Note);
    end;
  end;

begin
  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoExportTree(Child, ToDir, Depth)
  else
    DoExportTree(ID, ToDir, Depth);
end;

// ID    : 要导出的节点
// ToFile: 存放导出结果的文本文件
// Depth : 要导出的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
procedure TTreeDB.ExportToFile(ID: integer; ToFile, Splitter: string; Depth: integer);
var
  Buf: TStringStream;
  Child: integer;

  // 用于递归的子函数
  procedure DoExportTreeToFile(ID: integer; Depth: integer);
  var
    Name, Note: string;
    Child: integer;
  begin
    Dec(Depth);

    Name := GetName(ID);
    Note := GetNote(ID);

    // 将节点名称写入缓存中
    Buf.WriteString(Name + LineEnding + LineEnding);

    // 将节点内容写入缓存中
    if Note <> '' then
      Buf.WriteString(Note.Trim + LineEnding + LineEnding);

    // 将分隔符写入缓存中
    if Splitter <> '' then
      Buf.WriteString(Splitter + LineEnding + LineEnding);

    if Depth = 0 then Exit;
    for Child in GetChildren(ID) do
      DoExportTreeToFile(Child, Depth);
  end;

begin
  Buf := TStringStream.Create('');
  try

    if ID in [RootID, RecyclerID] then
      for Child in GetChildren(ID) do
        DoExportTreeToFile(Child, Depth)
    else
      DoExportTreeToFile(ID, Depth);

    WriteFile(ToFile, Buf.DataString);

  finally
    Buf.Free;
  end;
end;

// ID      : 要导出的节点
// ToID    : 目标数据库中的目标节点
// ToDBFile: 要导出的目标数据库（如果文件不存在会自动创建，如果文件已经存在，
//           则不会覆盖原文件的数据，新的数据将追加到目标数据库的尾部）
// Depth   : 要导出的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
function TTreeDB.ExportToDB(ID, ToID: integer; ToDBFile: string; Depth: integer): boolean;
var
  Child: integer;
  TreeDB: TTreeDB;

  // 用于递归的子函数
  procedure DoExportTreeToDB(ID, ToID: integer; Depth: integer);
  var
    Child, TargetNewID: integer;
  begin
    Dec(Depth);

    TargetNewID := TreeDB.AddNode(GetName(ID), GetNote(ID), ToID, naAddChild);

    if Depth = 0 then Exit;
    for Child in GetChildren(ID) do
    begin
      DoExportTreeToDB(Child, TargetNewID, Depth);
    end;
  end;

begin
  TreeDB := TTreeDB.Create;
  try
    Result := TreeDB.OpenDB(ToDBFile);
    if not Result then Exit;

    if ID in [RootID, RecyclerID] then
      for Child in GetChildren(ID) do
        DoExportTreeToDB(Child, ToID, Depth)
    else
      DoExportTreeToDB(ID, ToID, Depth);

    TreeDB.CloseDB(True);
  finally
    TreeDB.Free;
  end;
end;

// 普通搜索
// ID         ：要在其中搜索内容的节点
// ASearchText：要搜索的字符串
// IncludeName：是否在节点名称中搜索
// IncludeNote：是否在节点内容中搜索
// Depth      ：要搜索的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
// Rst        ：用来存放搜索结果的变量
procedure TTreeDB.Search(ID: integer; ASearchText: string; IncludeName, IncludeNote: boolean;
  Depth: integer; IgnoreCase: boolean; var Rst: TSearchResult);
var
  Child: integer;

  // 用于递归的子函数
  procedure DoSearch(ID: integer; Depth: integer);
  var
    Child: integer;
  begin
    Dec(Depth);

    // 在节点名称中搜索
    if IncludeName and (UTF8Search(ID, GetName(ID), ASearchText, IgnoreCase, nil, 1, 1) > 0) then
      Rst.Add(ID, 0, 0);

    // 在节点内容中搜索
    if IncludeNote then
      UTF8Search(ID, GetNote(ID), ASearchText, IgnoreCase, Rst);

    if Depth = 0 then Exit;
    for Child in GetChildren(ID) do
      DoSearch(Child, Depth);
  end;

begin
  if ASearchText = '' then Exit;

  ASearchText := UnEscape(ASearchText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoSearch(Child, Depth)
  else
    DoSearch(ID, Depth);
end;

// 普通替换
// ID          ：要在其中搜索内容的节点
// ASearchText ：要搜索的字符串
// AReplaceText：用来替换的字符串
// IncludeName ：是否在节点名称中搜索
// IncludeNote ：是否在节点内容中搜索
// Depth       ：要搜索的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
// Rst         ：用来存放替换结果的变量
procedure TTreeDB.Replace(ID: integer; ASearchText, AReplaceText: string; IncludeName, IncludeNote: boolean;
  Depth: integer; IgnoreCase: boolean; var Rst: TSearchResult);
var
  Child: integer;

  // 用于递归的子函数
  procedure DoReplace(ID: integer; Depth: integer);
  var
    Child: integer;
    NewName, NewNote: string;
    Count: Integer;
  begin
    Dec(Depth);

    if IncludeName then begin
      Count := Rst.Count;
      NewName := UTF8Replace(ID, GetName(ID), ASearchText, AReplaceText, IgnoreCase, Rst);
      if Count < Rst.Count then
        SetName(ID, NewName);
    end;

    if IncludeNote then begin
      Count := Rst.Count;
      NewNote := UTF8Replace(ID, GetNote(ID), ASearchText, AReplaceText, IgnoreCase, Rst);
      if Count < Rst.Count then
        SetNote(ID, NewNote);
    end;

    if Depth = 0 then Exit;
    for Child in GetChildren(ID) do
      DoReplace(Child, Depth);
  end;

begin
  if ASearchText = '' then Exit;

  ASearchText := UnEscape(ASearchText);
  AReplaceText := UnEscape(AReplaceText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoReplace(Child, Depth)
  else
    DoReplace(ID, Depth);
end;

// 正则表达式搜索
// ID         ：要在其中搜索内容的节点
// ASearchText：要搜索的字符串
// IncludeName：是否在节点名称中搜索
// IncludeNote：是否在节点内容中搜索
// Depth      ：要搜索的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
// Rst        ：用来存放搜索结果的变量
procedure TTreeDB.RegSearch(ID: integer; ASearchText: string; IncludeName, IncludeNote: boolean;
  Depth: integer; var Rst: TSearchResult);
var
  Child: integer;
  Expr: TRegExpr;

  // 用于递归的子函数
  procedure DoSearch(ID: integer; Depth: integer);
  var
    Child: integer;
    Name, Note: string;
    Found: boolean;
  begin
    Dec(Depth);

    if IncludeName then begin
      Name := GetName(ID);
      if Expr.Exec(Name) then
        Rst.Add(ID, 0, 0);
    end;

    if IncludeNote then begin
      Note := GetNote(ID);
      Found := Expr.Exec(Note);  // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符
      while Found do begin
        Rst.Add(ID, Expr.MatchPos[0], Expr.MatchLen[0]);
        Found := Expr.ExecNext;  // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符
      end;
    end;

    if Depth = 0 then Exit;
    for Child in GetChildren(ID) do
      DoSearch(Child, Depth);
  end;

begin
  if ASearchText = '' then Exit;

  Expr := TRegExpr.Create(ASearchText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoSearch(Child, Depth)
  else
    DoSearch(ID, Depth);

  Expr.Free;
end;

// 普通替换
// ID          ：要在其中搜索内容的节点
// ASearchText ：要搜索的字符串
// AReplaceText：用来替换的字符串
// IncludeName ：是否在节点名称中搜索
// IncludeNote ：是否在节点内容中搜索
// Depth       ：要搜索的深度，1 表示只导出当前节点，AllDepth 表示导出所有节点，其它值表示指定深度
// Rst         ：用来存放替换结果的变量
procedure TTreeDB.RegReplace(ID: integer; ASearchText, AReplaceText: string; IncludeName, IncludeNote: boolean;
  Depth: integer; var Rst: TSearchResult);
var
  Child: integer;
  Expr: TRegExpr;
  Replaced: boolean;

  function OneReplace(ID: integer; AText: string): string;
  var
    Found: boolean;
    Start, ReplacedStart, ReplacedLength: integer;
    ReplacedOne, MiddleString: string;
  begin
    Result := '';

    Start := 1;

    ReplacedStart := 1;
    ReplacedLength := 0;

    Found := Expr.Exec(AText);  // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符

    if not Found then
    begin
      Replaced := False;
      Exit;
    end;

    while Found do
    begin
      // 获取不匹配的字符串
      MiddleString := UTF8Copy(AText, Start, Expr.MatchPos[0] - Start);

      // 获取单个替换结果
      ReplacedOne := ReplaceRegExpr(ASearchText, Expr.Match[0], AReplaceText, True);
      ReplacedLength := UTF8Length(ReplacedOne);

      ReplacedStart := ReplacedStart + UTF8Length(MiddleString);
      Rst.Add(ID, ReplacedStart, ReplacedLength);

      Result := Result + MiddleString + ReplacedOne;

      Start := Expr.MatchPos[0] + Expr.MatchLen[0];
      ReplacedStart := ReplacedStart + ReplacedLength;

      Found := Expr.ExecNext;
    end;
    MiddleString := UTF8Copy(AText, Start, MaxInt);
    Result := Result + MiddleString;

    Replaced := True;
  end;

  // 用于递归的子函数
  procedure DoReplace(ID: integer; Depth: integer);
  var
    Child: integer;
    Name, Note: string;
  begin
    Dec(Depth);

    if IncludeName then
    begin
      Name := GetName(ID);
      if Expr.Exec(Name) then
      begin
        SetName(ID, Expr.Replace(Name, AReplaceText, True));
        Rst.Add(ID, 0, 0);
      end;
    end;

    if IncludeNote then begin
      Note := OneReplace(ID, GetNote(ID));
      if Replaced then
        SetNote(ID, Note);
    end;

    if Depth = 0 then Exit;
    for Child in GetChildren(ID) do
      DoReplace(Child, Depth);
  end;

begin
  if ASearchText = '' then Exit;

  Expr := TRegExpr.Create(ASearchText);

  if ID in [RootID, RecyclerID] then
    for Child in GetChildren(ID) do
      DoReplace(Child, Depth)
  else
    DoReplace(ID, Depth);

  Expr.Free;
end;

initialization

  // 请将 SQLite3 的动态链接库文件放到 lib 目录中
{$ifdef MSWINDOWS}
  LibFile := ConcatPaths([ExtractFileDir(ParamStrUTF8(0)), 'lib', 'sqlite3.dll']);
{$else}
  LibFile := ConcatPaths([ExtractFileDir(ParamStrUTF8(0)), 'lib', 'libsqlite3.so']);
{$endif}
  // 为 SQLite3 控件指定一个动态链接库文件，如果未指定，则使用系统默认的动态链接库文件
  if FileExists(LibFile) then
    SQLiteDefaultLibrary := LibFile;
end.

