{
  注意：这里的代码只针对 Windows 和 GTK2，其它平台未测试。

  编辑框内容的变化无非就是增加内容和减少内容，既不增加也不减少那就算不上变化。
  即使是选中了部分文本后再执行粘贴操作，也是分两步变化的，先删除被选中的内容，
  再加入剪贴板中的内容（Windows 是这么干的），或者先加入剪贴板中的内容，再删除
  被选中的内容（GTK2 是这么干的）

  所以只要把这些变化记录下来，就可以随时撤销和恢复了。最佳的记录时机就是在编辑
  框的内容改变前后进行记录，但是编辑框只提供了 OnChange 事件，也就是内容改变之
  后才触发的事件，所以我需要把改变之前的内容提前保存下来，以便在内容改变之后进
  行对比。

  正常情况下（GTK2 属于非正常情况）：
  1、增加内容之后，光标应该在新增加的内容之后，而且新文本内容比旧文本内容长，
  所以很容易算出增加了什么内容，从哪里开始增加的。
  2、删除内容之后，光标应该在被删除的内容之前，而且新文本内容比旧文本内容短，
  所以也容易算出减少了什么内容容，从哪里开始减少的。

  这就是本单元实现历史记录的基本思路，所需要的条件有三个：
  1、变化前的文本内容
  2、变化后的文本内容
  3、在哪里发生了改变（即正确的 SelStart）

  满足下面两个条件的 SelStart 就是正确的 SelStart：
  1、向编辑框中添加文本后，SelStart 应该在新增加的文本之后。
  2、从编辑框中删除文本后，SelStart 应该在被删除的文本之前。

  但是 GTK2 的这种“先加入剪贴板中的内容，再删除选中的内容”的做法给计算历史记录
  带来了麻烦，因为无法在 OnChange 事件中得到正确的 SelStart，在 OnChange 事件
  中，GTK2 的 SelStart 要用来记录被选中的内容，所以无法提供新增加的内容的具体
  位置，这就是一个盲点，需要比较变化前后文本内容才能知道增内容的正确位置。这种
  情况的一个特点就是编辑框内容变化之后，SelLength 不为 0（只有 GTK2 会出现这种
  情况，这可以作为一个着手点）。

  GTK2 还有一个致命的问题，就是拖放文本时无法提供正确的放置位置，也就是拖放文
  本前和拖放文本后的 SelStart 和 SelLength 是相同的，你不知道用户把文本拖放到
  了什么位置，这种情况简直就是一片漆黑，你只知道文本框的内容变化了，其它什么都
  不知道。更要命的是，拖放操作也是先把内容增加的目标位置，然后再删除之前选择的
  内容，这和前面说过的情况相同，因此你无法区分用户是执行了拖放操作还是执行了粘
  贴操作。

  最后，GTK2 的拖放操作还有一个 BUG，如果你快速反复的选择文本并拖放文本，你会
  发现你拖放的文本会莫名其妙的消失，也就是丢失数据。

  我很想禁用 GTK2 的编辑框文本拖放功能，但是我不知道怎么实现，如果有谁知道，恳
  请告诉我，非常感谢！

  本代码最后很艰难的实现了 Undo Redo 功能，支持任何拖拽操作。

  E-Mail: tomitomy@163.com
}

unit uhistory;

{$mode objfpc}{$H+}
{.$define DEBUG}

interface

uses
  Classes, SysUtils, Variants, Controls, StdCtrls, Dialogs, Forms, Clipbrd;

type

  THalfRecord = (hrFull, hrFirstHalf, hrSecondHalf);

  PHistoryRecord = ^THistoryRecord;

  { THistoryRecord 用来存放单个操作的历史记录 }

  THistoryRecord = record
    PrevSelStart  : integer;      // OnChange 之前的 SelStart
    PrevSelLength : integer;      // OnChange 之前的 SelLength
    PrevSelText   : string;       // OnChange 之前的 SelText
    SelStart      : integer;      // OnChange 之后的 SelStart
    SelLength     : integer;      // OnChange 之后的 SelLength
    SelText       : string;       // OnChange 之后的 SelText
    HalfRecord    : THalfRecord;  // 标记当前记录是否只是半个过程，用于拖放操作
  end;

  { THistory 用来存取多个操作的历史记录 }

  THistory = class
  private
    FList  : TList;        // 历史记录列表
    FIndex : integer;      // 当前历史记录索引
    FSize  : integer;      // 历史记录总大小
    // 清空并复位历史记录数据
    procedure Reset;
    // 限制历史记录总大小
    procedure Limit(MaxSize, MinCount: integer);
    function  GetCount: Integer;
    function  GetRecordSize(Idx: integer): integer;
    function  Get(Idx: Integer): PHistoryRecord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRecord(PHR: PHistoryRecord);
    procedure AddRecord(
      APrevSelStart  : integer;
      APrevSelLength : integer;
      APrevSelText   : string;
      ASelStart      : integer;
      ASelLength     : integer;
      ASelText       : string);
    property  Index : Integer read FIndex;
    property  Size  : integer read FSize;
    property  Count : integer read GetCount;
    property  List  : TList   read FList;
    property  Items[Idx: Integer]: PHistoryRecord read Get; default;
  end;

  { THistoryManager }

  THistoryManager = class
  private
    FEdit               : TCustomEdit;   // 用来实现历史操作的编辑框控件
    FHistory            : THistory;      // 历史记录数据
    FMaxSize            : integer;       // 历史记录最大容量限制
    FMinCount           : integer;       // 历史记录最小数量保护
    FTotalSize          : integer;       // 用于多重历史记录（多个 THistory）
    FEditing            : boolean;       // 标记编辑框是否处于编辑状态
    FEnabled            : boolean;       // 启用或禁用对编辑框的监视
    FHalfRecord         : boolean;       // 用于处理拖拽操作的标记
    FPrevText           : string;        // OnChagne 之前的 Text
    FPrevSelStart       : integer;       // OnChagne 之前的 SelStart
    FOldApplicationIdle : TIdleEvent;    // 修改 OnApplicationIdle 用来获取 OnChagne 之前的 SelStart
    FOldEditChange      : TNotifyEvent;  // 修改 OnChange 用来监视编辑框内容的变化
    FOnHistoryChanged   : TNotifyEvent;  // 提供 OnHistoryChanged 给外部代码用于监视历史记录的变化

    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure EditChange(Sender: TObject);
    // 仅使用 PrevText、Text、SelStart 来计算历史记录数据（可以在 Windows 中正常工作）
    procedure CalcRecord(PHR: PHistoryRecord);
    // 针对 GTK2 的特殊处理
    procedure CalcRecordGTK2Memo(PHR: PHistoryRecord);
    // 仅使用 PrevText、Text 来计算历史记录数据（速度最慢）
    procedure CalcRecordHard(PHR: PHistoryRecord);

    function  GetIndex     : integer;
    function  GetCount     : integer;
    function  GetSize      : integer;
    function  GetTotalSize : integer;
    procedure SetEnabled(AValue: boolean);
  public
    constructor Create(AEdit: TCustomEdit; AMaxSize: Integer = 32 * 1024; AMinCount: integer = 10);
    destructor Destroy; override;
    function  CanUndo: boolean;
    function  CanRedo: boolean;
    procedure Undo;
    procedure Redo;
    procedure Cut;
    procedure Paste;
    procedure Delete;
    // 创建历史记录数据存取对象
    procedure CreateHistory(AText: string);
    // 销毁历史记录数据存取对象
    procedure DestroyHistory;
    // 切换历史记录数据存取对象
    procedure SwitchHistory(AHistory: THistory; APrevText: string);
    // 简单的添加历史记录，对所有内容进行整体替换
    procedure AddRecordSimply(APrevText: string);
    // 清空并复位历史记录数据
    procedure Reset;
    property  History   : THistory read FHistory;
    property  Index     : integer read GetIndex;
    property  Size      : integer read GetSize;
    property  TotalSize : integer read GetTotalSize;
    property  Count     : integer read GetCount;
    property  MaxSize   : integer read FMaxSize write FMaxSize;
    property  MinCount  : integer read FMinCount write FMinCount;
    property  Enabled   : boolean read FEnabled write SetEnabled;
    property  OnHistoryChanged: TNotifyEvent read FOnHistoryChanged write FOnHistoryChanged;
  end;


implementation

uses
  lazUTF8, ucommon;

{ THistory }

constructor THistory.Create;
begin
  inherited Create;
  FList := TList.Create;
  Reset;
end;

destructor THistory.Destroy;
begin
  Reset;
  FList.Free;
  inherited Destroy;
end;

// 清空并复位历史记录
procedure THistory.Reset;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    dispose(Items[i]);
  FList.Clear;
  FIndex := -1;
  FSize := 0;
end;

// 添加一条历史记录
procedure THistory.AddRecord(PHR: PHistoryRecord);
var
  i: integer;
begin
  // 删除当前索引之后的历史记录
  for i := FList.Count - 1 downto FIndex + 1 do begin
    Dec(FSize, GetRecordSize(i));
    dispose(Items[i]);
    FList.Delete(i);
  end;
  FList.Add(PHR);
  Inc(FIndex);
  Inc(FSize, GetRecordSize(FIndex));
end;

// 添加一条历史记录
procedure THistory.AddRecord(
  APrevSelStart  : integer;
  APrevSelLength : integer;
  APrevSelText   : string;
  ASelStart      : integer;
  ASelLength     : integer;
  ASelText       : string);
var
  PHR: PHistoryRecord;
begin
  PHR := new(PHistoryRecord);
  PHR^.PrevSelStart  := APrevSelStart;
  PHR^.PrevSelLength := APrevSelLength;
  PHR^.PrevSelText   := APrevSelText;
  PHR^.SelStart      := ASelStart;
  PHR^.SelLength     := ASelLength;
  PHR^.SelText       := ASelText;
  AddRecord(PHR);
end;

// 获取指定的历史记录的大小
function THistory.GetRecordSize(Idx: integer): integer;
begin
  Result :=
    Items[Idx]^.PrevSelText.Length +
    Items[Idx]^.SelText.Length +
    Sizeof(THistoryRecord);
end;

// 限制历史记录总大小，但保证历史记录数量不低于 MinCount
procedure THistory.Limit(MaxSize, MinCount: integer);
begin
  while (MaxSize > 0) and (FSize > MaxSize) and (Count > MinCount) do begin
    Dec(FSize, GetRecordSize(0));
    dispose(Items[0]);
    FList.Delete(0);
    Dec(FIndex);
  end;
end;

// 获取历史记录总个数
function THistory.GetCount: Integer;
begin
  Result := FList.Count;
end;

// 获取指定的历史记录
function THistory.Get(Idx: Integer): PHistoryRecord;
begin
  Result := PHistoryRecord(FList[Idx]);
end;

{ THistoryManager }

constructor THistoryManager.Create(AEdit: TCustomEdit; AMaxSize: Integer = 32 * 1024; AMinCount: integer = 10);
begin
  inherited Create;
  FEdit     := AEdit;
  FMaxSize  := AMaxSize;
  FMinCount := AMinCount;
  FPrevText := FEdit.Text;

  FOldApplicationIdle := Application.OnIdle;
  Application.OnIdle := @ApplicationIdle;

  // 历史记录数据由外部代码在需要的时候创建
  FHistory    := nil;
  // 创建了历史记录数据之后才能开始监视
  FEnabled    := False;
  FEditing    := True;
  FHalfRecord := False
end;

destructor THistoryManager.Destroy;
begin
  Application.OnIdle := FOldApplicationIdle;
  // 取消监视
  if FEnabled then
    FEdit.OnChange := FOldEditChange;
  FHistory.Free;
  inherited Destroy;
end;

procedure THistoryManager.Reset;
begin
  FHistory.Reset;
  FPrevText := FEdit.Text;

  if Assigned(FOnHistoryChanged) then
    FOnHistoryChanged(self);
end;

procedure THistoryManager.CreateHistory(AText: string);
begin
  // 旧的 FHistory 由外部代码保管，这里不做处理，只保留其大小。
  if FHistory <> nil then
    Inc(FTotalSize, FHistory.Size);

  FHistory := THistory.Create;
  FPrevText := AText;
end;

procedure THistoryManager.DestroyHistory;
begin
  if FHistory <> nil then
    FreeAndNil(FHistory);

  FPrevText := '';
  // 没有数据可供读写时，要禁用编辑框监视，否则会出现异常。
  Enabled := False;
end;

procedure THistoryManager.SwitchHistory(AHistory: THistory; APrevText: string);
begin
  // 旧的 FHistory 由外部代码保管，这里不做处理，只保留其大小。
  if FHistory <> nil then
    Inc(FTotalSize, FHistory.Size);

  // 去掉之前保存过的大小（FTotalSize 不包括当前历史记录的大小）
  if AHistory <> nil then
    Dec(FTotalSize, AHistory.Size);

  FHistory := AHistory;
  // 历史记录与 PrevText 是相关联的，必须保持一致。
  FPrevText := APrevText;
  // 没有数据可供读写时，要禁用编辑框监视，否则会出现异常。
  if FHistory = nil then Enabled := False;
end;

procedure THistoryManager.AddRecordSimply(APrevText: string);
begin
  FHistory.AddRecord(
    0, UTF8Length(APrevText), APrevText,
    0, UTF8Length(FEdit.Text), FEdit.Text);
end;

// 记录 OnChange 之前的 SelStart
procedure THistoryManager.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  FPrevSelStart := FEdit.SelStart;

  if Assigned(FOldApplicationIdle) then
    FOldApplicationIdle(Sender, Done);
end;

// 分析并存储历史记录
procedure THistoryManager.EditChange(Sender: TObject);
var
  PHR: PHistoryRecord;
begin
  if FEditing then
  begin
    PHR := new(PHistoryRecord);
    PHR^.HalfRecord := hrFull;

    // Pos 的结果是从索引 1 开始计数的
    if Pos('Gtk2', FEdit.WidgetSetClass.ClassName) > 0 then begin
      if FEdit.ClassType = TEdit then
        CalcRecordHard(PHR)
      else
        CalcRecordGTK2Memo(PHR);
    end else
      CalcRecord(PHR);

    FHistory.AddRecord(PHR);
    FHistory.Limit(FMaxSize, FMinCount);
  end;

  // 用于下次文本改变时做比较
  FPrevText := FEdit.Text;

  if Assigned(FOldEditChange) then
    FOldEditChange(Sender);

  if Assigned(FOnHistoryChanged) then
    FOnHistoryChanged(self);
end;

// 这个函数可以在 Windows 中正确处理所有变化情况
procedure THistoryManager.CalcRecord(PHR: PHistoryRecord);
var
  PrevLen, Len, BSelStart: integer;
  SelText : string;
  SelLength: integer;
begin
  PrevLen := Length(FPrevText);
  Len     := Length(FEdit.Text);
  // 转换为字节索引，因为字节索引的操作速度比字符索引快很多。
  // UTF8CharToByteIndex 的参数和结果是从索引 0 开始计数的。
  BSelStart := UTF8CharToByteIndex(PChar(FEdit.Text), Len, FEdit.SelStart);
  // Len > PrevLen 表示编辑框中增加了内容，此时 SelStart 应该在新增加的内容之后。
  // Len < PrevLen 表示编辑框中删除了内容，此时 SelStart 应该在被删除的内容之前。
  // Len = PrevLen 不可能存在
  if Len > PrevLen then begin
    // 计算出增加了什么内容（Substring 的参数是从索引 0 开始计数的）
    SelText := string(FEdit.Text).Substring(BSelStart - (Len - PrevLen), (Len - PrevLen));
    // 计算出增加了多少字符
    SelLength := UTF8Length(SelText);

    PHR^.PrevSelStart := FEdit.SelStart - SelLength;
    PHR^.PrevSelLength := 0;
    PHR^.PrevSelText := '';

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := SelLength;
    PHR^.SelText := SelText;
  end else if Len < PrevLen then begin
    // 计算出删除了什么内容（Substring 的参数是从索引 0 开始计数的）
    SelText := FPrevText.Substring(BSelStart, (PrevLen - Len));
    // 计算出删除了多少字符
    SelLength := UTF8Length(SelText);

    PHR^.PrevSelStart := FEdit.SelStart;
    PHR^.PrevSelLength := SelLength;
    PHR^.PrevSelText := SelText;

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := 0;
    PHR^.SelText := '';
  end else
    ShowMessage('CalcRecord: History Record Error !');

  if FHalfRecord then begin
    PHR^.HalfRecord := hrSecondHalf;
    FHalfRecord := False;
  end;

  {$ifdef DEBUG}
  // 太长的内容会导致输出信息时间过长
  if Length(FPrevText) < 64 then
    writeln('CalcRecord: PrevText: ', FPrevText.Replace(#10, '_'));
  if Length(FEdit.Text) < 64 then
    writeln('CalcRecord: CurrText: ', string(FEdit.Text).Replace(#10, '_'));
  writeln('CalcRecord: AddOrDel: ', SelText.Replace(#10, '_'), ' | ', PHR^.PrevSelStart, ' ', SelLength);
  writeln('------------------------------')
  {$endif}
end;

procedure THistoryManager.CalcRecordGTK2Memo(PHR: PHistoryRecord);
var
  BPrevSelStart, BSelLength: integer;
  BSelStartFix, BSelStartFix2: SizeInt;
  SelStart: integer;
  PrevSelText, SelText: string;
begin
  // 只有 GTK2 会出现“编辑框内容改变之后，SelLength 不为 0”的情况。
  // 这里不考虑“选中文本后再粘贴”的情况，因为粘贴操作可以在 OnChange 之前进行拦
  // 截并处理，以便减轻这里的负担。这里只考虑拖拽操作，拖拽前后 SelStart 始终是
  // 拖拽前的值，但如果是向头拖拽（朝文本开始的方向拖拽），则当文本内容增加后，
  // SelStart 也会相应进行偏移，如果是向尾拖拽，则增加的内容在 SelStart 之后，
  // SelStart 不会进偏移。
  // 通过这个特点，我可以知道文本是向头拖拽还是向尾拖拽，从而从 SelStart 处开始
  // 定向搜索变化前后的不同之处。拖拽操作一般不会拖太远，所以计算起来也比较快。
  // 这个操作依赖于 FPrevSelStart，这样才能知道 SelStart 有没有进行偏移。
  if FEdit.SelLength > 0 then begin
    PHR^.HalfRecord := hrFirstHalf;
    FHalfRecord := True;

    if FEdit.SelStart = FPrevSelStart then
    // SelStart 没有进行偏移，执行的是向尾拖拽，接下来要从 SelStart 处开始向
    // 后比较文本的不同之处，从而获取新增加的内容。
    begin
      // 获取 PrevSelStart 的字节索引。
      // UTF8CharToByteIndex 的参数和结果是从索引 0 开始计数的。
      BPrevSelStart := UTF8CharToByteIndex(PChar(FPrevText), FPrevText.Length, FPrevSelStart);
      // 获取新增内容的字节长度
      BSelLength := Length(FEdit.Text) - FPrevText.Length;
      PrevSelText := FPrevText.Substring(BPrevSelStart, BSelLength);
      // 准备查找的起始位置（索引从 1 开始，跳过已选择的部分）。
      BSelStartFix := BPrevSelStart + BSelLength + 1;
      BSelStartFix2 := BSelStartFix;
      // 向尾查找不同之处（即新内容的插入位置，返回值索引从 1 开始）。
      // 如果没有不同之处，则说明新内容被插入到文本的尾部。
      UTF8DiffBytePos(FEdit.Text, FPrevText, BSelStartFix, BSelStartFix2);
      // 将索引恢复到从 0 开始。BSelStartFix2 和 BSelStartFix 相同，用不上。
      Dec(BSelStartFix);
      // 获取新增加的内容（新增加的内容不一定就是之前选择的内容，比如 a++b++c++，
      // 当把 +b+ 拖到 c+ 之后，就会出现这种情况）。
      // Substring 的参数是从索引 0 开始计数的。
      SelText := string(FEdit.Text).Substring(BSelStartFix, BSelLength);
      while PrevSelText <> SelText do begin
        Dec(BSelStartFix);
        SelText := string(FEdit.Text).Substring(BSelStartFix, BSelLength);
      end;
      // 计算正确的 SelStart。
      SelStart := FPrevSelStart + UTF8Length(PChar(FPrevText) + BPrevSelStart, BSelStartFix - BPrevSelStart);
    end
    else if FEdit.SelStart > FPrevSelStart then
    // SelStart 发生了偏移，执行的是向头拖拽，接下来要从 SelStart 处开始向头比
    // 较文本的不同之处，从而获取新增加的内容。
    begin
      // 获取 PrevSelStart 的字节索引。
      // UTF8CharToByteIndex 的参数和结果是从索引 0 开始计数的。
      BPrevSelStart := UTF8CharToByteIndex(PChar(FPrevText), FPrevText.Length, FPrevSelStart);
      // 获取新增内容的字节长度
      BSelLength := Length(FEdit.Text) - FPrevText.Length;
      PrevSelText := FPrevText.Substring(BPrevSelStart, BSelLength);
      // 准备查找的起始位置（索引从 1 开始，跳过已选择的部分）。
      BSelStartFix := BPrevSelStart + BSelLength + 1; // FEdit.Text 偏移后的 SelStart
      BSelStartFix2 := BPrevSelStart + 1;             // FPrevSelStart
      // 向头查找不同之处（即新内容的插入位置，返回值索引从 1 开始）。
      // 如果没有不同之处，则说明新内容被插入到文本的头部。
      UTF8DiffBytePos(FEdit.Text, FPrevText, BSelStartFix, BSelStartFix2, True);
      // 计算正确的 SelStart
      if BSelStartFix2 = 0 then begin // 新内容被插入到文本的头部
        SelStart := 0;
        SelText := PrevSelText;
      end
      else begin
        // 将索引恢复到从 0 开始。
        Dec(BSelStartFix);
        // 跳过找到的不同的字符
        BSelStartFix := BSelStartFix + UTF8CharacterLength(PChar(FEdit.Text) + BSelStartFix);
        // 获取新增加的内容（新增加的内容不一定就是之前选择的内容，比如 a++b++c++，
        // 当把 +c+ 拖到 b+ 之前，就会出现这种情况）。
        // Substring 的参数是从索引 0 开始计数的。
        if BSelStartFix < BSelLength then BSelStartFix := BSelLength;
        // 将 BSelStartFix 定位到新添加文本的开头位置
        Dec(BSelStartFix, BSelLength);
        // 获取新添加的文本
        SelText := string(FEdit.Text).Substring(BSelStartFix, BSelLength);
        // 修补错误的结果
        while (PrevSelText <> SelText) do begin
          Inc(BSelStartFix);
          SelText := string(FEdit.Text).Substring(BSelStartFix, BSelLength);
        end;
        // 计算正确的 SelStart
        SelStart := FPrevSelStart - UTF8Length(PChar(FPrevText) + BSelStartFix, BPrevSelStart - BSelStartFix);
      end;
    end else begin
      ShowMessage('CalcRecordGTK2Memo: History Record Error !');
      Exit;
    end;

    PHR^.PrevSelStart  := SelStart;
    PHR^.PrevSelLength := 0;
    PHR^.PrevSelText   := '';

    PHR^.SelStart      := PHR^.PrevSelStart;
    PHR^.SelText       := SelText;
    PHR^.SelLength     := UTF8Length(SelText);

    {$ifdef DEBUG}
    // 太长的内容会导致输出信息时间过长
    if Length(FPrevText) < 64 then
      writeln('CalcRecordGTK2Memo: PrevText: ', FPrevText.Replace(#10, '_'));
    if Length(FEdit.Text) < 64 then
      writeln('CalcRecordGTK2Memo: CurrText: ', string(FEdit.Text).Replace(#10, '_'));
    writeln('CalcRecordGTK2Memo: AddOrDel: ', SelText.Replace(#10, '_'), ' | ', SelStart, ' ', PHR^.SelLength);
    writeln('------------------------------')
    {$endif}
  end else if (FEdit.SelStart = FPrevSelStart) and (Length(FEdit.Text) > FPrevText.Length) then
    CalcRecordHard(PHR)
  else if FEdit.SelStart - FPrevSelStart > 1 then // 粘贴的情况已经在之前被拦截了
     CalcRecordHard(PHR)
  else
    CalcRecord(PHR);
end;

procedure THistoryManager.CalcRecordHard(PHR: PHistoryRecord);
var
  PrevLen, Len: integer;
  SelStart, PrevSelStart, SelLength: SizeInt;
  SelText: string;
begin
  Len     := Length(FEdit.Text);
  PrevLen := Length(FPrevText);

  // 向尾查找不同之处（参数和返回值的索引从 1 开始）。
  SelStart     := 1;
  PrevSelStart := 1;
  UTF8DiffBytePos(FEdit.Text, FPrevText, SelStart, PrevSelStart);

  if Len > PrevLen then begin
    // Add content to FEdit
    Dec(SelStart);
    SelText := string(FEdit.Text).Substring(SelStart, (Len - PrevLen));
    SelLength := UTF8Length(SelText);
    SelStart := UTF8Length(PChar(FEdit.Text), SelStart);

    PHR^.PrevSelStart := SelStart;
    PHR^.PrevSelLength := 0;
    PHR^.PrevSelText := '';

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := SelLength;
    PHR^.SelText := SelText;
  end else if Len < PrevLen then begin
    Dec(PrevSelStart);
    SelText := FPrevText.Substring(PrevSelStart, (PrevLen - Len));
    SelLength := UTF8Length(SelText);
    SelStart := UTF8Length(PChar(FPrevText), PrevSelStart);

    PHR^.PrevSelStart := SelStart;
    PHR^.PrevSelLength := SelLength;
    PHR^.PrevSelText := SelText;

    PHR^.SelStart := PHR^.PrevSelStart;
    PHR^.SelLength := 0;
    PHR^.SelText := '';
  end else begin
    ShowMessage('CalcRecordHard: History Record Error !');
    Exit;
  end;
  if (FEdit.SelLength > 0) and (Length(FEdit.Text) > FPrevText.Length) then begin
    PHR^.HalfRecord := hrFirstHalf;
    FHalfRecord := True;
  end else if FHalfRecord then begin
    PHR^.HalfRecord := hrSecondHalf;
    FHalfRecord := False;
  end;

  {$ifdef DEBUG}
  // 太长的内容会导致输出信息时间过长
  if Length(FPrevText) < 64 then
    writeln('CalcRecordHard: PrevText: ', FPrevText.Replace(#10, '_'));
  if Length(FEdit.Text) < 64 then
    writeln('CalcRecordHard: CurrText: ', string(FEdit.Text).Replace(#10, '_'));
  writeln('CalcRecordHard: AddOrDel: ', SelText.Replace(#10, '_'), ' | ', SelStart, ' ', PHR^.SelLength);
  writeln('------------------------------')
  {$endif}
end;

function THistoryManager.CanUndo: boolean;
begin
  Result := (FHistory <> nil) and (FHistory.FIndex >= 0);
end;

function THistoryManager.CanRedo: boolean;
begin
  Result := (FHistory <> nil) and (FHistory.FIndex < FHistory.Count - 1);
end;

procedure THistoryManager.Undo;
var
  Half: THalfRecord;
begin
  if FHistory.FIndex < 0 then Exit;

  FEditing := False;
  with FHistory[FHistory.FIndex]^ do begin
    FEdit.SelStart  := SelStart;
    FEdit.SelLength := SelLength;
    FEdit.SelText   := PrevSelText;
    Half            := HalfRecord;
  end;
  FEditing := True;

  Dec(FHistory.FIndex);

  if Half = hrSecondHalf then Undo;

  if Assigned(FOnHistoryChanged) and (Half <> hrFirstHalf) then
    FOnHistoryChanged(self);
end;

procedure THistoryManager.Redo;
var
  Half: THalfRecord;
begin
  if FHistory.FIndex >= FHistory.Count - 1 then Exit;

  Inc(FHistory.FIndex);

  FEditing := False;
  with FHistory[FHistory.FIndex]^ do begin
    FEdit.SelStart  := PrevSelStart;
    FEdit.SelLength := PrevSelLength;
    FEdit.SelText   := SelText;
    Half            := HalfRecord;
  end;
  FEditing := True;

  if Half = hrFirstHalf then Redo;

  if Assigned(FOnHistoryChanged) and (Half <> hrSecondHalf) then
    FOnHistoryChanged(self);
end;

procedure THistoryManager.Cut;
var
  SelStart, SelLength: integer;
  SelText: string;
begin
  if FEdit.SelLength = 0 then Exit;
  SelStart  := FEdit.SelStart;
  SelLength := FEdit.SelLength;
  SelText   := FEdit.SelText;

  Enabled := False;
  FEdit.CutToClipboard;
  FHistory.AddRecord(SelStart, SelLength, SelText, SelStart, 0, '');
  FPrevText := FEdit.Text;
  Enabled := True;
end;

procedure THistoryManager.Paste;
var
  SelStart, SelLength: integer;
  SelText: string;
  ClipBoardText: string;
begin
  if not Clipboard.HasFormat(CF_TEXT) then Exit;
  SelStart  := FEdit.SelStart;
  SelLength := FEdit.SelLength;
  SelText   := FEdit.SelText;
  ClipBoardText := ToLF(ClipBoard.AsText);;

  Enabled := False;
  FEdit.SelText := ClipBoardText;
  FHistory.AddRecord(SelStart, SelLength, SelText, SelStart, UTF8Length(ClipBoardText), ClipBoardText);
  FPrevText := FEdit.Text;
  Enabled := True;
end;

procedure THistoryManager.Delete;
var
  SelStart, SelLength: integer;
  SelText: string;
begin
  if FEdit.SelLength = 0 then Exit;
  SelStart  := FEdit.SelStart;
  SelLength := FEdit.SelLength;
  SelText   := FEdit.SelText;

  Enabled := False;
  FEdit.ClearSelection;
  FHistory.AddRecord(SelStart, SelLength, SelText, SelStart, 0, '');
  FPrevText := FEdit.Text;
  Enabled := True;
end;

function THistoryManager.GetIndex: integer;
begin
  if FHistory <> nil then
    Result := FHistory.FIndex + 1
  else
    Result := -1;
end;

function THistoryManager.GetCount: integer;
begin
  if FHistory <> nil then
    Result := FHistory.Count
  else
    Result := 0;
end;

function THistoryManager.GetSize: integer;
begin
  if FHistory <> nil then
    Result := FHistory.FSize
  else
    Result := 0;
end;

function THistoryManager.GetTotalSize: integer;
begin
  Result := FTotalSize + Size + FPrevText.Length;
end;

procedure THistoryManager.SetEnabled(AValue: boolean);
begin
  if FEnabled = AValue then Exit;

  FEnabled := AValue;

  if FEnabled then begin
    FOldEditChange := FEdit.OnChange;
    FEdit.OnChange := @EditChange
  end
  else
    FEdit.OnChange := FOldEditChange;
end;

end.

