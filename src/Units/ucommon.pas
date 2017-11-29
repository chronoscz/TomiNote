unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, StrUtils;

type

  PSearchRecord = ^TSearchRecord;

  TSearchRecord = record
    ID      : Integer;  // Node ID
    UStart  : SizeInt;  // UTF8 Start
    ULength : SizeInt;  // UTF8 Length
  end;

  TSearchResult = class
  private
    FList: TList;
    function  Get(Index: integer): PSearchRecord;
    procedure Put(Index: integer; Item: PSearchRecord);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(PSR: PSearchRecord);
    procedure Add(ID: integer; UStart, ULength: SizeInt);
    procedure Del(Index: integer);
    procedure Clear;
    function  Count: integer;
    property  Items[Index: integer]: PSearchRecord read Get write Put; default;
  end;

function IsKeyDown(AKey: integer): boolean; inline;

function StartsWith(S, SubStr: string; StartPos: SizeInt):boolean;
function EndsWith(S, SubStr: string; EndPos: SizeInt):boolean;
function UTF8TrimLeft(S: string; SubStrs: array of string): string;
function UTF8TrimRight(S: string; SubStrs: array of string): string;
function UTF8TrimString(S: string; SubStrs: array of string): string;
function UTF8TrimSpace(S: string): string;

function UTF8LengthFast(S: PChar; SSize: SizeInt): SizeInt;
function UTF8LengthFast(S: string): SizeInt;

function UTF8Pos(const S,OldPattern: String; const IgnoreCase: Boolean = False): SizeInt;

procedure UTF8FindMatches(const S,OldPattern: String; out aMatches: SizeIntArray;
  const aMatchAll: Boolean = False; const IgnoreCase: Boolean = False);
function UTF8ReplaceMatches(const S, OldPattern, NewPattern: string; out aMatches: SizeIntArray;
  const aMatchAll: Boolean = False; const IgnoreCase: Boolean = False): string;

procedure UTF8SearchSpecial(ID: integer; const AText, ASearchText: string;
  IgnoreCase: boolean; Rst: TSearchResult);
function UTF8ReplaceSpecial(ID: integer; const AText, ASearchText, AReplaceText: string;
  IgnoreCase: boolean; Rst: TSearchResult): string;

function UTF8DiffBytePos(S1, S2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;

function ToLF(S: string): string;
function ToLineEnding(S: string): string;

// function Escape_Abandoned(S: string): string;
function Escape(S: string): string;
function UnEscape(S: string): string;

function ReadFile(FileName: string): string;
procedure WriteFile(FileName: string; Text: string);

function FixFileName(FileName: string): string;
function GetNonExistsPath(ToDir, FileName, FileExt: string; LenSuffix: integer): string;
procedure FindFiles(APath: string; Rst: TStringList);

implementation

{ TSearchResult }

constructor TSearchResult.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TSearchResult.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TSearchResult.Get(Index: integer): PSearchRecord;
begin
  Result := PSearchRecord(FList.Items[Index]);
end;

procedure TSearchResult.Put(Index: integer; Item: PSearchRecord);
begin
  FList.Items[Index] := Item;
end;

procedure TSearchResult.Add(PSR: PSearchRecord);
begin
  FList.Add(PSR);
end;

procedure TSearchResult.Add(ID: integer; UStart, ULength: SizeInt);
var
  PSR: PSearchRecord;
begin
  PSR := new(PSearchRecord);
  PSR^.ID      := ID;
  PSR^.UStart  := UStart;
  PSR^.ULength := ULength;

  FList.Add(PSR);
end;

procedure TSearchResult.Del(Index: integer);
begin
  dispose(PSearchRecord(FList.Items[Index]));
  FList.Delete(Index);
end;

function TSearchResult.Count: integer;
begin
  Result := FList.Count;
end;

procedure TSearchResult.Clear;
var
  i: integer;
begin
  for i := FList.Count - 1 downto 0 do
    dispose(Items[i]);
  FList.Clear;
end;

{ 自定义函数 }

// 判断某个按键是否按下
function IsKeyDown(AKey: integer): boolean; inline;
begin
  Result := GetKeyState(AKey) < 0;
end;

// 判断 SubStr 是否在 S 中，并且开始于 StartPos 位置
function StartsWith(S, SubStr: string; StartPos: SizeInt):boolean;
var
  c: Char;
begin
  if Length(S) - StartPos + 1 < Length(SubStr) then begin
    Result := False;
    Exit;
  end;

  for c in SubStr do begin
    if c = S[StartPos] then
      Inc(StartPos)
    else begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

// 判断 SubStr 是否在 S 中，并且结束于 EndPos 位置
function EndsWith(S, SubStr: string; EndPos: SizeInt):boolean;
var
  c: Char;
begin
  if EndPos < Length(SubStr) then begin
    Result := False;
    Exit;
  end;

  EndPos := EndPos - Length(SubStr) + 1;
  for c in SubStr do begin
    if c = S[EndPos] then
      Inc(EndPos)
    else begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

// 删除 UTF8 字符串左边的指定子字符串
function UTF8TrimLeft(S: string; SubStrs: array of string): string;
var
  SS: string;
  NotFound: boolean;
  StartPos: SizeInt;
begin
  Result := '';
  StartPos := 1;
  repeat
    NotFound := True;
    for SS in SubStrs do begin
      if StartsWith(S, SS, StartPos) then begin
        Inc(StartPos, Length(SS));
        NotFound := False;
      end;
    end;
  until NotFound;

  if StartPos > Length(S) then
    Result := ''
  else
    Result := Copy(S, StartPos, Length(S) - StartPos + 1);
end;

// 删除 UTF8 字符串右边的指定子字符串
function UTF8TrimRight(S: string; SubStrs: array of string): string;
var
  SS: string;
  NotFound: boolean;
  EndPos: SizeInt;
begin
  Result := '';
  EndPos := Length(S);
  repeat
    NotFound := True;
    for SS in SubStrs do begin
      if EndsWith(S, SS, EndPos) then begin
        Dec(EndPos, Length(SS));
        NotFound := False;
      end;
    end;
  until NotFound;

  if EndPos < 1 then
    Result := ''
  else
    Result := Copy(S, 1, EndPos);
end;

// 删除 UTF8 字符串首尾的指定子字符串
function UTF8TrimString(S: string; SubStrs: array of string): string;
var
  SS: string;
  NotFound: boolean;
  StartPos, EndPos: SizeInt;
begin
  Result := '';
  StartPos := 1;
  EndPos := Length(S);
  repeat
    NotFound := True;
    for SS in SubStrs do begin
      if StartsWith(S, SS, StartPos) then begin
        Inc(StartPos, Length(SS));
        NotFound := False;
      end;
    end;
  until NotFound;

  repeat
    NotFound := True;
    for SS in SubStrs do begin
      if EndsWith(S, SS, EndPos) then begin
        Dec(EndPos, Length(SS));
        NotFound := False;
      end;
    end;
  until NotFound;

  if EndPos < StartPos then
    Result := ''
  else
    Result := Copy(S, StartPos, EndPos - StartPos + 1);
end;

// 删除 UTF8 字符串首尾的空白字符
function UTF8TrimSpace(S: string): string;
begin
  Result := UTF8TrimString(S, [' ', '　', #13, #10, #9]);
end;

// 比 lazUTF8.UTF8LengthFast 慢，但比 lazUTF8.UTF8Length 快
// lazUTF8.UTF8LengthFast 不能用在这里，会得到错误的值。
function UTF8LengthFast(S: PChar; SSize: SizeInt): SizeInt;
var
  Pos: Integer;
begin
  Result := 0;
  Pos    := 0;
  while Pos < SSize do begin
    case S[Pos] of
        #0  ..#127: Inc(Pos);
        #192..#223: Inc(Pos, 2);
        #224..#239: Inc(Pos, 3);
        #240..#247: Inc(Pos, 4);
        else begin Inc(Pos); continue; end;
    end;
    Inc(Result);
  end;
end;

function UTF8LengthFast(S: string): SizeInt;
begin
  if S = '' then
    Result := 0
  else
    Result := UTF8LengthFast(@S[1], Length(S));
end;

// 索引从 0 开始
procedure FindMatches(const S,OldPattern: String; out aMatches: SizeIntArray;
  const aMatchAll: Boolean = False; const IgnoreCase: Boolean = False);
begin
  if IgnoreCase then begin
    FindMatchesBoyerMooreCaseINSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end else begin
    FindMatchesBoyerMooreCaseSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end;
end;

// 索引从 0 开始
function ReplaceMatches(const S, OldPattern, NewPattern: string; out aMatches: SizeIntArray;
  const aMatchAll: Boolean = False; const IgnoreCase: Boolean = False): string;
var
  OldPatternSize: SizeInt;
  NewPatternSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchTarget: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
begin
  OldPatternSize:=Length(OldPattern);
  NewPatternSize:=Length(NewPattern);
  if (OldPattern='') or (Length(OldPattern)>Length(S)) then begin
    Result:=S;
    exit;
  end;

  if IgnoreCase then begin
    FindMatchesBoyerMooreCaseINSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end else begin
    FindMatchesBoyerMooreCaseSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end;

  MatchesCount:=Length(aMatches);

  //Create room enougth for the result string
  SetLength(Result,Length(S)-OldPatternSize*MatchesCount+NewPatternSize*MatchesCount);
  MatchIndex:=1;
  MatchTarget:=1;
  //aMatches[x] are 0 based offsets
  for MatchInternal := 0 to Pred(MatchesCount) do begin
    //Copy information up to next match
    AdvanceIndex:=aMatches[MatchInternal]+1-MatchIndex;
    if AdvanceIndex>0 then begin
      move(S[MatchIndex],Result[MatchTarget],AdvanceIndex);
      inc(MatchTarget,AdvanceIndex);
      inc(MatchIndex,AdvanceIndex);
      aMatches[MatchInternal] := MatchTarget - 1;
    end;
    //Copy the new replace information string
    if NewPatternSize>0 then begin
      move(NewPattern[1],Result[MatchTarget],NewPatternSize);
      inc(MatchTarget,NewPatternSize);
    end;
    inc(MatchIndex,OldPatternSize);
  end;
  if MatchTarget<=Length(Result) then begin
    //Add remain data at the end of source.
    move(S[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
  end;
end;

// 索引从 0 开始
procedure UTF8FindMatches(const S,OldPattern: String; out aMatches: SizeIntArray;
  const aMatchAll: Boolean = False; const IgnoreCase: Boolean = False);
var
  OldPatternSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
  MarchIndexU: SizeInt;
  OldPatternSizeU: SizeInt;
begin
  if IgnoreCase then begin
    FindMatchesBoyerMooreCaseINSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end else begin
    FindMatchesBoyerMooreCaseSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end;

  MatchesCount:=Length(aMatches);

  //Create room enougth for the result string
  MatchIndex:=1;
  MarchIndexU:=0;
  OldPatternSize:=Length(OldPattern);
  OldPatternSizeU:=UTF8LengthFast(PChar(OldPattern), Length(OldPattern));
  //aMatches[x] are 0 based offsets
  for MatchInternal := 0 to Pred(MatchesCount) do begin
    AdvanceIndex:=aMatches[MatchInternal]+1-MatchIndex;
    Inc(MarchIndexU, UTF8LengthFast(@S[MatchIndex], AdvanceIndex));
    inc(MatchIndex,AdvanceIndex);
    aMatches[MatchInternal] := MarchIndexU;
    Inc(MarchIndexU, OldPatternSizeU);
    inc(MatchIndex,OldPatternSize);
  end;
end;

// 索引从 0 开始
function UTF8ReplaceMatches(const S, OldPattern, NewPattern: string; out aMatches: SizeIntArray;
  const aMatchAll: Boolean = False; const IgnoreCase: Boolean = False): string;
var
  OldPatternSize: SizeInt;
  NewPatternSize: SizeInt;
  MatchesCount: SizeInt;
  MatchIndex: SizeInt;
  MatchTarget: SizeInt;
  MatchInternal: SizeInt;
  AdvanceIndex: SizeInt;
  MarchTargetU: SizeInt;
  NewPatternSizeU: SizeInt;
begin
  OldPatternSize:=Length(OldPattern);
  NewPatternSize:=Length(NewPattern);
  if (OldPattern='') or (Length(OldPattern)>Length(S)) then begin
    Result:=S;
    exit;
  end;

  if IgnoreCase then begin
    FindMatchesBoyerMooreCaseINSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end else begin
    FindMatchesBoyerMooreCaseSensitive(@S[1],@OldPattern[1],Length(S),Length(OldPattern),aMatches, aMatchAll);
  end;

  MatchesCount:=Length(aMatches);

  //Create room enougth for the result string
  SetLength(Result,Length(S)-OldPatternSize*MatchesCount+NewPatternSize*MatchesCount);
  MatchIndex:=1;
  MatchTarget:=1;
  MarchTargetU:=0;
  NewPatternSizeU:=UTF8LengthFast(PChar(NewPattern), Length(NewPattern));
  //aMatches[x] are 0 based offsets
  for MatchInternal := 0 to Pred(MatchesCount) do begin
    //Copy information up to next match
    AdvanceIndex:=aMatches[MatchInternal]+1-MatchIndex;
    if AdvanceIndex>0 then begin
      move(S[MatchIndex],Result[MatchTarget],AdvanceIndex);
      Inc(MarchTargetU, UTF8LengthFast(@S[MatchIndex], AdvanceIndex));
      inc(MatchTarget,AdvanceIndex);
      inc(MatchIndex,AdvanceIndex);
    end;
    aMatches[MatchInternal] := MarchTargetU;
    Inc(MarchTargetU, NewPatternSizeU);
    //Copy the new replace information string
    if NewPatternSize>0 then begin
      move(NewPattern[1],Result[MatchTarget],NewPatternSize);
      inc(MatchTarget,NewPatternSize);
    end;
    inc(MatchIndex,OldPatternSize);
  end;
  if MatchTarget<=Length(Result) then begin
    //Add remain data at the end of source.
    move(S[MatchIndex],Result[MatchTarget],Length(Result)-MatchTarget+1);
  end;
end;

// 索引从 1 开始
function UTF8Pos(const S,OldPattern: String; const IgnoreCase: Boolean = False): SizeInt;
var
  Matches: SizeIntArray;
begin
  UTF8FindMatches(S, OldPattern, Matches, False, IgnoreCase);
  if Length(Matches) > 0 then
    Result := Matches[Length(Matches) - 1] + 1
  else
    Result := 1;
end;

// 索引从 0 开始
procedure UTF8SearchSpecial(ID: integer; const AText, ASearchText: string;
  IgnoreCase: boolean; Rst: TSearchResult);
var
  i: Integer;
  Matches: SizeIntArray;
  SearchTextLen: Integer;
begin
  SearchTextLen := UTF8LengthFast(ASearchText);
  UTF8FindMatches(AText, ASearchText, Matches, True, IgnoreCase);
  for i := 0 to High(Matches) do
    Rst.Add(ID, Matches[i], SearchTextLen);
end;

// 索引从 0 开始
function UTF8ReplaceSpecial(ID: integer; const AText, ASearchText, AReplaceText: string;
  IgnoreCase: boolean; Rst: TSearchResult): string;
var
  i: Integer;
  Matches: SizeIntArray;
  ReplaceTextLen: Integer;
begin
  ReplaceTextLen := UTF8LengthFast(AReplaceText);
  Result := UTF8ReplaceMatches(AText, ASearchText, AReplaceText, Matches, True, IgnoreCase);
  for i := 0 to High(Matches) do
    Rst.Add(ID, Matches[i], ReplaceTextLen);
end;

// 查找 S1 和 S2 的不同之处，参数和返回值的索引都是从 1 开始，到 Length(S) 结束。
// Start1 表示 S1 的起始查找位置，查找结果也通过该参数返回。
// Start2 表示 S2 的起始查找位置，查找结果也通过该参数返回。
// Reverse 为 True 表示向头查找，False 表示向尾查找。
// 返回值：True 表示找到，False 表示没找到。
// 如果没找到，则向头查找时，某一 Start 返回 0，向尾查找时某一 Start 返回 UTF8Length(S)+1。
function UTF8DiffBytePos(S1, S2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;

  procedure GoToCpStartS1;
  var
    b: byte;
  begin  // 回到 S1 中字符码点的起始位置
    while Start1 > 0 do begin // 如果 UTF8 编码不正确，则 Start1 可能小于等于 0。
      b := Ord(S1[Start1]) shr 6;
      if (b = 3) or (b shr 1 = 0) then
        break;
      Dec(Start1);
    end;
  end;

  procedure GoToCpStartS2;
  var
    b: byte;
  begin  // 回到 S2 中字符码点的起始位置
    while Start2 > 0 do begin // 如果 UTF8 编码不正确，则 Start2 可能小于等于 0。
      b := Ord(S2[Start2]) shr 6;
      if (b = 3) or (b shr 1 = 0) then
        break;
      Dec(Start2);
    end;
  end;

begin
  Result := False;
  if (Start1 <= 0) or (Start2 <= 0) or (Start1 > Length(S1)) or (Start2 > Length(S2)) then Exit;

  if Reverse then begin
    while (Start1 >= 1) and (Start2 >= 1) and (S1[Start1] = S2[Start2]) do begin
      Dec(Start1);
      Dec(Start2);
    end;
    if Start1 > 1 then
      GoToCpStartS1;
    if Start2 > 1 then
      GoToCpStartS2;
    Result := (Start1 > 0) and (Start2 > 0);
  end else begin
    while (Start1 <= Length(S1)) and (Start2 <= Length(S2)) and (S1[Start1] = S2[Start2]) do begin
      Inc(Start1);
      Inc(Start2);
    end;
    if Start1 <= Length(S1) then
      GoToCpStartS1;
    if Start2 <= Length(S2) then
      GoToCpStartS2;
    Result := (Start1 <= Length(S1)) and (Start2 <= Length(S2));
  end;
end;

// 将 \r\n 或 \r 转换为 \n
function ToLF(S: string): string;
var
  Index: integer;
  LastIndex: integer;
  ResultIndex: integer;
begin
  if S = '' then Exit;

  Index := 1;
  LastIndex := 1;
  ResultIndex := 1;
  SetLength(Result, Length(S));
  while Index < Length(S) do begin
    if S[Index] = #13 then begin
      move(S[LastIndex], Result[ResultIndex], Index - LastIndex);
      Inc(ResultIndex, Index - LastIndex);
      Inc(Index);
      LastIndex := Index;
      if S[Index] = #10 then continue;
      Result[ResultIndex] := #10;
      Inc(ResultIndex);
    end else
      Inc(Index);
  end;
  move(S[LastIndex], Result[ResultIndex], Length(S) - LastIndex);
  Inc(ResultIndex, Length(S) - LastIndex);
  if S[Length(S)] = #13 then
    Result[ResultIndex] := #10
  else
    Result[ResultIndex] := S[Length(S)];
  SetLength(Result, ResultIndex);
end;

// 将 \n 转换为系统默认的换行符
function ToLineEnding(S: string): string;
begin
  Result := StringReplace(S, #10, LineEnding, [rfReplaceAll], sraBoyerMoore);
end;

{
// 将特殊字符（#13 #10 #9 \）转换为转义字符(\r \n \t \\)
// 字符串中原有的 \r \n \t \\ 不会进行处理
// 这个转换函数会影响正则表达式字符串，所以弃用
function Escape_Abandoned(S: string): string;
var
  StartPos  : SizeInt;
  i         : SizeInt;
  c         : Char;
  MeetSlash : boolean;
begin
  StartPos  := 1;
  Result    := '';
  MeetSlash := False;

  for i := 1 to Length(S) do begin
    c := S[i];
    if MeetSlash then begin
      MeetSlash := False;
      case c of
        '\', 'r', 'n', 't': continue;
      end;
    end else begin
      case c of
        '\': begin MeetSlash := True; continue; end;
        #13: c := 'r';
        #10: c := 'n';
        #9 : c := 't';
        else continue;
      end;
    end;
    Result := Result + Copy(S, StartPos, i - StartPos) + '\' + c;
    StartPos := i + 1;
  end;

  if StartPos <= Length(S) then
    Result := Result + Copy(S, StartPos, Length(S) - StartPos + 1);
end;
}

// 将特殊字符（#13 #10 #9）转换为转义字符(\r \n \t)
// 这个转换函数不会处理 \ 字符，所以可以用于正则表达式字符串
function Escape(S: string): string;
var
  StartPos  : SizeInt;
  i         : SizeInt;
  c         : Char;
  SS        : string;
  MeetSlash : Boolean;
begin
  StartPos  := 1;
  Result    := '';
  SS        := '';
  MeetSlash := False;

  for i := 1 to Length(S) do begin
    c := S[i];
    if MeetSlash then begin
      MeetSlash := False;
      case c of
        #13: SS := '\\r';
        #10: SS := '\\n';
        #9 : SS := '\\t';
        else continue;
      end;
    end else begin
      case c of
        '\': begin MeetSlash := True; continue; end;
        #13: SS := '\r';
        #10: SS := '\n';
        #9 : SS := '\t';
        else continue;
      end;
    end;
    Result := Result + Copy(S, StartPos, i - StartPos) + SS;
    StartPos := i + 1;
  end;

  if StartPos <= Length(S) then
    Result := Result + Copy(S, StartPos, Length(S) - StartPos + 1);
end;

// 将转义字符（\r \n \t \\）转换为特殊字符（#13 #10 #9 \）
// 其它地方单独的 \ 字符会被保留
// 与 Escape 函数配合使用
function UnEscape(S: string): string;
var
  Start     : SizeInt;
  i         : SizeInt;
  c         : Char;
  MeetSlash : boolean;
begin
  Start     := 1;
  Result    := '';
  MeetSlash := False;

  for i := 1 to Length(S) do begin
    c := S[i];
    if MeetSlash then begin
      MeetSlash := False;
      case c of
        '\': Result := Result + '\';
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        't': Result := Result + #9;
        else Result := Result + '\' + c;
      end;
    end else if c = '\' then begin
      MeetSlash := True;
      Result := Result + Copy(S, Start, i - Start);
      Start := i + 2;
    end;
  end;

  if Start <= Length(S) then
    Result := Result + Copy(S, Start, Length(S) - Start + 1);
end;

// 读取文本文件内容，同时将换行符统一为 #10
function ReadFile(FileName: string): string;
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, AStream.Size);
    AStream.ReadBuffer(Result[1], AStream.Size);
    Result := ToLF(Result);
  finally
    AStream.Free;
  end;
end;

// 将字符串写入文件，同时转换换行符为当前系统默认的换行符
procedure WriteFile(FileName: string; Text: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(FileName, fmCreate);
  try
    Text := ToLineEnding(Text);
    AStream.WriteBuffer(Text[1], Length(Text));
  finally
    AStream.Free;
  end;
end;

// 处理文件名中的非法字符
function FixFileName(FileName: string): string;
var
  i: integer;
begin
  for i := 0 to High(FileName) do
    if FileName[i] in ['/', '\', ':', '<', '>', '*', '?', '|', '"', #9, #10, #13] then
      FileName[i] := '_';
  Result := FileName;
end;

// 获取一个不存在的文件名（避免与现有文件重名）
function GetNonExistsPath(ToDir, FileName, FileExt: string; LenSuffix: integer): string;
var
  i: integer;
  Suffix: string;
begin
  Result := ConcatPaths([ToDir, FileName + FileExt]);
  i := 1;
  while FileExists(Result) or DirectoryExists(Result) do begin
    Suffix := Format('%.' + IntToStr(LenSuffix) + 'd', [i]);
    Result := ConcatPaths([ToDir, FileName + ' (' + Suffix + ')' + FileExt]);
    Inc(i);
  end;
end;

// 搜索 APath 中的所有文件和子目录
procedure FindFiles(APath: string; Rst: TStringList);
var
  SearchRec: TRawbyteSearchRec;
begin
  if FindFirst(ConcatPaths([APath, '*']), faAnyFile or faDirectory, SearchRec) = 0 then
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        continue;
      Rst.Add(SearchRec.Name);
    until FindNext(SearchRec) <> 0;
  FindClose(SearchRec);
end;

end.

