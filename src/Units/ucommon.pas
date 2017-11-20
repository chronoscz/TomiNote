unit ucommon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, lazUTF8;

type

  PSearchRecord = ^TSearchRecord;

  TSearchRecord = record
    ID      : Integer;  // Node ID
    UStart  : SizeInt;  // UTF8 Start
    ULength : SizeInt;  // UTF8 Length
  end;

  PSearchResult = ^TSearchResult;

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

function StartsWith(Str, SubStr: string; StartPos: SizeInt):boolean;
function EndsWith(Str, SubStr: string; EndPos: SizeInt):boolean;
function UTF8TrimLeft(Str: string; SubStrs: array of string): string;
function UTF8TrimRight(Str: string; SubStrs: array of string): string;
function UTF8TrimString(Str: string; SubStrs: array of string): string;
function UTF8TrimSpace(Str: string): string;

function UTF8Search(ID: integer; const AText, ASearchText: string;
  IgnoreCase: boolean; Rst: TSearchResult; StartUPos: SizeInt = 1;
  SearchULen: SizeInt = 0; SearchCount: integer = 0): PtrInt;

function UTF8Replace(ID: integer; const AText, ASearchText, AReplaceText: string;
  IgnoreCase: boolean; Rst: TSearchResult; StartUPos: SizeInt = 1;
  SearchULen: SizeInt = 0; SearchCount: integer = 0): string;

function UTF8DiffBytePos(Str1, Str2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;
function UTF8Diff(Str1, Str2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;

function ToLF(Str: string): string;
function ToLineEnding(Str: string): string;

function Escape(Str: string): string;
function UnEscape(Str: string): string;

function ReadFile(FileName: string): string;
procedure WriteFile(FileName: string; Text: string);

function FixFileName(FileName: string): string;
function GetNotExistsPath(ToDir, FileName, FileExt: string; LenSuffix: integer): string;
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

// 判断 SubStr 是否在 Str 中，并且开始于 StartPos 位置
function StartsWith(Str, SubStr: string; StartPos: SizeInt):boolean;
var
  c: Char;
begin
  if Str.Length - StartPos + 1 < SubStr.Length then begin
    Result := False;
    Exit;
  end;

  for c in SubStr do begin
    if c = Str[StartPos] then
      Inc(StartPos)
    else begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

// 判断 SubStr 是否在 Str 中，并且结束于 EndPos 位置
function EndsWith(Str, SubStr: string; EndPos: SizeInt):boolean;
var
  c: Char;
begin
  if EndPos < SubStr.Length then begin
    Result := False;
    Exit;
  end;

  EndPos := EndPos - SubStr.Length + 1;
  for c in SubStr do begin
    if c = Str[EndPos] then
      Inc(EndPos)
    else begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

// 删除 UTF8 字符串左边的指定子字符串
function UTF8TrimLeft(Str: string; SubStrs: array of string): string;
var
  s: string;
  NotFound: boolean;
  StartPos: SizeInt;
begin
  Result := '';
  StartPos := 1;
  repeat
    NotFound := True;
    for s in SubStrs do begin
      if StartsWith(Str, s, StartPos) then begin
        Inc(StartPos, s.Length);
        NotFound := False;
      end;
    end;
  until NotFound;

  if StartPos > Str.Length then
    Result := ''
  else
    Result := Str.Substring(StartPos - 1, Str.Length - StartPos + 1);
end;

// 删除 UTF8 字符串右边的指定子字符串
function UTF8TrimRight(Str: string; SubStrs: array of string): string;
var
  s: string;
  NotFound: boolean;
  EndPos: SizeInt;
begin
  Result := '';
  EndPos := Str.Length;
  repeat
    NotFound := True;
    for s in SubStrs do begin
      if EndsWith(Str, s, EndPos) then begin
        Dec(EndPos, s.Length);
        NotFound := False;
      end;
    end;
  until NotFound;

  if EndPos < 1 then
    Result := ''
  else
    Result := Str.Substring(0, EndPos);
end;

// 删除 UTF8 字符串首尾的指定子字符串
function UTF8TrimString(Str: string; SubStrs: array of string): string;
var
  s: string;
  NotFound: boolean;
  StartPos, EndPos: SizeInt;
begin
  Result := '';
  StartPos := 1;
  EndPos := Str.Length;
  repeat
    NotFound := True;
    for s in SubStrs do begin
      if StartsWith(Str, s, StartPos) then begin
        Inc(StartPos, s.Length);
        NotFound := False;
      end;
    end;
  until NotFound;

  repeat
    NotFound := True;
    for s in SubStrs do begin
      if EndsWith(Str, s, EndPos) then begin
        Dec(EndPos, s.Length);
        NotFound := False;
      end;
    end;
  until NotFound;

  if EndPos < StartPos then
    Result := ''
  else
    Result := Str.Substring(StartPos - 1, EndPos - StartPos + 1);
end;

// 删除 UTF8 字符串首尾的空白字符
function UTF8TrimSpace(Str: string): string;
begin
  Result := UTF8TrimString(Str, [' ', '　', #13, #10, #9]);
end;

// 查找字符串，如果去掉参数 ID 就是一个通用函数
// AText      ：要在其中进行搜索的源字符串
// ASearchText：要搜索的内容
// IgnoreCase ：忽略大小写
// Rst        ：用来存放搜索结果的 TSearchResult 对象（如果 为 nil 则不记录搜索结果）
// StartUPos  ：搜索的起始字符位置（从 1 开始，不是字节位置）
// SearchULen ：搜索的长度，从 StartUPos 开始，到达此长度则停止搜索，0表示无限制
// SearchCount：搜索的次数，匹配指定次数后则停止搜索，0表示无限制
// 返回值：最后一个查找结果的起始位置
function UTF8Search(ID: integer; const AText, ASearchText: string;
  IgnoreCase: boolean; Rst: TSearchResult;
  StartUPos: SizeInt = 1; SearchULen: SizeInt = 0; SearchCount: integer = 0): PtrInt;
var
  b1, b2: byte;
  BPos, SubBPos: SizeInt;
  UPos, UStart, ULength: SizeInt;
begin
  Result  := 0;

  // 安全检查
  if (StartUPos <= 0) or (AText.Length < ASearchText.Length) then Exit;

  BPos    := 1;
  SubBPos := 1;
  UStart  := 1;
  UPos    := 0;

  // 在这里处理起始位置（跳过指定个数的 UTF8 字符）
  while BPos <= AText.Length do begin
    b1 := Ord(AText[BPos]) shr 6;
    if (b1 = 3) or (b1 shr 1 = 0) then Inc(UPos);

    if UPos = StartUPos then break;
    Inc(BPos);
  end;

  // 安全检查
  if BPos > AText.Length then Exit;

  Dec(UPos);
  ULength := UTF8Length(ASearchText);

  // 按字节遍历
  for BPos := BPos to AText.Length do begin
    b1 := Ord(AText[BPos]);
    b2 := Ord(ASearchText[SubBPos]);
    // 遇到一个 UTF8 字符
    if (b1 shr 6 = 3) or (b1 shr 7 = 0) then begin
      Inc(UPos);
      Dec(SearchULen);
    end;

    if (b1 = b2) or IgnoreCase and ((b1 >= 97) and (b1 <= 122) and (b1 - b2 = 32) or (b1 >= 65) and (b1 <= 90) and (b2 - b1 = 32)) then begin
      // 首字节匹配
      if SubBPos = 1 then UStart := UPos;
      // 完全匹配
      if SubBPos = ASearchText.Length then begin
        Result := UStart;
        SubBPos := 0;
        if Rst <> nil then Rst.Add(ID, UStart, ULength);
        // 这里处理次数限制
        Dec(SearchCount);
        if SearchCount = 0 then Exit;
      end;
      Inc(SubBPos);
    end else
      SubBPos := 1;
    // 在这里处理长度限制
    if SearchULen = 0 then break;
  end;
end;

// 替换字符串，如果去掉参数 ID 就是一个通用函数
// AText       ：要在其中进行搜索的源字符串
// ASearchText ：要搜索的内容
// AReplaceText：用来替换的内容
// IgnoreCase  ：忽略大小写
// Rst         ：用来存放搜索结果的 TSearchResult 对象（如果 为 nil 则不记录搜索结果）
// StartUPos   ：搜索的起始字符位置（从 1 开始，不是字节位置）
// SearchULen  ：搜索的长度，从 StartUPos 开始，到达此长度则停止搜索，0表示无限制
// SearchCount ：搜索的次数，匹配指定次数后则停止搜索，0表示无限制
// 返回值：最后一个查找结果的起始位置
function UTF8Replace(ID: integer; const AText, ASearchText, AReplaceText: string;
  IgnoreCase: boolean; Rst: TSearchResult;
  StartUPos: SizeInt = 1; SearchULen: SizeInt = 0; SearchCount: integer = 0): string;
var
  b1, b2: byte;
  BPos, SubBPos, BStart, LastBStart: SizeInt;
  UPos, UStart, ULength, ULengthFix: SizeInt;
  Count: integer;
begin
  Result := AText;

  // 安全检查
  if (StartUPos <= 0) or (AText.Length < ASearchText.Length) then Exit;

  BPos    := 1;
  SubBPos := 1;
  UStart  := 1;
  UPos    := 0;

  // 跳过指定个数的 UTF8 字符
  while BPos <= AText.Length do begin
    b1 := Ord(AText[BPos]) shr 6;
    if (b1 = 3) or (b1 shr 1 = 0) then Inc(UPos);

    if UPos = StartUPos then break;
    Inc(BPos);
  end;

  // 安全检查
  if BPos > AText.Length then Exit;

  Dec(UPos);
  Result := '';
  Count := 0;
  LastBStart := 1;
  ULength := UTF8Length(AReplaceText);
  ULengthFix := UTF8Length(AReplaceText) - UTF8Length(ASearchText);

  // 按字节遍历
  for BPos := BPos to AText.Length do begin
    b1 := Ord(AText[BPos]);
    b2 := Ord(ASearchText[SubBPos]);
    // 遇到一个 UTF8 字符
    if (b1 shr 6 = 3) or (b1 shr 7 = 0) then begin
      Inc(UPos);
      Dec(SearchULen);
    end;

    if (b1 = b2) or IgnoreCase and ((b1 >= 97) and (b1 <= 122) and (b1 - b2 = 32) or (b1 >= 65) and (b1 <= 90) and (b2 - b1 = 32)) then begin
      // 首字节匹配
      if SubBPos = 1 then begin
        BStart := BPos;
        UStart := UPos + ULengthFix * Count;
      end;
      // 完全匹配
      if SubBPos = ASearchText.Length then begin
        Result := Result + AText.Substring(LastBStart - 1, BStart - LastBStart) + AReplaceText;
        LastBStart := BPos + 1;
        SubBPos := 0;
        if Rst <> nil then Rst.Add(ID, UStart, ULength);
        Inc(Count);
        // 这里处理次数限制
        if Count = SearchCount then break;
      end;
      Inc(SubBPos);
    end else
      SubBPos := 1;
    // 在这里处理长度限制
    if SearchULen = 0 then break;
  end;
  if LastBStart < AText.Length then
    Result := Result + AText.Substring(LastBStart - 1);
end;

// 查找 Str1 和 Str2 的不同之处，参数和返回值的索引都是从 1 开始，到 Length(Str) 结束。
// Start1 表示 Str1 的起始查找位置，查找结果也通过该参数返回。
// Start2 表示 Str2 的起始查找位置，查找结果也通过该参数返回。
// Reverse 为 True 表示向头查找，False 表示向尾查找。
// 返回值：True 表示找到，False 表示没找到。
// 如果没找到，则向头查找时，某一 Start 返回 0，向尾查找时某一 Start 返回 UTF8Length(Str)+1。
function UTF8DiffBytePos(Str1, Str2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;

  procedure GoToCpStartStr1;
  var
    b: byte;
  begin  // 回到 Str1 中字符码点的起始位置
    while Start1 > 0 do begin // 如果 UTF8 编码不正确，则 Start1 可能小于等于 0。
      b := Ord(Str1[Start1]) shr 6;
      if (b = 3) or (b shr 1 = 0) then
        break;
      Dec(Start1);
    end;
  end;

  procedure GoToCpStartStr2;
  var
    b: byte;
  begin  // 回到 Str2 中字符码点的起始位置
    while Start2 > 0 do begin // 如果 UTF8 编码不正确，则 Start2 可能小于等于 0。
      b := Ord(Str2[Start2]) shr 6;
      if (b = 3) or (b shr 1 = 0) then
        break;
      Dec(Start2);
    end;
  end;

begin
  Result := False;
  if (Start1 <= 0) or (Start2 <= 0) or (Start1 > Str1.Length) or (Start2 > Str2.Length) then Exit;

  if Reverse then begin
    while (Start1 >= 1) and (Start2 >= 1) and (Str1[Start1] = Str2[Start2]) do begin
      Dec(Start1);
      Dec(Start2);
    end;
    if Start1 > 1 then
      GoToCpStartStr1;
    if Start2 > 1 then
      GoToCpStartStr2;
    Result := (Start1 > 0) and (Start2 > 0);
  end else begin
    while (Start1 <= Str1.Length) and (Start2 <= Str2.Length) and (Str1[Start1] = Str2[Start2]) do begin
      Inc(Start1);
      Inc(Start2);
    end;
    if Start1 <= Str1.Length then
      GoToCpStartStr1;
    if Start2 <= Str2.Length then
      GoToCpStartStr2;
    Result := (Start1 <= Str1.Length) and (Start2 <= Str2.Length);
  end;
end;

// 查找 Str1 和 Str2 的不同之处，参数和返回值的索引都是从 1 开始，到 UTF8Length(Str) 结束。
// Start1 表示 Str1 的起始查找位置，查找结果也通过该参数返回。
// Start2 表示 Str2 的起始查找位置，查找结果也通过该参数返回。
// Reverse 为 True 表示向头查找，False 表示向尾查找。
// 返回值：True 表示找到，False 表示没找到。
// 如果没找到，则向头查找时，某一 Start 返回 0，向尾查找时某一 Start 返回 UTF8Length(Str)+1。
function UTF8Diff(Str1, Str2: string; var Start1, Start2: SizeInt; Reverse: boolean = False): boolean;
begin
  if not Reverse then begin
    Dec(Start1);
    Dec(Start2);
  end;

  Start1 := UTF8CharToByteIndex(PChar(Str1), Str1.Length, Start1);
  Start2 := UTF8CharToByteIndex(PChar(Str2), Str2.Length, Start2);

  if not Reverse then begin
    Inc(Start1);
    Inc(Start2);
  end;

  Result := UTF8DiffBytePos(Str1, Str2, Start1, Start2, Reverse);

  if Start1 > 0 then Start1 := UTF8Length(PChar(Str1), Start1 - 1) + 1;
  if Start2 > 0 then Start2 := UTF8Length(PChar(Str2), Start2 - 1) + 1;
end;

// 将 \r\n 或 \r 转换为 \n
function ToLF(Str: string): string;
var
  iStart, iEnd: SizeInt;
  Stream: TStringStream;
begin
  iStart := 0;
  Stream := TStringStream.Create('');
  try
    iEnd := Str.IndexOf(#13, iStart);
    while iEnd <> -1 do begin
      Stream.WriteString(Str.Substring(iStart, iEnd - iStart));
      if (iEnd = Str.Length - 1) or (Str[iEnd + 2] <> #10) then
        Stream.WriteString(#10);
      iStart := iEnd + 1;
      iEnd := Str.IndexOf(#13, iStart);
    end;
    if (iEnd < Str.Length - 1) then
      Stream.WriteString(Str.Substring(iStart));
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

// 将 \n 转换为系统默认的换行符
function ToLineEnding(Str: string): string;
var
  StartPos, EndPos: integer;
  Stream: TStringStream;
begin
  if LineEnding = #10 then begin
    Result := Str;
    Exit;
  end;

  StartPos := 0;
  Stream := TStringStream.Create('');
  try
    EndPos := Str.IndexOf(#10, StartPos);
    while EndPos <> -1 do begin
      Stream.WriteString(Str.Substring(StartPos, EndPos - StartPos));
      Stream.WriteString(LineEnding);
      StartPos := EndPos + 1;
      EndPos := Str.IndexOf(#10, StartPos);
    end;
    if (EndPos < Str.Length - 1) then
      Stream.WriteString(Str.Substring(StartPos));
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

// 将特殊字符（#13 #10 #9 \）转换为转义字符(\r \n \t \\)
// 字符串中原有的 \r \n \t \\ 不会进行处理
function Escape(Str: string): string;
var
  Start     : SizeInt;
  i         : SizeInt;
  c         : Char;
  MeetSlash : boolean;
begin
  Start := 1;
  Result := '';
  MeetSlash := False;

  for i := 1 to Str.Length do begin
    c := Str[i];
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
    Result := Result + Str.SubString(Start - 1, i - Start) + '\' + c;
    Start := i + 1;
  end;
  Result := Result + Str.SubString(Start - 1, Str.Length - Start + 1);
  if MeetSlash then Result := Result + '\';
end;

// 将转义字符（\r \n \t \\）转换为特殊字符（#13 #10 #9 \）
function UnEscape(Str: string): string;
var
  Start     : SizeInt;
  i         : SizeInt;
  c         : Char;
  MeetSlash : boolean;
begin
  Start     := 1;
  Result    := '';
  MeetSlash := False;

  for i := 1 to Str.Length do begin
    c := Str[i];
    if MeetSlash then begin
      MeetSlash := False;
      case c of
        '\': Result := Result + '\';
        'r': Result := Result + #13;
        'n': Result := Result + #10;
        't': Result := Result + #9;
        else Result := Result + c;
      end;
    end else if c = '\' then begin
      MeetSlash := True;
      Result := Result + Str.SubString(Start - 1, i - Start);
      Start := i + 2;
    end;
  end;
  Result := Result + Str.SubString(Start - 1, Str.Length - Start + 1);
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
    AStream.WriteBuffer(Text[1], Text.Length);
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
function GetNotExistsPath(ToDir, FileName, FileExt: string; LenSuffix: integer): string;
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

