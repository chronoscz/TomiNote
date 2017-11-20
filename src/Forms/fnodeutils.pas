unit fnodeutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, StdCtrls, ExtCtrls, LCLTranslator, Types, LCLType, Controls;

type

  { TformNodeUtils }

  TformNodeUtils = class(TForm)
    chkbNonGreedy        : TCheckBox;
    chkbMultiLine        : TCheckBox;
    chkbIgnoreCase       : TCheckBox;
    pgctMain             : TPageControl;
    tabsSplit            : TTabSheet;
    lablSeparator        : TLabel;
    lablTitle            : TLabel;
    combSeparator        : TComboBox;
    combTitle            : TComboBox;
    chkbIncludeSeparator : TCheckBox;
    chkbAddPreNum        : TCheckBox;
    chkbAddSufNum        : TCheckBox;
    editPreNumLen        : TEdit;
    editSufNumLen        : TEdit;

    tabsSort             : TTabSheet;
    radgSortDirection    : TRadioGroup;
    radgSortOf           : TRadioGroup;

    bttnOK               : TButton;
    bttnCancel           : TButton;

    procedure bttnCancelClick(Sender: TObject);
    procedure bttnOKClick(Sender: TObject);
    procedure combSeparatorCloseUp(Sender: TObject);
    procedure combTitleCloseUp(Sender: TObject);
    procedure editMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    procedure CancelEvent;
    procedure OKEvent;
    procedure PerformSplit;
    procedure PerformSort;
  public

  end;

var
  formNodeUtils: TformNodeUtils;

resourcestring
  Res_SortDirection = 'Ascending'#10'Descending';
  Res_SortOf        = 'Sibling'#10'Children';

implementation

uses
  fmain, uconfig, ucommon;

{$R *.lfm}

{ TformNodeUtils }

procedure TformNodeUtils.FormCreate(Sender: TObject);
begin
  // 初始化窗口状态
  if Config.KeepNodeUtilsFormRect then begin
    BoundsRect := Config.NodeUtilsFormRect;
    AutoSize := False;
  end;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 这些资源需要手动载入
  radgSortDirection.Items.Text  := Res_SortDirection;
  radgSortOf.Items.Text         := Res_SortOf;
  chkbIgnoreCase.Caption        := Res_IgnoreCase;
  chkbMultiLine.Caption         := Res_MultiLine;
  chkbNonGreedy.Caption         := Res_NonGreedy;

  // 初始化控件状态
  pgctMain.ActivePageIndex      := 0;

  combSeparator.Items           := Config.RecentSeparator;
  combTitle.Items               := Config.RecentTitle;

  combSeparator.Text            := Config.SeparatorText;
  combTitle.Text                := Config.TitleText;

  chkbIgnoreCase.Checked        := Config.SplitIgnoreCase;
  chkbMultiLine.Checked         := Config.SplitMultiLine;
  chkbNonGreedy.Checked         := Config.SplitNonGreedy;

  chkbIncludeSeparator.Checked  := Config.IncludeSeparator;

  chkbAddPreNum.Checked         := Config.AddPrefixNumber;
  chkbAddSufNum.Checked         := Config.AddSuffixNumber;

  editPreNumLen.Text            := IntToStr(Config.PrefixNumberLength);
  editSufNumLen.Text            := IntToStr(Config.SuffixNumberLength);

  radgSortDirection.ItemIndex   := Config.SortDirection;
  radgSortOf.ItemIndex          := Config.SortOf;

  if Config.SwapOKCancel then begin
    bttnOK.Caption              := Res_CaptionCancel;
    bttnOK.Cancel               := True;

    bttnCancel.Caption          := Res_CaptionExecute;
    // bttnCancel.Default       := True;
  end else begin
    bttnOK.Caption              := Res_CaptionExecute;
    // bttnOK.Default           := True;

    bttnCancel.Caption          := Res_CaptionCancel;
    bttnCancel.Cancel           := True;
  end;
end;

procedure TformNodeUtils.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 保存窗口状态
  Config.NodeUtilsFormRect      := BoundsRect;

  combSeparator.Text            := Escape(combSeparator.Text);
  combTitle.Text                := Escape(combTitle.Text);

  // 保存控件状态
  Config.SeparatorText          := combSeparator.Text;
  Config.TitleText              := combTitle.Text;

  Config.SplitIgnoreCase        := chkbIgnoreCase.Checked;
  Config.SplitMultiLine         := chkbMultiLine.Checked;
  Config.SplitNonGreedy         := chkbNonGreedy.Checked;

  Config.IncludeSeparator       := chkbIncludeSeparator.Checked;

  Config.AddPrefixNumber        := chkbAddPreNum.Checked;
  Config.AddSuffixNumber        := chkbAddSufNum.Checked;

  Config.PrefixNumberLength     := StrToIntDef(editPreNumLen.Text, 3);
  Config.SuffixNumberLength     := StrToIntDef(editSufNumLen.Text, 3);

  Config.SortDirection          := radgSortDirection.ItemIndex;
  Config.SortOf                 := radgSortOf.ItemIndex;

  CloseAction                   := caFree;
  formNodeUtils                 := nil;
end;

procedure TformNodeUtils.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformNodeUtils.combSeparatorCloseUp(Sender: TObject);
begin
  if IsKeyDown(VK_SHIFT) and (combSeparator.ItemIndex >= 0) then begin
    Config.RecentSeparator.Delete(combSeparator.ItemIndex);
    combSeparator.Items.Delete(combSeparator.ItemIndex);
  end;
end;

procedure TformNodeUtils.combTitleCloseUp(Sender: TObject);
begin
  if IsKeyDown(VK_SHIFT) and (combTitle.ItemIndex >= 0) then begin
    Config.RecentTitle.Delete(combTitle.ItemIndex);
    combTitle.Items.Delete(combTitle.ItemIndex);
  end;
end;

procedure TformNodeUtils.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformNodeUtils.OKEvent;
var
  Index: integer;
begin
  Hide;

  formMain.SubmitNote;

  // 保存分隔符文本
  if combSeparator.Text <> '' then begin
    combSeparator.Text := Escape(combSeparator.Text);
    Index := Config.RecentSeparator.IndexOf(combSeparator.Text);
    if Index >= 0 then
      Config.RecentSeparator.Delete(Index)
    else if Config.RecentSeparator.Count >= 10 then
      Config.RecentSeparator.Delete(Config.RecentSeparator.Count - 1);

    Config.RecentSeparator.Insert(0, combSeparator.Text);
  end;

  // 保存标题文本
  if combTitle.Text <> '' then begin
    combTitle.Text := Escape(combTitle.Text);
    Index := Config.RecentTitle.IndexOf(combTitle.Text);
    if Index >= 0 then
      Config.RecentTitle.Delete(Index)
    else if Config.RecentTitle.Count >= 10 then
      Config.RecentTitle.Delete(Config.RecentTitle.Count - 1);

    Config.RecentTitle.Insert(0, combTitle.Text);
  end;

  case pgctMain.ActivePageIndex of
    0: PerformSort;
    1: PerformSplit;
  end;
  Close;
end;

procedure TformNodeUtils.CancelEvent;
begin
  Close;
end;

procedure TformNodeUtils.PerformSplit;
var
  HeadStr, SeparatorPattern, TitlePattern: string;
  IncludeSeparator: boolean;
  PreNumLen, SufNumLen: integer;
begin
  HeadStr := '';
  if chkbIgnoreCase.Checked    then HeadStr := HeadStr + '(?i)'   else HeadStr := HeadStr + '(?-i)';
  if chkbMultiLine.Checked     then HeadStr := HeadStr + '(?m-s)' else HeadStr := HeadStr + '(?s-m)';
  if chkbNonGreedy.Checked     then HeadStr := HeadStr + '(?-g)'  else HeadStr := HeadStr + '(?g)';

  SeparatorPattern := combSeparator.Text;
  if SeparatorPattern <> '' then SeparatorPattern := HeadStr + SeparatorPattern;

  TitlePattern     := combTitle.Text;
  if TitlePattern <> '' then TitlePattern := HeadStr + TitlePattern;

  IncludeSeparator := chkbIncludeSeparator.Checked;

  if chkbAddPreNum.Checked then
    PreNumLen := StrToIntDef(editPreNumLen.Text, 0)
  else
    PreNumLen := 0;

  if chkbAddSufNum.Checked then
    SufNumLen := StrToIntDef(editSufNumLen.Text, 0)
  else
    SufNumLen := 0;

  formMain.SplitNote(SeparatorPattern, TitlePattern, IncludeSeparator, PreNumLen, SufNumLen);
end;

procedure TformNodeUtils.PerformSort;
begin
  formMain.SortNode(radgSortOf.ItemIndex = 0, radgSortDirection.ItemIndex = 1);
end;

procedure TformNodeUtils.editMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Num: Integer;
begin
  Num := StrToIntDef((Sender as TEdit).Text, 0);
  if WheelDelta > 0 then Inc(Num) else Dec(Num);
  if Num < 0 then Num := 0;
  (Sender as TEdit).Text := IntToStr(Num);
end;

end.

