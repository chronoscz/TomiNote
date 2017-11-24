unit foptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Types, LCLTranslator;

type

  { TformOptions }

  TformOptions = class(TForm)
    lstbTabs                : TListBox;
    pagcMain                : TPageControl;
    tabsGeneral             : TTabSheet;
    tabsLayout              : TTabSheet;
    tabsTheme               : TTabSheet;
    tabsOthers              : TTabSheet;

    chkgFWHideBars          : TCheckGroup;
    chkbTreeBarAutoSize     : TCheckBox;
    chkbInfoBarAutoSize     : TCheckBox;
    chkbRecyBarAutoSize     : TCheckBox;
    editTreeBarPercent      : TEdit;
    editInfoBarPercent      : TEdit;
    editRecyBarPercent      : TEdit;
    chkgKeepFormSize        : TCheckGroup;
    chkbSwapOKCancel        : TCheckBox;
    chkbRemoveMenuBarItem   : TCheckBox;

    chkbLoadLastFile        : TCheckBox;
    chkbSelectLastNode      : TCheckBox;
    lablAutoSaveInterval    : TLabel;
    lablAutoBackupInterval  : TLabel;
    lablAutoBackupCount     : TLabel;
    editAutoSaveInterval    : TEdit;
    editAutoBackupInterval  : TEdit;
    editAutoBackupCount     : TEdit;
    lablHistoryMaxSize      : TLabel;
    lablHistoryMinCount     : TLabel;
    editHistoryMaxSize      : TEdit;
    editHistoryMinCount     : TEdit;
    chkbKeepNodesHistory    : TCheckBox;
    lablLanguage            : TLabel;
    imagLanguage            : TImage;
    combLanguage            : TComboBox;

    lablWindowFontName      : TLabel;
    lablTreeBarFontName     : TLabel;
    lablNoteBarFontName     : TLabel;
    lablInfoBarFontName     : TLabel;
    combWindowFontName      : TComboBox;
    combTreeFontName        : TComboBox;
    combNoteFontName        : TComboBox;
    combInfoFontName        : TComboBox;
    lablWindowFontSize      : TLabel;
    lablTreeBarFontSize     : TLabel;
    lablNoteBarFontSize     : TLabel;
    lablInfoBarFontSize     : TLabel;
    editWindowFontSize      : TEdit;
    editTreeBarFontSize     : TEdit;
    editNoteBarFontSize     : TEdit;
    editInfoBarFontSize     : TEdit;
    grpbBrightTheme         : TGroupBox;
    lablBrightForeColor     : TLabel;
    lablBrightBackColor     : TLabel;
    clrbBrightForeColor     : TColorButton;
    clrbBrightBackColor     : TColorButton;
    bttnDefBrightTheme      : TButton;
    grpbDarkTheme           : TGroupBox;
    lablDarkForeColor       : TLabel;
    lablDarkBackColor       : TLabel;
    clrbDarkForeColor       : TColorButton;
    clrbDarkBackColor       : TColorButton;
    bttnDefDarkTheme        : TButton;

    lablAutoSaveRemaining   : TLabel;
    lablAutoBackupRemaining : TLabel;
    lablAutoBackupActive    : TLabel;
    editAutoSaveRemaining   : TEdit;
    editAutoBackupRemaining : TEdit;
    lablTotalHistorySize    : TLabel;
    lablTotalHistorySizeNum : TLabel;
    chkbDiscardHistory      : TCheckBox;

    bttnOK                  : TButton;
    bttnCancel              : TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure OKEvent;
    procedure CancelEvent;

    procedure lstbTabsSelectionChange(Sender: TObject; User: boolean);
    procedure editMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure bttnDefDarkThemeClick(Sender: TObject);
    procedure bttnDefBrightThemeClick(Sender: TObject);
  private
    FOldAutoSaveRemaining: integer;
    FOldAutoBackupRemaining: integer;

    FLanguageStrings: TStringList;
    procedure GetLanguageList;
    procedure SetLanguage;
  public
  end;

var
  formOptions: TformOptions;

ResourceString
  Res_lstbTabsItems         = 'Gerenal'#10'Layout'#10'Theme'#10'Others';
  Res_chkgFWHideBarsItems   = 'MenuBar'#10'ToolBar'#10'StatBar'#10'TreeBar'#10'RecyBar'#10'InfoBar';
  Res_chkgKeepFormSizeItems = 'Main'#10'Search'#10'Import'#10'Export'#10'NodeUtils'#10'TextUtils'#10'Options';

implementation

uses
  fmain, uconfig;

{$R *.lfm}

{ TformOptions }

procedure TformOptions.FormCreate(Sender: TObject);
begin
  FLanguageStrings := TStringList.Create;

  // 初始化窗口状态
  if Config.KeepOptionsFormRect then begin
    BoundsRect := Config.OptionsFormRect;
    AutoSize := False;
  end else
    AutoSize := True;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 这些资源需要手动载入
  lstbTabs.Items.Text             := Res_lstbTabsItems;
  chkgFWHideBars.Items.Text       := Res_chkgFWHideBarsItems;
  chkgKeepFormSize.Items.Text     := Res_chkgKeepFormSizeItems;

  // 初始化控件状态
  pagcMain.ShowTabs               := False;
  lstbTabs.ItemIndex              := 0;

  // 常规
  chkbLoadLastFile.Checked        := Config.LoadLastFile;
  chkbSelectLastNode.Checked      := Config.SelectLastNode;

  editAutoSaveInterval.Text       := Config.AutoSaveInterval.ToString;
  editAutoBackupInterval.Text     := Config.AutoBackupInterval.ToString;
  editAutoBackupCount.Text        := Config.AutoBackupCount.ToString;

  editHistoryMaxSize.Text         := Config.HistoryMaxSize.ToString;
  editHistoryMinCount.Text        := Config.HistoryMinCount.ToString;
  chkbKeepNodesHistory.Checked    := Config.KeepNodesHistory;

  // 界面
  chkgFWHideBars.Checked[0]       := Config.FWHideMenuBar;
  chkgFWHideBars.Checked[1]       := Config.FWHideToolBar;
  chkgFWHideBars.Checked[2]       := Config.FWHideStatBar;
  chkgFWHideBars.Checked[3]       := Config.FWHideTreeBar;
  chkgFWHideBars.Checked[4]       := Config.FWHideRecyBar;
  chkgFWHideBars.Checked[5]       := Config.FWHideInfoBar;

  chkbTreeBarAutoSize.Checked     := Config.TreeBarAutoSize;
  editTreeBarPercent.Text         := Config.TreeBarPercent.ToString;
  chkbRecyBarAutoSize.Checked     := Config.RecyBarAutoSize;
  editRecyBarPercent.Text         := Config.RecyBarPercent.ToString;
  chkbInfoBarAutoSize.Checked     := Config.InfoBarAutoSize;
  editInfoBarPercent.Text         := Config.InfoBarPercent.ToString;

  chkgKeepFormSize.Checked[0]     := Config.KeepMainFormRect;
  chkgKeepFormSize.Checked[1]     := Config.KeepSearchFormRect;
  chkgKeepFormSize.Checked[2]     := Config.KeepImportFormRect;
  chkgKeepFormSize.Checked[3]     := Config.KeepExportFormRect;
  chkgKeepFormSize.Checked[4]     := Config.KeepNodeUtilsFormRect;
  chkgKeepFormSize.Checked[5]     := Config.KeepTextUtilsFormRect;
  chkgKeepFormSize.Checked[6]     := Config.KeepOptionsFormRect;

  chkbSwapOKCancel.Checked        := Config.SwapOKCancel;
  chkbRemoveMenuBarItem.Checked   := Config.RemoveMenuBarItem;

  // 主题
  combWindowFontName.Items        := Screen.Fonts;
  combTreeFontName.Items          := Screen.Fonts;
  combNoteFontName.Items          := Screen.Fonts;
  combInfoFontName.Items          := Screen.Fonts;

  combWindowFontName.ItemIndex    := combWindowFontName.Items.IndexOf(Config.WindowFontName);
  combTreeFontName.ItemIndex      := combTreeFontName.Items.IndexOf(Config.TreeBarFontName);
  combNoteFontName.ItemIndex      := combNoteFontName.Items.IndexOf(Config.NoteBarFontName);
  combInfoFontName.ItemIndex      := combInfoFontName.Items.IndexOf(Config.InfoBarFontName);

  lablTreeBarFontSize.Caption     := lablWindowFontSize.Caption;
  lablNoteBarFontSize.Caption     := lablWindowFontSize.Caption;
  lablInfoBarFontSize.Caption     := lablWindowFontSize.Caption;

  editWindowFontSize.Text         := Config.WindowFontSize.ToString;
  editTreeBarFontSize.Text        := Config.TreeBarFontSize.ToString;
  editNoteBarFontSize.Text        := Config.NoteBarFontSize.ToString;
  editInfoBarFontSize.Text        := Config.InfoBarFontSize.ToString;

  clrbBrightForeColor.ButtonColor := Config.BrightFontColor;
  clrbBrightBackColor.ButtonColor := Config.BrightBackColor;

  clrbDarkForeColor.ButtonColor   := Config.DarkFontColor;
  clrbDarkBackColor.ButtonColor   := Config.DarkBackColor;

  lablDarkForeColor.Caption       := lablBrightForeColor.Caption;
  lablDarkBackColor.Caption       := lablBrightBackColor.Caption;
  bttnDefDarkTheme.Caption        := bttnDefBrightTheme.Caption;

  GetLanguageList;

  // 其它
  FOldAutoSaveRemaining           := Config.AutoSaveRemaining;
  FOldAutoBackupRemaining         := Config.AutoBackupRemaining;

  editAutoSaveRemaining.Text      := Config.AutoSaveRemaining.ToString;
  editAutoBackupRemaining.Text    := Config.AutoBackupRemaining.ToString;

  if Config.ChangedAfterBackup then
    lablAutoBackupActive.Caption  := '(' + Res_Activated + ')'
  else
    lablAutoBackupActive.Caption  := '(' + Res_NotActive + ')';

  lablTotalHistorySizeNum.Caption := IntToStr(formMain.GetTotalHistorySize div 1024) + ' KB';

  if Config.SwapOKCancel then begin
    bttnOK.Caption          := Res_CaptionCancel;
    bttnOK.Cancel           := True;

    bttnCancel.Caption      := Res_CaptionOK;
    // bttnCancel.Default      := True;
  end else begin
    bttnOK.Caption          := Res_CaptionOK;
    // bttnOK.Default          := True;

    bttnCancel.Caption      := Res_CaptionCancel;
    bttnCancel.Cancel       := True;
  end;
end;

procedure TformOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Config.OptionsFormRect := BoundsRect;
  FLanguageStrings.Free;
  CloseAction := caFree;
  formOptions := nil;
end;

procedure TformOptions.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformOptions.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformOptions.OKEvent;
var
  Num: Integer;
begin
  Hide;

  // 常规
  Config.LoadLastFile        := chkbLoadLastFile.Checked;
  Config.SelectLastNode      := chkbSelectLastNode.Checked;

  Config.AutoSaveInterval    := StrToIntDef(editAutoSaveInterval.Text, DefAutoSaveInterval);
  Config.AutoBackupInterval  := StrToIntDef(editAutoBackupInterval.Text, DefAutoBackupInterval);
  Config.AutoBackupCount     := StrToIntDef(editAutoBackupCount.Text, DefAutoBackupCount);

  if Config.AutoSaveRemaining > Config.AutoSaveInterval then
    Config.AutoSaveRemaining := Config.AutoSaveInterval;

  if Config.AutoBackupRemaining > Config.AutoBackupInterval then
    Config.AutoBackupRemaining := Config.AutoBackupInterval;

  Config.HistoryMaxSize      := StrToIntDef(editHistoryMaxSize.Text, DefHistoryMaxSize);
  Config.HistoryMinCount     := StrToIntDef(editHistoryMinCount.Text, DefHistoryMinCount);
  Config.KeepNodesHistory    := chkbKeepNodesHistory.Checked;

  // 界面
  Config.FWHideMenuBar       := chkgFWHideBars.Checked[0];
  Config.FWHideToolBar       := chkgFWHideBars.Checked[1];
  Config.FWHideStatBar       := chkgFWHideBars.Checked[2];
  Config.FWHideTreeBar       := chkgFWHideBars.Checked[3];
  Config.FWHideRecyBar       := chkgFWHideBars.Checked[4];
  Config.FWHideInfoBar       := chkgFWHideBars.Checked[5];

  Config.TreeBarAutoSize     := chkbTreeBarAutoSize.Checked;
  Config.RecyBarAutoSize     := chkbRecyBarAutoSize.Checked;
  Config.InfoBarAutoSize     := chkbInfoBarAutoSize.Checked;

  Config.TreeBarPercent      := StrToIntDef(editTreeBarPercent.Text, DefTreeBarPercent);
  Config.RecyBarPercent      := StrToIntDef(editRecyBarPercent.Text, DefRecyBarPercent);
  Config.InfoBarPercent      := StrToIntDef(editInfoBarPercent.Text, DefInfoBarPercent);

  Config.KeepMainFormRect    := chkgKeepFormSize.Checked[0];
  Config.KeepSearchFormRect  := chkgKeepFormSize.Checked[1];
  Config.KeepImportFormRect  := chkgKeepFormSize.Checked[2];
  Config.KeepExportFormRect  := chkgKeepFormSize.Checked[3];
  Config.KeepNodeUtilsFormRect := chkgKeepFormSize.Checked[4];
  Config.KeepTextUtilsFormRect := chkgKeepFormSize.Checked[5];
  Config.KeepOptionsFormRect := chkgKeepFormSize.Checked[6];

  Config.SwapOKCancel        := chkbSwapOKCancel.Checked;
  Config.RemoveMenuBarItem   := chkbRemoveMenuBarItem.Checked;

  // 主题
  Config.WindowFontName      := combWindowFontName.Text;
  Config.TreeBarFontName     := combTreeFontName.Text;
  Config.NoteBarFontName     := combNoteFontName.Text;
  Config.InfoBarFontName     := combInfoFontName.Text;

  Config.WindowFontSize      := StrToIntDef(editWindowFontSize.Text, DefWindowFontSize);
  Config.TreeBarFontSize     := StrToIntDef(editTreeBarFontSize.Text, DefTreeBarFontSize);
  Config.NoteBarFontSize     := StrToIntDef(editNoteBarFontSize.Text, DefNoteBarFontSize);
  Config.InfoBarFontSize     := StrToIntDef(editInfoBarFontSize.Text, DefInfoBarFontSize);

  Config.BrightFontColor     := clrbBrightForeColor.ButtonColor;
  Config.BrightBackColor     := clrbBrightBackColor.ButtonColor;

  Config.DarkFontColor       := clrbDarkForeColor.ButtonColor;
  Config.DarkBackColor       := clrbDarkBackColor.ButtonColor;

  SetLanguage;

  // 其它
  Num := StrToIntDef(editAutoSaveRemaining.Text, Config.AutoSaveRemaining);
  if FOldAutoSaveRemaining <> Num then
    Config.AutoSaveRemaining := Num;

  Num := StrToIntDef(editAutoBackupRemaining.Text, Config.AutoBackupRemaining);
  if FOldAutoBackupRemaining <> Num then
    Config.AutoBackupRemaining := Num;

  if chkbDiscardHistory.Checked then
    formMain.DiscardHistory(False)
  else if not Config.KeepNodesHistory then
    formMain.DiscardHistory(True);

  formMain.LoadControlState;

  Close;
end;

procedure TformOptions.CancelEvent;
begin
  Close;
end;

procedure TformOptions.lstbTabsSelectionChange(Sender: TObject; User: boolean);
begin
  if pagcMain.PageCount > lstbTabs.ItemIndex then
    pagcMain.ActivePageIndex := lstbTabs.ItemIndex;
end;

procedure TformOptions.editMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Num: Integer;
begin
  Num := StrToIntDef((Sender as TEdit).Text, 0);
  if WheelDelta > 0 then Inc(Num) else Dec(Num);
  if Num < 0 then Num := 0;
  (Sender as TEdit).Text := IntToStr(Num);
end;

procedure TformOptions.bttnDefBrightThemeClick(Sender: TObject);
begin
  clrbBrightForeColor.ButtonColor := DefBrightFontColor;
  clrbBrightBackColor.ButtonColor := DefBrightBackColor;
end;

procedure TformOptions.bttnDefDarkThemeClick(Sender: TObject);
begin
  clrbDarkForeColor.ButtonColor  := DefDarkFontColor;
  clrbDarkBackColor.ButtonColor  := DefDarkBackColor;
end;

procedure TformOptions.GetLanguageList;
var
  i: integer;
  ALangDir: string;
  ALangCode, ALocale: string;
begin
  FLanguageStrings.Delimiter := '|';
  FLanguageStrings.StrictDelimiter := True;
  FLanguageStrings.DelimitedText := Res_LangStrings;

  ALangDir := formMain.LangDir;

  for i := 0 to FLanguageStrings.Count - 1 do begin
    ALangCode := FLanguageStrings.Names[i];
    ALocale := FLanguageStrings.ValueFromIndex[i];

    if FileExists(ConcatPaths([ALangDir, AppName + '.' + ALangCode + '.po'])) or
      FileExists(ConcatPaths([ALangDir, AppName + '.' + ALangCode + '.mo'])) then
        combLanguage.Items.Add(ALocale);

    if ALangCode = Config.Language then
      combLanguage.ItemIndex := combLanguage.Items.Count - 1;
  end;
end;

procedure TformOptions.SetLanguage;
var
  Index: integer;
  Lang: string;
begin
  Lang := '';
  for Index := 0 to FLanguageStrings.Count - 1 do begin
    if FLanguageStrings.ValueFromIndex[Index] = combLanguage.Text then begin
      Lang := FLanguageStrings.Names[Index];
      break;
    end;
  end;

  Config.Language:= Lang;
  SetDefaultLang(Lang, formMain.LangDir);
  formMain.actnTextUtils.Caption := formMain.actnNodeUtils.Caption;
end;

end.
