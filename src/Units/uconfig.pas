unit uconfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Forms, Controls, ComCtrls, Graphics, lazUTF8;

type

  { TConfig }

  TConfig = class(TObject)
  private
    FConfigFile           : string;

    procedure Load;
    procedure Save;
  public
    // 主窗口
    KeepMainFormRect      : boolean;
    MainFormRect          : TRect;

    MenuBarVisible        : boolean;
    ToolBarVisible        : boolean;
    StatBarVisible        : boolean;
    TreeBarVisible        : boolean;
    RecyBarVisible        : boolean;
    InfoBarVisible        : boolean;
    Maximized             : boolean;
    FullScreen            : boolean;

    TreeBarWidth          : integer;
    RecyBarHeight         : integer;
    InfoBarHeight         : integer;

    WordWrap              : boolean;

    RecentFiles           : TStringList;

    // 搜索
    KeepSearchFormRect    : boolean;
    SearchFormRect        : TRect;

    SearchFrom            : integer;
    SearchName            : boolean;
    SearchNote            : boolean;
    SearchIgnoreCase      : boolean;
    UseRegExpr            : boolean;
    SearchMultiLine       : boolean;
    SearchNonGreedy       : boolean;

    SearchText            : string;
    DoReplace             : boolean;
    ReplaceText           : string;

    RecentSearch          : TStringList;
    RecentReplace         : TStringList;

    // 导入
    KeepImportFormRect    : boolean;
    ImportFormRect        : TRect;

    ImportFrom            : integer;
    ImportMode            : integer;

    ImportFileExt         : boolean;
    ImportRootDir         : boolean;

    LastImportDir         : string;

    // 导出
    KeepExportFormRect    : boolean;
    ExportFormRect        : TRect;

    ExportTo              : integer;
    ExportMode            : integer;

    ExportAddFileExt      : boolean;
    ExportFileExt         : string;

    ExportAddSeparator    : boolean;
    ExportSeparator       : string;

    LastExportDir         : string;

    // 节点工具
    KeepNodeUtilsFormRect : boolean;
    NodeUtilsFormRect     : TRect;

    SeparatorText         : string;
    TitleText             : string;
    SplitIgnoreCase       : boolean;
    SplitMultiLine        : boolean;
    SplitNonGreedy        : boolean;
    IncludeSeparator      : boolean;
    AddPrefixNumber       : boolean;
    AddSuffixNumber       : boolean;
    PrefixNumberLength    : integer;
    SuffixNumberLength    : integer;
    RecentSeparator       : TStringList;
    RecentTitle           : TStringList;
    SortDirection         : integer;
    SortOf                : integer;

    // 文本工具
    KeepTextUtilsFormRect : boolean;
    TextUtilsFormRect     : TRect;

    SearchInSelection     : boolean;
    ScriptListBarWidth    : integer;
    LastScriptID          : integer;

    // 选项
    KeepOptionsFormRect   : boolean;
    OptionsFormRect       : TRect;

    LoadLastFile          : boolean;
    LastFile              : string;
    SelectLastNode        : boolean;

    // 选项 - 自动保存
    AutoSaveInterval      : integer;
    AutoSaveRemaining     : integer;

    // 选项 - 自动备份
    AutoBackupInterval    : integer;
    AutoBackupRemaining   : integer;
    ChangedAfterBackup    : boolean;

    AutoBackupCount       : integer;

    // 选项 - 历史记录
    HistoryMaxSize        : integer;
    HistoryMinCount       : integer;
    KeepNodesHistory      : boolean;

    // 选项 - 界面
    FWHideMenuBar         : boolean;
    FWHideToolBar         : boolean;
    FWHideStatBar         : boolean;
    FWHideTreeBar         : boolean;
    FWHideRecyBar         : boolean;
    FWHideInfoBar         : boolean;

    TreeBarAutoSize       : boolean;
    TreeBarPercent        : integer;

    RecyBarAutoSize       : boolean;
    RecyBarPercent        : integer;

    InfoBarAutoSize       : boolean;
    InfoBarPercent        : integer;

    SwapOKCancel          : boolean;
    RemoveMenuBarItem     : boolean;

    // 选项 - 主题
    ActiveTheme           : integer;

    BrightFontColor       : TColor;
    BrightBackColor       : TColor;

    DarkFontColor         : TColor;
    DarkBackColor         : TColor;

    TreeBarFontName       : string;
    NoteBarFontName       : string;
    InfoBarFontName       : string;
    WindowFontName        : string;

    TreeBarFontSize       : integer;
    NoteBarFontSize       : integer;
    InfoBarFontSize       : integer;
    WindowFontSize        : integer;

    Language              : string;

    constructor Create(FileName: string);
    destructor Destroy; override;
  end;

var
  Config: TConfig;

  AppDir                  : string;
  AppName                 : string;

ResourceString
  // 所有单元都会 use uconfig
  Res_CaptionOK           = 'OK(&O)';
  Res_CaptionExecute      = 'Execute(&E)';
  Res_CaptionCancel       = 'Cancel(&C)';

  Res_CaptionImport       = 'Import(&I)';
  Res_CaptionExport       = 'Export(&E)';

  Res_CaptionSearch       = 'Search(&S)';
  Res_CaptionReplace      = 'Replace(&R)';

  Res_Activated           = 'Activated';
  Res_NotActive           = 'NotActive';

  Res_IgnoreCase          = 'Ignore case';
  Res_MultiLine           = 'Multi line';
  Res_NonGreedy           = 'Non greedy';

const
  AppTitle                = 'TomiNote';
  Version                 = 'v1.0 Beta3';

  AllDepth                = 0;

  DefAutoSaveInterval     = 10;
  DefAutoBackupInterval   = 60;
  DefAutoBackupCount      = 5;

  DefHistoryMaxSize       = 256;
  DefHistoryMinCount      = 10;

  DefMainFormLeft         = 0;
  DefMainFormTop          = 0;
  DefMainFormWidth        = 1000;
  DefMainFormHeight       = 740;

  DefTreeBarPercent       = 25;
  DefRecyBarPercent       = 30;
  DefInfoBarPercent       = 30;

  BrightThemeID           = 1;
  DarkThemeID             = 2;

  DefBrightFontColor      = $212121;
  DefBrightBackColor      = $F7F7F7;
  DefDarkFontColor        = $D3D7CF;
  DefDarkBackColor        = $2E3436;

  DefFontName             = '';

  DefWindowFontSize       = 0;
  DefTreeBarFontSize      = 12;
  DefNoteBarFontSize      = 16;
  DefInfoBarFontSize      = 12;


implementation

const

  DefSearchFormWidth      = 320;
  DefSearchFormHeight     = 420;

  DefImportFormWidth      = 320;
  DefImportFormHeight     = 420;

  DefExportFormWidth      = 320;
  DefExportFormHeight     = 420;

  DefNodeUtilsFormWidth   = 640;
  DefNodeUtilsFormHeight  = 480;

  DefTextUtilsFormWidth   = 640;
  DefTextUtilsFormHeight  = 480;

  DefOptionsFormWidth     = 680;
  DefOptionsFormHeight    = 500;

  DefActiveTheme          = BrightThemeID;

  DefFileExt              = '.txt';
  DefSeparator            = '==============================';

{ TConfig }

constructor TConfig.Create(FileName: string);
begin
  FConfigFile      := FileName;

  RecentFiles      := TStringList.Create;

  RecentSeparator  := TStringList.Create;
  RecentTitle      := TStringList.Create;

  RecentSearch     := TStringList.Create;
  RecentReplace    := TStringList.Create;

  Load;
  inherited Create;
end;

destructor TConfig.Destroy;
begin
  Save;

  FreeAndNil(RecentSearch);
  FreeAndNil(RecentSearch);

  FreeAndNil(RecentSeparator);
  FreeAndNil(RecentTitle);

  FreeAndNil(RecentFiles);

  inherited Destroy;
end;

procedure TConfig.Load;
var
  IniFile: TIniFile;
  i: integer;
  Recent: string;
begin
  IniFile                 := TIniFile.Create    (FConfigFile, [ifoStripQuotes]);
  try
    // 主窗口
    KeepMainFormRect      := IniFile.ReadBool   ('Main', 'KeepMainFormRect', True);

    MainFormRect.Left     := IniFile.ReadInteger('Main', 'Left', DefMainFormLeft);
    MainFormRect.Top      := IniFile.ReadInteger('Main', 'Top', DefMainFormTop);
    MainFormRect.Width    := IniFile.ReadInteger('Main', 'Width', DefMainFormWidth);
    MainFormRect.Height   := IniFile.ReadInteger('Main', 'Height', DefMainFormHeight);

    MenuBarVisible        := IniFile.ReadBool   ('Main', 'MenuBarVisible', True);
    ToolBarVisible        := IniFile.ReadBool   ('Main', 'ToolBarVisible', True);
    StatBarVisible        := IniFile.ReadBool   ('Main', 'StatBarVisible', True);
    TreeBarVisible        := IniFile.ReadBool   ('Main', 'TreeBarVisible', True);
    InfoBarVisible        := IniFile.ReadBool   ('Main', 'InfoBarVisible', False);
    RecyBarVisible        := IniFile.ReadBool   ('Main', 'RecyBarVisible', False);

    TreeBarWidth          := IniFile.ReadInteger('Main', 'TreeBarWidth', DefMainFormWidth * DefTreeBarPercent div 100);
    InfoBarHeight         := IniFile.ReadInteger('Main', 'InfoBarHeight', DefMainFormHeight * DefInfoBarPercent div 100);
    RecyBarHeight         := IniFile.ReadInteger('Main', 'RecyBarHeight', DefMainFormHeight * DefRecyBarPercent div 100);

    Maximized             := IniFile.ReadBool   ('Main', 'Maximized', False);
    FullScreen            := IniFile.ReadBool   ('Main', 'FullScreen', False);

    WordWrap              := IniFile.ReadBool   ('Main', 'WordWrap', True);

    RecentFiles.Clear;
    for i := 1 to 10 do begin
      Recent              := IniFile.ReadString ('Main', 'RecentFile' + IntToStr(i), '');
      if Recent <> '' then RecentFiles.Add(Recent);
    end;

    // 界面
    FWHideMenuBar         := IniFile.ReadBool   ('Layout', 'FullWindowHideMenuBar', True);
    FWHideToolBar         := IniFile.ReadBool   ('Layout', 'FullWindowHideToolBar', True);
    FWHideStatBar         := IniFile.ReadBool   ('Layout', 'FullWindowHideStatBar', True);
    FWHideTreeBar         := IniFile.ReadBool   ('Layout', 'FullWindowHideTreeBar', False);
    FWHideInfoBar         := IniFile.ReadBool   ('Layout', 'FullWindowHideInfoBar', False);
    FWHideRecyBar         := IniFile.ReadBool   ('Layout', 'FullWindowHideRecyBar', False);

    TreeBarAutoSize       := IniFile.ReadBool   ('Layout', 'TreeBarAutoSize', True);
    InfoBarAutoSize       := IniFile.ReadBool   ('Layout', 'InfoBarAutoSize', True);
    RecyBarAutoSize       := IniFile.ReadBool   ('Layout', 'RecyBarAutoSize', True);

    TreeBarPercent        := IniFile.ReadInteger('Layout', 'TreeBarPercent', DefTreeBarPercent);
    InfoBarPercent        := IniFile.ReadInteger('Layout', 'InfoBarPercent', DefInfoBarPercent);
    RecyBarPercent        := IniFile.ReadInteger('Layout', 'RecyBarPercent', DefRecyBarPercent);

    SwapOKCancel          := IniFile.ReadBool   ('Layout', 'SwapOKCancel', False);
    RemoveMenuBarItem     := IniFile.ReadBool   ('Layout', 'RemoveMenuBarItem', False);

    // 主题
    ActiveTheme           := IniFile.ReadInteger('Theme', 'ActiveTheme', DefActiveTheme);

    BrightFontColor       := IniFile.ReadInteger('Theme', 'BrightFontColor', DefBrightFontColor);
    BrightBackColor       := IniFile.ReadInteger('Theme', 'BrightBackColor', DefBrightBackColor);

    DarkFontColor         := IniFile.ReadInteger('Theme', 'DarkFontColor', DefDarkFontColor);
    DarkBackColor         := IniFile.ReadInteger('Theme', 'DarkBackColor', DefDarkBackColor);

    WindowFontName        := IniFile.ReadString ('Theme', 'WindowFontName', DefFontName);
    TreeBarFontName       := IniFile.ReadString ('Theme', 'TreeBarFontName', DefFontName);
    NoteBarFontName       := IniFile.ReadString ('Theme', 'NoteBarFontName', DefFontName);
    InfoBarFontName       := IniFile.ReadString ('Theme', 'InfoBarFontName', DefFontName);

    WindowFontSize        := IniFile.ReadInteger('Theme', 'WindowFontSize', DefWindowFontSize);
    TreeBarFontSize       := IniFile.ReadInteger('Theme', 'TreeBarFontSize', DefTreeBarFontSize);
    NoteBarFontSize       := IniFile.ReadInteger('Theme', 'NoteBarFontSize', DefNoteBarFontSize);
    InfoBarFontSize       := IniFile.ReadInteger('Theme', 'InfoBarFontSize', DefInfoBarFontSize);

    Language              := IniFile.ReadString ('Theme', 'Language', 'en');

    // 搜索
    KeepSearchFormRect    := IniFile.ReadBool   ('Search', 'KeepSearchFormRect', False);

    SearchFormRect.Left   := iniFile.ReadInteger('Search', 'Left', (Screen.Width - DefSearchFormWidth) div 2);
    SearchFormRect.Top    := iniFile.ReadInteger('Search', 'Top', (Screen.Height - DefSearchFormHeight) div 2);
    SearchFormRect.Width  := iniFile.ReadInteger('Search', 'Width', DefSearchFormWidth);
    SearchFormRect.Height := iniFile.ReadInteger('Search', 'Height', DefSearchFormHeight);

    SearchFrom            := IniFile.ReadInteger('Search', 'SearchFrom', 1);
    DoReplace             := IniFile.ReadBool   ('Search', 'DoReplace', False);
    SearchName            := IniFile.ReadBool   ('Search', 'SearchName', False);
    SearchNote            := IniFile.ReadBool   ('Search', 'SearchNote', True);
    SearchIgnoreCase      := IniFile.ReadBool   ('Search', 'IgnoreCase', True);
    UseRegExpr            := IniFile.ReadBool   ('Search', 'UseRegExpr', False);
    SearchMultiLine       := IniFile.ReadBool   ('Search', 'MultiLine', True);
    SearchNonGreedy       := IniFile.ReadBool   ('Search', 'NonGreedy', True);

    SearchText            := IniFile.ReadString ('Search', 'SearchText', '');
    ReplaceText           := IniFile.ReadString ('Search', 'ReplaceText', '');

    RecentSearch.Clear;
    for i := 1 to 10 do begin
      Recent              := IniFile.ReadString ('Search', 'RecentSearch' + IntToStr(i), '');
      if Recent <> '' then RecentSearch.Add(Recent);
    end;

    RecentReplace.Clear;
    for i := 1 to 10 do begin
      Recent              := IniFile.ReadString ('Search', 'RecentReplace' + IntToStr(i), '');
      if Recent <> '' then RecentReplace.Add(Recent);
    end;

    // 导入
    KeepImportFormRect    := IniFile.ReadBool   ('Import', 'KeepImportFormRect', False);

    ImportFormRect.Left   := iniFile.ReadInteger('Import', 'Left', (Screen.Width - DefImportFormWidth) div 2);
    ImportFormRect.Top    := iniFile.ReadInteger('Import', 'Top', (Screen.Height - DefImportFormHeight) div 2);
    ImportFormRect.Width  := iniFile.ReadInteger('Import', 'Width', DefImportFormWidth);
    ImportFormRect.Height := iniFile.ReadInteger('Import', 'Height', DefImportFormHeight);

    ImportFrom            := IniFile.ReadInteger('Import', 'ImportFrom', 1);
    ImportMode            := IniFile.ReadInteger('Import', 'ImportMode', 1);

    ImportFileExt         := IniFile.ReadBool   ('Import', 'IncludeFileExt', False);
    ImportRootDir         := IniFile.ReadBool   ('Import', 'IncludeRootDir', True);

    LastImportDir         := IniFile.ReadString ('Import', 'LastImportDir', '');

    // 导出
    KeepExportFormRect    := IniFile.ReadBool   ('Export', 'KeepExportFormRect', False);

    ExportFormRect.Left   := iniFile.ReadInteger('Export', 'Left', (Screen.Width - DefExportFormWidth) div 2);
    ExportFormRect.Top    := iniFile.ReadInteger('Export', 'Top', (Screen.Height - DefExportFormHeight) div 2);
    ExportFormRect.Width  := iniFile.ReadInteger('Export', 'Width', DefExportFormWidth);
    ExportFormRect.Height := iniFile.ReadInteger('Export', 'Height', DefExportFormHeight);

    ExportTo              := IniFile.ReadInteger('Export', 'ExportTo', 1);
    ExportMode            := IniFile.ReadInteger('Export', 'ExportMode', 1);

    ExportAddFileExt      := IniFile.ReadBool   ('Export', 'AddFileExt', True);
    ExportFileExt         := IniFile.ReadString ('Export', 'FileExt', DefFileExt);

    ExportAddSeparator    := IniFile.ReadBool   ('Export', 'AddSeparator', True);
    ExportSeparator       := IniFile.ReadString ('Export', 'Separator', DefSeparator);

    LastExportDir         := IniFile.ReadString ('Export', 'LastExportDir', '');

    // 节点工具
    KeepNodeUtilsFormRect := IniFile.ReadBool   ('NodeUtils', 'KeepNodeUtilsFormRect', True);

    NodeUtilsFormRect.Left   := IniFile.ReadInteger('NodeUtils', 'Left', (Screen.Width - DefNodeUtilsFormWidth) div 2);
    NodeUtilsFormRect.Top    := IniFile.ReadInteger('NodeUtils', 'Top', (Screen.Width - DefNodeUtilsFormHeight) div 2);
    NodeUtilsFormRect.Width  := IniFile.ReadInteger('NodeUtils', 'Width', DefNodeUtilsFormWidth);
    NodeUtilsFormRect.Height := IniFile.ReadInteger('NodeUtils', 'Height', DefNodeUtilsFormHeight);

    SeparatorText         := IniFile.ReadString ('NodeUtils', 'SeparatorText', '');
    TitleText             := IniFile.ReadString ('NodeUtils', 'TitleText', '');
    SplitIgnoreCase       := IniFile.ReadBool   ('NodeUtils', 'IgnoreCase', True);
    SplitMultiLine        := IniFile.ReadBool   ('NodeUtils', 'MultiLine', True);
    SplitNonGreedy        := IniFile.ReadBool   ('NodeUtils', 'NonGreedy', True);
    IncludeSeparator      := IniFile.ReadBool   ('NodeUtils', 'IncludeSeparator', True);
    AddPrefixNumber       := IniFile.ReadBool   ('NodeUtils', 'AddPrefixNumber', False);
    AddSuffixNumber       := IniFile.ReadBool   ('NodeUtils', 'AddSuffixNumber', False);
    PrefixNumberLength    := iniFile.ReadInteger('NodeUtils', 'PrefixNumberLength', 3);
    SuffixNumberLength    := iniFile.ReadInteger('NodeUtils', 'SuffixNumberLength', 3);

    RecentSeparator.Clear;
    for i := 1 to 10 do begin
      Recent              := IniFile.ReadString ('NodeUtils', 'RecentSeparator' + IntToStr(i), '');
      if Recent <> '' then RecentSeparator.Add(Recent);
    end;

    RecentTitle.Clear;
    for i := 1 to 10 do begin
      Recent              := IniFile.ReadString ('NodeUtils', 'RecentTitle' + IntToStr(i), '');
      if Recent <> '' then RecentTitle.Add(Recent);
    end;

    SortDirection         := iniFile.ReadInteger('NodeUtils', 'SortDirection', 0);
    SortOf                := iniFile.ReadInteger('NodeUtils', 'SortOf', 1);

    // 文本工具
    KeepTextUtilsFormRect := IniFile.ReadBool   ('TextUtils', 'KeepTextUtilsFormRect', True);

    TextUtilsFormRect.Left   := IniFile.ReadInteger('TextUtils', 'Left', (Screen.Width - DefTextUtilsFormWidth) div 2);
    TextUtilsFormRect.Top    := IniFile.ReadInteger('TextUtils', 'Top', (Screen.Width - DefTextUtilsFormHeight) div 2);
    TextUtilsFormRect.Width  := IniFile.ReadInteger('TextUtils', 'Width', DefTextUtilsFormWidth);
    TextUtilsFormRect.Height := IniFile.ReadInteger('TextUtils', 'Height', DefTextUtilsFormHeight);

    SearchInSelection     := IniFile.ReadBool   ('TextUtils', 'ReplaceInSelection', True);
    ScriptListBarWidth    := IniFile.ReadInteger('TextUtils', 'ScriptListBarWidth', 180);
    LastScriptID          := IniFile.ReadInteger('TextUtils', 'LastScriptID', 0);

    // 选项
    KeepOptionsFormRect   := IniFile.ReadBool   ('Options', 'KeepOptionsFormRect', True);

    OptionsFormRect.Left  := iniFile.ReadInteger('Options', 'Left', (Screen.Width - DefOptionsFormWidth) div 2);
    OptionsFormRect.Top   := iniFile.ReadInteger('Options', 'Top', (Screen.Height - DefOptionsFormHeight) div 2);
    OptionsFormRect.Width := iniFile.ReadInteger('Options', 'Width', DefOptionsFormWidth);
    OptionsFormRect.Height:= iniFile.ReadInteger('Options', 'Height', DefOptionsFormHeight);

    LoadLastFile          := IniFile.ReadBool   ('Options', 'LoadLastFile', True);
    LastFile              := IniFile.ReadString ('Options', 'LastFile', '');

    SelectLastNode        := IniFile.ReadBool   ('Options', 'SelectLastNode', True);

    // 选项 - 自动保存
    AutoSaveInterval      := IniFile.ReadInteger('Options', 'AutoSaveInterval', DefAutoSaveInterval);

    // 选项 - 自动备份
    AutoBackupInterval    := IniFile.ReadInteger('Options', 'AutoBackupInterval', DefAutoBackupInterval);
    AutoBackupCount       := IniFile.ReadInteger('Options', 'AutoBackupCount', DefAutoBackupCount);

    // 选项 - 历史记录
    HistoryMaxSize        := IniFile.ReadInteger('Options', 'HistoryMaxSize', DefHistoryMaxSize);
    HistoryMinCount       := IniFile.ReadInteger('Options', 'HistoryMinCount', DefHistoryMinCount);
    KeepNodesHistory      := IniFile.ReadBool   ('Options', 'KeepNodesHistory', True);

    if Screen.Fonts.IndexOf(TreeBarFontName) = -1 then TreeBarFontName := 'default';
    if Screen.Fonts.IndexOf(NoteBarFontName) = -1 then NoteBarFontName := 'default';
    if Screen.Fonts.IndexOf(InfoBarFontName) = -1 then InfoBarFontName := 'default';
  finally
    IniFile.Free;
  end;
end;

procedure TConfig.Save;
var
  i: integer;
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FConfigFile);
  try
    // 主窗口
    IniFile.WriteBool   ('Main', 'KeepMainFormRect', KeepMainFormRect);

    IniFile.WriteInteger('Main', 'Left', MainFormRect.Left);
    IniFile.WriteInteger('Main', 'Top', MainFormRect.Top);
    IniFile.WriteInteger('Main', 'Width', MainFormRect.Width);
    IniFile.WriteInteger('Main', 'Height', MainFormRect.Height);

    IniFile.WriteBool   ('Main', 'MenuBarVisible', MenuBarVisible);
    IniFile.WriteBool   ('Main', 'ToolBarVisible', ToolBarVisible);
    IniFile.WriteBool   ('Main', 'StatBarVisible', StatBarVisible);
    IniFile.WriteBool   ('Main', 'TreeBarVisible', TreeBarVisible);
    IniFile.WriteBool   ('Main', 'InfoBarVisible', InfoBarVisible);
    IniFile.WriteBool   ('Main', 'RecyBarVisible', RecyBarVisible);

    IniFile.WriteInteger('Main', 'TreeBarWidth', TreeBarWidth);
    IniFile.WriteInteger('Main', 'InfoBarHeight', InfoBarHeight);
    IniFile.WriteInteger('Main', 'RecyBarHeight', RecyBarHeight);

    IniFile.WriteBool   ('Main', 'Maximized', Maximized);
    IniFile.WriteBool   ('Main', 'FullScreen', FullScreen);

    IniFile.WriteBool   ('Main', 'WordWrap', WordWrap);

    for i := 1 to 10 do
    begin
      if RecentFiles.Count >= i then
        IniFile.WriteString('Main', 'RecentFile' + IntToStr(i), '"' + RecentFiles[i - 1] + '"')
      else
        IniFile.WriteString('Main', 'RecentFile' + IntToStr(i), '');
    end;

    // 界面
    IniFile.WriteBool   ('Layout', 'FullWindowHideMenuBar', FWHideMenuBar);
    IniFile.WriteBool   ('Layout', 'FullWindowHideToolBar', FWHideToolBar);
    IniFile.WriteBool   ('Layout', 'FullWindowHideStatBar', FWHideStatBar);
    IniFile.WriteBool   ('Layout', 'FullWindowHideTreeBar', FWHideTreeBar);
    IniFile.WriteBool   ('Layout', 'FullWindowHideInfoBar', FWHideInfoBar);
    IniFile.WriteBool   ('Layout', 'FullWindowHideRecyBar', FWHideRecyBar);

    IniFile.WriteBool   ('Layout', 'TreeBarAutoSize', TreeBarAutoSize);
    IniFile.WriteBool   ('Layout', 'InfoBarAutoSize', InfoBarAutoSize);
    IniFile.WriteBool   ('Layout', 'RecyBarAutoSize', RecyBarAutoSize);

    IniFile.WriteInteger('Layout', 'TreeBarPercent', TreeBarPercent);
    IniFile.WriteInteger('Layout', 'InfoBarPercent', InfoBarPercent);
    IniFile.WriteInteger('Layout', 'RecyBarPercent', RecyBarPercent);

    IniFile.WriteBool   ('Layout', 'SwapOKCancel', SwapOKCancel);
    IniFile.WriteBool   ('Layout', 'RemoveMenuBarItem', RemoveMenuBarItem);


    // 主题
    IniFile.WriteInteger('Theme', 'ActiveTheme', ActiveTheme);

    IniFile.WriteInteger('Theme', 'BrightFontColor', BrightFontColor);
    IniFile.WriteInteger('Theme', 'BrightBackColor', BrightBackColor);

    IniFile.WriteInteger('Theme', 'DarkFontColor', DarkFontColor);
    IniFile.WriteInteger('Theme', 'DarkBackColor', DarkBackColor);

    if WindowFontName  = 'default' then WindowFontName := '';
    if TreeBarFontName = 'default' then TreeBarFontName := '';
    if NoteBarFontName = 'default' then NoteBarFontName := '';
    if InfoBarFontName = 'default' then InfoBarFontName := '';

    IniFile.WriteString ('Theme', 'WindowFontName', '"' + WindowFontName + '"');
    IniFile.WriteString ('Theme', 'TreeBarFontName', '"' + TreeBarFontName + '"');
    IniFile.WriteString ('Theme', 'NoteBarFontName', '"' + NoteBarFontName + '"');
    IniFile.WriteString ('Theme', 'InfoBarFontName', '"' + InfoBarFontName + '"');

    IniFile.WriteInteger('Theme', 'WindowFontSize', WindowFontSize);
    IniFile.WriteInteger('Theme', 'TreeBarFontSize', TreeBarFontSize);
    IniFile.WriteInteger('Theme', 'NoteBarFontSize', NoteBarFontSize);
    IniFile.WriteInteger('Theme', 'InfoBarFontSize', InfoBarFontSize);

    IniFile.WriteString ('Theme', 'Language', '"' + Language + '"');

    // 搜索
    IniFile.WriteBool   ('Search', 'KeepSearchFormRect', KeepSearchFormRect);

    iniFile.WriteInteger('Search', 'Left', SearchFormRect.Left);
    iniFile.WriteInteger('Search', 'Top', SearchFormRect.Top);
    iniFile.WriteInteger('Search', 'Width', SearchFormRect.Width);
    iniFile.WriteInteger('Search', 'Height', SearchFormRect.Height);

    IniFile.WriteInteger('Search', 'SearchFrom', SearchFrom);
    IniFile.WriteBool   ('Search', 'DoReplace', DoReplace);
    IniFile.WriteBool   ('Search', 'SearchName', SearchName);
    IniFile.WriteBool   ('Search', 'SearchNote', SearchNote);
    IniFile.WriteBool   ('Search', 'IgnoreCase', SearchIgnoreCase);
    IniFile.WriteBool   ('Search', 'UseRegExpr', UseRegExpr);
    IniFile.WriteBool   ('Search', 'MultiLine', SearchMultiLine);
    IniFile.WriteBool   ('Search', 'NonGreedy', SearchNonGreedy);

    IniFile.WriteString ('Search', 'SearchText', '"' + SearchText + '"');
    IniFile.WriteString ('Search', 'ReplaceText', '"' + ReplaceText + '"');

    for i := 1 to 10 do
    begin
      if RecentSearch.Count >= i then
        IniFile.WriteString('Search', 'RecentSearch' + IntToStr(i), '"' + RecentSearch[i - 1] + '"')
      else
        IniFile.WriteString('Search', 'RecentSearch' + IntToStr(i), '');
    end;

    for i := 1 to 10 do
    begin
      if RecentReplace.Count >= i then
        IniFile.WriteString('Search', 'RecentReplace' + IntToStr(i), '"' + RecentReplace[i - 1] + '"')
      else
        IniFile.WriteString('Search', 'RecentReplace' + IntToStr(i), '');
    end;

    // 导入
    IniFile.WriteBool   ('Import', 'KeepImportFormRect', KeepImportFormRect);

    iniFile.WriteInteger('Import', 'Left', ImportFormRect.Left);
    iniFile.WriteInteger('Import', 'Top', ImportFormRect.Top);
    iniFile.WriteInteger('Import', 'Width', ImportFormRect.Width);
    iniFile.WriteInteger('Import', 'Height', ImportFormRect.Height);

    IniFile.WriteInteger('Import', 'ImportFrom', ImportFrom);
    IniFile.WriteInteger('Import', 'ImportMode', ImportMode);

    IniFile.WriteBool   ('Import', 'IncludeFileExt', ImportFileExt);
    IniFile.WriteBool   ('Import', 'IncludeRootDir', ImportRootDir);

    IniFile.WriteString ('Import', 'LastImportDir', '"' + LastImportDir + '"');

    // 导出
    IniFile.WriteBool   ('Export', 'KeepExportFormRect', KeepExportFormRect);

    iniFile.WriteInteger('Export', 'Left', ExportFormRect.Left);
    iniFile.WriteInteger('Export', 'Top', ExportFormRect.Top);
    iniFile.WriteInteger('Export', 'Width', ExportFormRect.Width);
    iniFile.WriteInteger('Export', 'Height', ExportFormRect.Height);

    IniFile.WriteInteger('Export', 'ExportTo', ExportTo);
    IniFile.WriteInteger('Export', 'ExportMode', ExportMode);

    IniFile.WriteBool   ('Export', 'AddFileExt', ExportAddFileExt);
    IniFile.WriteString ('Export', 'FileExt', '"' + ExportFileExt + '"');

    IniFile.WriteBool   ('Export', 'AddSeparator', ExportAddSeparator);
    IniFile.WriteString ('Export', 'Separator', '"' + ExportSeparator + '"');

    IniFile.WriteString ('Export', 'LastExportDir', '"' + LastExportDir + '"');

    // 节点工具
    IniFile.WriteBool   ('NodeUtils', 'KeepNodeUtilsFormRect', KeepNodeUtilsFormRect);

    IniFile.WriteInteger('NodeUtils', 'Left', NodeUtilsFormRect.Left);
    IniFile.WriteInteger('NodeUtils', 'Top', NodeUtilsFormRect.Top);
    IniFile.WriteInteger('NodeUtils', 'Width', NodeUtilsFormRect.Width);
    IniFile.WriteInteger('NodeUtils', 'Height', NodeUtilsFormRect.Height);

    IniFile.WriteString ('NodeUtils', 'SeparatorText', '"' + SeparatorText + '"');
    IniFile.WriteString ('NodeUtils', 'TitleText', '"' + TitleText + '"');

    for i := 1 to 10 do
    begin
      if RecentSeparator.Count >= i then
        IniFile.WriteString('NodeUtils', 'RecentSeparator' + IntToStr(i), '"' + RecentSeparator[i - 1] + '"')
      else
        IniFile.WriteString('NodeUtils', 'RecentSeparator' + IntToStr(i), '');
    end;

    for i := 1 to 10 do
    begin
      if RecentTitle.Count >= i then
        IniFile.WriteString('NodeUtils', 'RecentTitle' + IntToStr(i), '"' + RecentTitle[i - 1] + '"')
      else
        IniFile.WriteString('NodeUtils', 'RecentTitle' + IntToStr(i), '');
    end;

    IniFile.WriteBool   ('NodeUtils', 'IgnoreCase', SplitIgnoreCase);
    IniFile.WriteBool   ('NodeUtils', 'MultiLine', SplitMultiLine);
    IniFile.WriteBool   ('NodeUtils', 'NonGreedy', SplitNonGreedy);

    IniFile.WriteBool   ('NodeUtils', 'IncludeSeparator', IncludeSeparator);
    IniFile.WriteBool   ('NodeUtils', 'AddPrefixNumber', AddPrefixNumber);
    IniFile.WriteBool   ('NodeUtils', 'AddSuffixNumber', AddSuffixNumber);
    iniFile.WriteInteger('NodeUtils', 'PrefixNumberLength', PrefixNumberLength);
    iniFile.WriteInteger('NodeUtils', 'SuffixNumberLength', SuffixNumberLength);

    iniFile.WriteInteger('NodeUtils', 'SortDirection', SortDirection);
    iniFile.WriteInteger('NodeUtils', 'SortOf', SortOf);

    // 文本工具
    IniFile.WriteBool   ('TextUtils', 'KeepTextUtilsFormRect', KeepTextUtilsFormRect);

    IniFile.WriteInteger('TextUtils', 'Left', TextUtilsFormRect.Left);
    IniFile.WriteInteger('TextUtils', 'Top', TextUtilsFormRect.Top);
    IniFile.WriteInteger('TextUtils', 'Width', TextUtilsFormRect.Width);
    IniFile.WriteInteger('TextUtils', 'Height', TextUtilsFormRect.Height);

    IniFile.WriteBool   ('TextUtils', 'ReplaceInSelection', SearchInSelection);
    IniFile.WriteInteger('TextUtils', 'ScriptListBarWidth', ScriptListBarWidth);
    IniFile.WriteInteger('TextUtils', 'LastScriptID', LastScriptID);

    // 选项
    IniFile.WriteBool   ('Options', 'KeepOptionsFormRect', KeepOptionsFormRect);

    iniFile.WriteInteger('Options', 'Left', OptionsFormRect.Left);
    iniFile.WriteInteger('Options', 'Top', OptionsFormRect.Top);
    iniFile.WriteInteger('Options', 'Width', OptionsFormRect.Width);
    iniFile.WriteInteger('Options', 'Height', OptionsFormRect.Height);

    IniFile.WriteBool   ('Options', 'LoadLastFile', LoadLastFile);
    IniFile.WriteString ('Options', 'LastFile', '"' + LastFile + '"');

    IniFile.WriteBool   ('Options', 'SelectLastNode', SelectLastNode);

    // 选项 - 自动保存
    IniFile.WriteInteger('Options', 'AutoSaveInterval', AutoSaveInterval);

    // 选项 - 自动备份
    IniFile.WriteInteger('Options', 'AutoBackupInterval', AutoBackupInterval);
    IniFile.WriteInteger('Options', 'AutoBackupCount', AutoBackupCount);

    // 选项 - 历史记录
    IniFile.WriteInteger('Options', 'HistoryMaxSize', HistoryMaxSize);
    IniFile.WriteInteger('Options', 'HistoryMinCount', HistoryMinCount);
    IniFile.WriteBool   ('Options', 'KeepNodesHistory', KeepNodesHistory);
  finally
    IniFile.Free;
  end;
end;

initialization

AppDir  := ExtractFileDir(ParamStrUTF8(0));
AppName := ChangeFileExt(ExtractFileName(ParamStrUTF8(0)), '');

end.
