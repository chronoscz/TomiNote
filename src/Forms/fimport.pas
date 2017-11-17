unit fimport;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, ExtCtrls, StdCtrls, Classes, LCLTranslator;

type

  { TformImport }

  TformImport = class(TForm)
    radgImportFrom      : TRadioGroup;
    radgAttachMode      : TRadioGroup;
    chkbIncludeFileExt  : TCheckBox;
    chkbIncludeEntryDir : TCheckBox;

    bttnImport          : TButton;
    bttnCancel          : TButton;
    lablSpace           : TLabel;

    opdg1               : TOpenDialog;
    sddg1               : TSelectDirectoryDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure bttnImportClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure CancelEvent;
    procedure ImportEvent;

    procedure radgImportFromSelectionChanged(Sender: TObject);
  private
    FOldIncludeFileExtChecked: Boolean;
    FOldIncludeRootDirChecked: Boolean;
  public
  end;

var
  formImport: TformImport;

ResourceString
  Res_radgImportFromItems = 'Import from file'#10'Import from directory'#10'Import from database';
  Res_radgAttachModeItems = 'Import as the next node'#10'Import as the prev node'#10'Import as the first child'#10'Import as the last child';

implementation

uses
  fmain, utreedb, uconfig;

{$R *.lfm}

{ TformImport }

procedure TformImport.FormCreate(Sender: TObject);
begin
  // 初始化窗口状态
  if Config.KeepImportFormRect then begin
    BoundsRect := Config.ImportFormRect;
    AutoSize := False;
  end else
    AutoSize := True;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 这些资源需要手动载入
  radgImportFrom.Items.Text := Res_radgImportFromItems;
  radgAttachMode.Items.Text := Res_radgAttachModeItems;

  // 初始化控件状态
  chkbIncludeFileExt.Checked := Config.ImportFileExt;
  chkbIncludeEntryDir.Checked := Config.ImportRootDir;

  FOldIncludeFileExtChecked  := chkbIncludeFileExt.Checked;
  FOldIncludeRootDirChecked  := chkbIncludeEntryDir.Checked;

  // 这些代码会改变其它控件的状态，所以放在最后
  radgImportFrom.ItemIndex   := Config.ImportFrom;
  radgAttachMode.ItemIndex   := Config.ImportMode;

  if Config.SwapOKCancel then begin
    bttnImport.Caption       := Res_CaptionCancel;
    bttnImport.Cancel        := True;

    bttnCancel.Caption       := Res_CaptionImport;
    bttnCancel.Default       := True;
  end else begin
    bttnImport.Caption       := Res_CaptionImport;
    bttnImport.Default       := True;

    bttnCancel.Caption       := Res_CaptionCancel;
    bttnCancel.Cancel        := True;
  end;

end;

procedure TformImport.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 保存窗口状态
  Config.ImportFormRect      := BoundsRect;

  // 保存控件状态
  Config.ImportFrom          := radgImportFrom.ItemIndex;
  Config.ImportMode          := radgAttachMode.ItemIndex;
  Config.ImportFileExt       := chkbIncludeFileExt.Checked;
  Config.ImportRootDir       := chkbIncludeEntryDir.Checked;

  CloseAction := caFree;
  formImport := nil;
end;

procedure TformImport.bttnImportClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else ImportEvent;
end;

procedure TformImport.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then ImportEvent else CancelEvent;
end;

procedure TformImport.ImportEvent;
var
  FromPath: string;
  Mode: TAttachMode;
begin

  formMain.SubmitNote;

  case radgAttachMode.ItemIndex of
    0: Mode := naInsert;
    1: Mode := naInsertBehind;
    2: Mode := naAddChildFirst;
    3: Mode := naAddChild;
  end;

  if Config.LastImportDir = '' then
    Config.LastImportDir := GetInitDir;
  opdg1.InitialDir := Config.LastImportDir;
  sddg1.InitialDir := Config.LastImportDir;

  case radgImportFrom.ItemIndex of
    0: if opdg1.Execute then begin
      Hide;
      FromPath := opdg1.FileName;
      formMain.ImportFile(FromPath, chkbIncludeFileExt.Checked, mode).Selected := True;
      Config.LastImportDir := opdg1.InitialDir;
      Close;
    end;
    1: if sddg1.Execute then begin
      Hide;
      FromPath := sddg1.FileName;
      formMain.ImportDir(FromPath, chkbIncludeEntryDir.Checked, chkbIncludeFileExt.Checked, Mode).Selected := True;
      Config.LastImportDir := sddg1.InitialDir;
      Close;
    end;
    2: if opdg1.Execute then begin
      Hide;
      FromPath := opdg1.FileName;
      formMain.ImportDB(FromPath, Mode);
      Config.LastImportDir := opdg1.InitialDir;
      Close;
    end;
  end;
end;

procedure TformImport.CancelEvent;
begin
  Close;
end;

procedure TformImport.radgImportFromSelectionChanged(Sender: TObject);
begin
  if radgImportFrom.ItemIndex <> 2 then begin
    // 启用 FileExt 选项
    chkbIncludeFileExt.Checked := FOldIncludeFileExtChecked;
    chkbIncludeFileExt.Enabled := True;
  end else if chkbIncludeFileExt.Enabled then begin
    // 禁用 FileExt 选项
    FOldIncludeFileExtChecked  := chkbIncludeFileExt.Checked;
    chkbIncludeFileExt.Checked := False;
    chkbIncludeFileExt.Enabled := False;
  end;

  if radgImportFrom.ItemIndex = 1 then begin
    // 启用 RootDir 选项
    chkbIncludeEntryDir.Checked := FOldIncludeRootDirChecked;
    chkbIncludeEntryDir.Enabled := True;
  end else if chkbIncludeEntryDir.Enabled then begin
    // 禁用 RootDir 选项
    FOldIncludeRootDirChecked  := chkbIncludeEntryDir.Checked;
    chkbIncludeEntryDir.Checked := False;
    chkbIncludeEntryDir.Enabled := False;
  end;
end;

end.
