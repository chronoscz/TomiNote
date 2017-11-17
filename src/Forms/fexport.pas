unit fexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, ExtCtrls, StdCtrls, Dialogs, LCLTranslator;

type

  { TformExport }

  TformExport = class(TForm)
    radgExportTo     : TRadioGroup;
    radgExportMode   : TRadioGroup;
    chkbAddFileExt   : TCheckBox;
    chkbAddSeparator : TCheckBox;
    editFileExt      : TEdit;
    editSeparator    : TEdit;

    bttnExport       : TButton;
    bttnCancel       : TButton;
    lablSpace        : TLabel;

    svdg1            : TSaveDialog;
    sddg1            : TSelectDirectoryDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure bttnExportClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure CancelEvent;
    procedure ExportEvent;

    procedure radgExportToSelectionChanged(Sender: TObject);
    procedure chkbAddFileExtChange(Sender: TObject);
    procedure chkbAddSeparatorChange(Sender: TObject);
  private
    FOldAddFileExtChecked   : boolean;
    FOldAddSeparatorChecked : boolean;
  public
  end;

var
  formExport: TformExport;

ResourceString
  Res_radgExportToItem = 'Export to file'#10'Export to directory'#10'Export to database';
  Res_radgExportMode   = 'Export Selected Node'#10'Export Selected Branche'#10'Export All Nodes';

implementation

{$R *.lfm}

uses
  fmain, uconfig;

{ TformExport }

procedure TformExport.FormCreate(Sender: TObject);
begin
  // 初始化窗口状态
  if Config.KeepExportFormRect then begin
    BoundsRect := Config.ExportFormRect;
    AutoSize := False;
  end;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 这些资源需要手动载入
  radgExportTo.Items.Text   := Res_radgExportToItem;
  radgExportMode.Items.Text := Res_radgExportMode;

  // 初始化控件状态
  chkbAddFileExt.Checked    := Config.ExportAddFileExt;
  chkbAddSeparator.Checked  := Config.ExportAddSeparator;

  editFileExt.Enabled       := chkbAddFileExt.Checked;
  editFileExt.Text          := Config.ExportFileExt;

  editSeparator.Enabled     := chkbAddSeparator.Checked;
  editSeparator.Text        := Config.ExportSeparator;

  FOldAddFileExtChecked     := chkbAddFileExt.Checked;
  FOldAddSeparatorChecked   := chkbAddSeparator.Checked;

  // 这些代码会改变其它控件的状态，所以放在最后
  radgExportTo.ItemIndex    := Config.ExportTo;
  radgExportMode.ItemIndex  := Config.ExportMode;

  if Config.SwapOKCancel then begin
    bttnExport.Caption      := Res_CaptionCancel;
    bttnExport.Cancel       := True;

    bttnCancel.Caption      := Res_CaptionExport;
    // bttnCancel.Default      := True;
  end else begin
    bttnExport.Caption      := Res_CaptionExport;
    // bttnExport.Default      := True;

    bttnCancel.Caption      := Res_CaptionCancel;
    bttnCancel.Cancel       := True;
  end;
end;

procedure TformExport.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 保存窗口状态
  Config.ExportFormRect     := BoundsRect;

  editFileExt.Text := string(editFileExt.Text).Replace(#10, '\n', [rfReplaceAll]);
  editSeparator.Text := string(editSeparator.Text).Replace(#10, '\n', [rfReplaceAll]);

  // 保存控件状态
  Config.ExportTo           := radgExportTo.ItemIndex;
  Config.ExportMode         := radgExportMode.ItemIndex;
  Config.ExportAddFileExt   := chkbAddFileExt.Checked;
  Config.ExportFileExt      := editFileExt.Text;
  Config.ExportAddSeparator := chkbAddSeparator.Checked;
  Config.ExportSeparator    := editSeparator.Text;

  CloseAction := caFree;
  formExport := nil;
end;

procedure TformExport.bttnExportClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else ExportEvent;
end;

procedure TformExport.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then ExportEvent else CancelEvent;
end;

procedure TformExport.ExportEvent;
var
  ToPath: string;
  Node: TTreeNode;
  Depth: integer;
  FileExt: string;
  Separator: string;
begin
  if formMain.trevTree.Selected = nil then Exit;  // 空树

  formMain.SubmitNote;

  if chkbAddFileExt.Checked then begin
    editFileExt.Text   := string(editFileExt.Text).Replace(#10, '\n', [rfReplaceAll]);
    FileExt := editFileExt.Text;
  end;

  if chkbAddSeparator.Checked then begin
    Separator := editSeparator.Text;
    editSeparator.Text := string(editSeparator.Text).Replace(#10, '\n', [rfReplaceAll]);
  end;

  if Config.LastExportDir = '' then
    Config.LastExportDir := GetInitDir;
  svdg1.InitialDir := Config.LastExportDir;
  sddg1.InitialDir := Config.LastExportDir;

  case radgExportMode.ItemIndex of
    0:
    begin
      Node := formMain.trevTree.Selected;
      Depth := 1;
      svdg1.FileName := formMain.trevTree.Selected.Text;
    end;
    1:
    begin
      Node := formMain.trevTree.Selected;
      Depth := AllDepth;
      svdg1.FileName := formMain.trevTree.Selected.Text;
    end;
    2:
    begin
      Node := nil;
      Depth := AllDepth;
      svdg1.FileName := formMain.DBFileName;
    end;
  end;

  if radgExportTo.ItemIndex = 2 then
    svdg1.FileName := ChangeFileExt(svdg1.FileName, DBFileExt)
  else
    svdg1.FileName := ChangeFileExt(svdg1.FileName, FileExt);

  case radgExportTo.ItemIndex of
    0: if svdg1.Execute then begin
        Hide;
        ToPath := svdg1.FileName;
        formMain.ExportToFile(ToPath, Node, Separator, Depth);
        Config.LastExportDir := svdg1.InitialDir;
        Close;
      end;
    1: if sddg1.Execute then begin
        Hide;
        ToPath := sddg1.FileName;
        formMain.ExportToDir(ToPath, Node, FileExt, Depth);
        Config.LastExportDir := sddg1.InitialDir;
        Close;
      end;
    2: if svdg1.Execute then begin
        Hide;
        ToPath := svdg1.FileName;
        formMain.ExportToDB(ToPath, Node, Depth);
        Config.LastExportDir := svdg1.InitialDir;
        Close;
      end;
  end;
end;

procedure TformExport.CancelEvent;
begin
  Close;
end;

procedure TformExport.radgExportToSelectionChanged(Sender: TObject);
begin
  if radgExportTo.ItemIndex <> 2 then begin
    // 启用 FileExt 选项
    chkbAddFileExt.Checked := FOldAddFileExtChecked;
    chkbAddFileExt.Enabled := True;
  end else if chkbAddFileExt.Enabled then begin
    // 禁用 FileExt 选项
    FOldAddFileExtChecked := chkbAddFileExt.Checked;
    chkbAddFileExt.Checked := False;
    chkbAddFileExt.Enabled := False;
  end;

  if (radgExportTo.ItemIndex = 0) and (radgExportMode.ItemIndex <> 0) then begin
    // 启用 Separator 选项
    chkbAddSeparator.Enabled := True;
    chkbAddSeparator.Checked := FOldAddSeparatorChecked;
  end else if chkbAddSeparator.Enabled then begin
    // 禁用 Separator 选项
    FOldAddSeparatorChecked := chkbAddSeparator.Checked;
    chkbAddSeparator.Enabled := False;
    chkbAddSeparator.Checked := False;
  end;
end;

procedure TformExport.chkbAddFileExtChange(Sender: TObject);
begin
  editFileExt.Enabled := chkbAddFileExt.Checked;
end;

procedure TformExport.chkbAddSeparatorChange(Sender: TObject);
begin
  editSeparator.Enabled := chkbAddSeparator.Checked;
end;

end.
