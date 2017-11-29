unit ftextutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ComCtrls, ExtCtrls, LCLType, LCLTranslator, Controls;

type

  { TformTextUtils }

  TformTextUtils = class(TForm)
    pgctMain              : TPageControl;
    tabsScript            : TTabSheet;
    pnlScript             : TPanel;
    lstbScriptList        : TListBox;
    spltScript            : TSplitter;
    memoScript            : TMemo;
    editScriptName        : TEdit;
    bttnAddScript         : TButton;
    bttnDeleteScript      : TButton;
    bttnModifyScript      : TButton;
    chkbSearchInSelection : TCheckBox;

    bttnOK                : TButton;
    bttnCancel            : TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure bttnOKClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);

    procedure lstbScriptListSelectionChange(Sender: TObject; User: boolean);
    procedure editScriptNameKeyPress(Sender: TObject; var Key: char);
    procedure bttnAddScriptClick(Sender: TObject);
    procedure bttnDeleteScriptClick(Sender: TObject);
    procedure bttnModifyScriptClick(Sender: TObject);
  private
    FLastScriptName : string;
    FScripts        : TStringList;
    procedure LoadScrips;
    procedure SaveScripts;
    procedure OKEvent;
    procedure CancelEvent;
  public

  end;

var
  formTextUtils: TformTextUtils;

resourcestring
  Res_DelItemWarning = 'The Script will not be restored after deleting, Are you sure to delete the Script?';
  Res_NoSelection    = 'No text is currently selected. Do you want the Replace operation performed on the entire document?';

implementation

uses
  fmain, uconfig, ucommon;

const
  ScriptFile = 'script.ini';

{$R *.lfm}

{ TformTextUtils }

procedure TformTextUtils.FormCreate(Sender: TObject);
begin
  FScripts := TStringList.Create;

  // 初始化窗口状态
  if Config.KeepTextUtilsFormRect then begin
    BoundsRect := Config.TextUtilsFormRect;
    AutoSize   := False;
  end;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 初始化控件状态
  pgctMain.ActivePageIndex := 0;

  LoadScrips;

  chkbSearchInSelection.Checked := Config.SearchInSelection;
  lstbScriptList.Width := Config.ScriptListBarWidth;
  spltScript.Left := pnlScript.Width;

  if Config.SwapOKCancel then begin
    bttnOK.Caption          := Res_CaptionCancel;
    bttnOK.Cancel           := True;

    bttnCancel.Caption      := Res_CaptionExecute;
    // bttnCancel.Default      := True;
  end else begin
    bttnOK.Caption          := Res_CaptionExecute;
    // bttnOK.Default          := True;

    bttnCancel.Caption      := Res_CaptionCancel;
    bttnCancel.Cancel       := True;
  end;
end;

procedure TformTextUtils.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 保存窗口状态
  Config.TextUtilsFormRect  := BoundsRect;

  // 保存控件状态
  Config.SearchInSelection  := chkbSearchInSelection.Checked;
  Config.ScriptListBarWidth := lstbScriptList.Width;

  // 保存脚本
  SaveScripts;

  FScripts.Free;

  CloseAction := caFree;
  formTextUtils := nil;
end;

procedure TformTextUtils.bttnOKClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else OKEvent;
end;

procedure TformTextUtils.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then OKEvent else CancelEvent;
end;

procedure TformTextUtils.OKEvent;
begin
  if chkbSearchInSelection.Checked and (formMain.memoNote.SelLength = 0) and
    (Application.MessageBox(PChar(Res_NoSelection), PChar(AppTitle), MB_YESNO + MB_ICONQUESTION) <> ID_YES) then
      Exit;

  Hide;
  formMain.SubmitNote;
  formMain.ScriptReplace(memoScript.Text, chkbSearchInSelection.Checked);
  Close;
end;

procedure TformTextUtils.CancelEvent;
begin
  Close;
end;

procedure TformTextUtils.bttnAddScriptClick(Sender: TObject);
begin
  if editScriptName.Text = '' then Exit;

  editScriptName.Text := string(editScriptName.Text).Replace(#10, '\n', [rfReplaceAll]);

  if lstbScriptList.Items.IndexOf(editScriptName.Text) = -1 then begin
    lstbScriptList.Items.Add(editScriptName.Text);
    FScripts.Add(editScriptName.Text + '=');
  end;
  lstbScriptList.ItemIndex := lstbScriptList.Items.IndexOf(editScriptName.Text);
  memoScript.ReadOnly := lstbScriptList.ItemIndex = -1;
end;

procedure TformTextUtils.bttnDeleteScriptClick(Sender: TObject);
var
  Index: integer;
begin
  Index := lstbScriptList.ItemIndex;

  if Index = -1 then Exit;

  {
  // hold Shift key to ignore warning
  if (not IsKeyDown(VK_SHIFT)) and (Application.MessageBox(PChar(Res_DelItemWarning), PChar(Caption), MB_YESNO + MB_ICONWARNING) <> ID_YES) then Exit;
  }

  // can't ignore warning
  if Application.MessageBox(PChar(Res_DelItemWarning), PChar(Caption), MB_YESNO + MB_ICONWARNING) <> ID_YES then Exit;

  FScripts.Delete(FScripts.IndexOfName(lstbScriptList.Items[Index]));

  if FLastScriptName = lstbScriptList.Items[Index] then
    FLastScriptName := '';

  lstbScriptList.Items.Delete(Index);

  if lstbScriptList.Count - 1 >= Index then
    lstbScriptList.ItemIndex := Index
  else if lstbScriptList.Count - 1 >= Index - 1 then
    lstbScriptList.ItemIndex := Index - 1;

  if lstbScriptList.ItemIndex = -1 then begin
    memoScript.ReadOnly := True;
    memoScript.Text := '';
  end;
end;

procedure TformTextUtils.bttnModifyScriptClick(Sender: TObject);
begin
  if editScriptName.Text = '' then Exit;
  if lstbScriptList.ItemIndex = -1 then Exit;

  lstbScriptList.Items[lstbScriptList.ItemIndex] := editScriptName.Text;
end;

procedure TformTextUtils.editScriptNameKeyPress(Sender: TObject; var Key: char);
begin
  if Key = Chr(VK_RETURN) then
    bttnModifyScriptClick(Sender);
end;

procedure TformTextUtils.LoadScrips;
var
  ConfigFile: string;
  Strs: TStringList;
  i: Integer;
  Line: string;
  Title, Content: string;
begin
  Line := '';
  Title := '';
  Content := '';
  ConfigFile := ConcatPaths([formMain.ConfigDir, ScriptFile]);

  Strs := TStringList.Create;
  try
    if FileExists(ConfigFile) then Strs.LoadFromFile(ConfigFile);

    for i := 0 to Strs.Count - 1 do begin
      Line := Strs[i];
      if (Line <> '') and (Line[1] = '[') then begin
        if Title <> '' then begin
          FScripts.Add(Title + '=' + Content);
          lstbScriptList.Items.Add(Title);
          Content := '';
        end;
        Title := Copy(Line, 2, Length(Line) - 2);
      end else if Content = '' then
        Content := Line
      else
        Content := Content + #10 + Line;
    end;

    if Title <> '' then begin
      FScripts.Add(Title + '=' + Content);
      lstbScriptList.Items.Add(Title);
    end;
  finally
    Strs.Free;
  end;
  if lstbScriptList.Count > Config.LastScriptID then
    lstbScriptList.ItemIndex := Config.LastScriptID
  else if lstbScriptList.Count > 0 then
    lstbScriptList.ItemIndex := 0;

  memoScript.ReadOnly := lstbScriptList.ItemIndex = -1;
end;

procedure TformTextUtils.SaveScripts;
var
  ConfigFile: string;
  Strs: TStringList;
  i: integer;
begin
  ConfigFile := ConcatPaths([formMain.ConfigDir, ScriptFile]);

  if FScripts.Count = 0 then begin
    if FileExists(ConfigFile) then DeleteFile(ConfigFile);
    Exit;
  end;

  lstbScriptListSelectionChange(lstbScriptList, False);

  Strs := TStringList.Create;
  try
    for i := 0 to FScripts.Count - 1 do begin
      Strs.Add('[' + FScripts.Names[i] + ']');
      Strs.Add(FScripts.ValueFromIndex[i]);
    end;
    Strs.SaveToFile(ConfigFile);
  finally
    Strs.Free;
  end;

  Config.LastScriptID := lstbScriptList.ItemIndex;
end;

procedure TformTextUtils.lstbScriptListSelectionChange(Sender: TObject; User: boolean);
begin
  memoScript.ReadOnly := lstbScriptList.ItemIndex = -1;
  if FLastScriptName <> '' then
    FScripts.Values[FLastScriptName] := memoScript.Text;

  FLastScriptName := lstbScriptList.Items[lstbScriptList.ItemIndex];

  if FLastScriptName <> '' then
    memoScript.Text := FScripts.Values[FLastScriptName];
end;

end.

