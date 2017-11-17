unit fsearch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, ExtCtrls, StdCtrls, LCLType, Graphics, LCLTranslator;

type

  { TformSearch }

  TformSearch = class(TForm)
    radgSearchFrom     : TRadioGroup;
    combSearch         : TComboBox;
    chkbDoReplace      : TCheckBox;
    combReplace        : TComboBox;
    chkbSearchName     : TCheckBox;
    chkbSearchNote     : TCheckBox;
    chkbUseRegexpr     : TCheckBox;

    bttnSearch         : TButton;
    bttnCancel         : TButton;
    lablSpace          : TLabel;

    procedure combReplaceCloseUp(Sender: TObject);
    procedure combSearchCloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    procedure bttnSearchClick(Sender: TObject);
    procedure bttnCancelClick(Sender: TObject);
    procedure radgSearchFromSelectionChanged(Sender: TObject);

    procedure SearchEvent;
    procedure CancelEvent;

    procedure chkbDoReplaceChange(Sender: TObject);
    procedure combSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure combReplaceKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  public
  end;

var
  formSearch: TformSearch;


resourcestring
  Res_radgSearchFromItems = 'Search in selected node'#10'Search in selected branch'#10'Search in all nodes';
  Res_ScarchResultTip     = 'Found %d results';
  Res_SearchAllWarning    = 'Replacing multiple nodes will cause the history record to be lost. Do you want to continue?';

implementation

uses
  fmain, uconfig;

{$R *.lfm}

{ TformSearch }

procedure TformSearch.FormCreate(Sender: TObject);
begin
  // 初始化窗口状态
  if Config.KeepSearchFormRect then begin
    BoundsRect := Config.SearchFormRect;
    AutoSize := False;
  end;

  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 此资源需要手动载入
  radgSearchFrom.Items.Text := Res_radgSearchFromItems;

  // 初始化控件状态
  radgSearchFrom.ItemIndex   := Config.SearchFrom;

  combSearch.Items    := Config.RecentSearch;
  combSearch.Text     := Config.SearchText;

  combReplace.Items   := Config.RecentReplace;
  combReplace.Text    := Config.ReplaceText;
  combReplace.Enabled := chkbDoReplace.Checked;

  chkbSearchName.Checked     := Config.SearchName;
  chkbSearchNote.Checked     := Config.SearchNote;
  chkbUseRegexpr.Checked     := Config.UseRegExpr;

  if Config.SwapOKCancel then begin
    bttnSearch.Caption       := Res_CaptionCancel;
    bttnSearch.Cancel        := True;

    bttnCancel.Caption       := Res_CaptionSearch;
    // bttnCancel.Default       := True;
  end else begin
    bttnSearch.Caption       := Res_CaptionSearch;
    // bttnSearch.Default       := True;

    bttnCancel.Caption       := Res_CaptionCancel;
    bttnCancel.Cancel        := True;
  end;

  // 此代码可能会改变按钮的标题，所以放在最后
  chkbDoReplace.Checked      := Config.DoReplace;
end;

procedure TformSearch.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // 保存窗口状态
  Config.SearchFormRect := BoundsRect;

  // 保存控件状态
  Config.SearchFrom  := radgSearchFrom.ItemIndex;

  Config.DoReplace   := chkbDoReplace.Checked;
  Config.SearchName  := chkbSearchName.Checked;
  Config.SearchNote  := chkbSearchNote.Checked;
  Config.UseRegExpr  := chkbUseRegexpr.Checked;

  Config.SearchText  := string(combSearch.Text).Replace(#10, '\n', [rfReplaceAll]);
  Config.ReplaceText := string(combReplace.Text).Replace(#10, '\n', [rfReplaceAll]);

  CloseAction := caFree;
  formSearch := nil;
end;

procedure TformSearch.bttnSearchClick(Sender: TObject);
begin
  if Config.SwapOKCancel then CancelEvent else SearchEvent;
end;

procedure TformSearch.bttnCancelClick(Sender: TObject);
begin
  if Config.SwapOKCancel then SearchEvent else CancelEvent;
end;

procedure TformSearch.SearchEvent;
var
  Index: Integer;
  Node: TTreeNode;
  Depth: Integer;
begin
  if (radgSearchFrom.ItemIndex <> 0) and chkbDoReplace.Checked and
    (Application.MessageBox(PChar(Res_SearchAllWarning), PChar(Caption), MB_YESNO + MB_ICONWARNING) <> IDYES) then
    Exit;

  Hide;

  formMain.SubmitNote;

  // 保存搜索内容
  if combSearch.Text <> '' then begin
    combSearch.Text := string(combSearch.Text).Replace(#10, '\n', [rfReplaceAll]);
    Index := Config.RecentSearch.IndexOf(combSearch.Text);
    if Index >= 0 then
      Config.RecentSearch.Delete(Index)
    else while Config.RecentSearch.Count >= 10 do
      Config.RecentSearch.Delete(Config.RecentSearch.Count - 1);

    Config.RecentSearch.Insert(0, combSearch.Text);
  end;

  // 保存替换内容
  if (combReplace.Text <> '') then begin
    combReplace.Text := string(combReplace.Text).Replace(#10, '\n', [rfReplaceAll]);
    Index := Config.RecentReplace.IndexOf(combReplace.Text);
    if Index >= 0 then
      Config.RecentReplace.Delete(Index)
    else while Config.RecentReplace.Count >= 10 do
      Config.RecentReplace.Delete(Config.RecentReplace.Count - 1);

    Config.RecentReplace.Insert(0, combReplace.Text);
  end;

  // 执行搜索或替换
  with formMain do
  begin
    Node := trevTree.Selected;
    Depth := 0;

    case radgSearchFrom.ItemIndex of
      0: Depth := 1;
      2: Node := nil;
    end;

    if chkbDoReplace.Checked then
    begin
      if chkbUseRegExpr.Checked then
        RegReplace(Node, combSearch.Text, combReplace.Text, chkbSearchName.Checked, chkbSearchNote.Checked, Depth)
      else
        Replace(Node, combSearch.Text, combReplace.Text, chkbSearchName.Checked, chkbSearchNote.Checked, Depth);
    end
    else
    begin
      if chkbUseRegExpr.Checked then
        RegSearch(Node, combSearch.Text, chkbSearchName.Checked, chkbSearchNote.Checked, Depth)
      else
        Search(Node, combSearch.Text, chkbSearchName.Checked, chkbSearchNote.Checked, Depth);
    end;

    Config.InfoBarVisible := True;
    sbarMain.Panels[0].Text := Format(Res_ScarchResultTip, [SearchResult.Count]);
  end;

  Close;
end;

procedure TformSearch.CancelEvent;
begin
  Close;
end;

procedure TformSearch.chkbDoReplaceChange(Sender: TObject);
begin
  combReplace.Enabled := chkbDoReplace.Checked;

  if chkbDoReplace.Checked then begin
    if Config.SwapOKCancel then begin
      bttnCancel.Font.Color := clRed;
      bttnCancel.Caption := Res_CaptionReplace
    end else begin
      bttnSearch.Font.Color := clRed;
      bttnSearch.Caption := Res_CaptionReplace;
    end;
  end else begin
    if Config.SwapOKCancel then begin
      bttnCancel.Font.Color := clDefault;
      bttnCancel.Caption := Res_CaptionSearch;
    end else begin
      bttnSearch.Font.Color := clDefault;
      bttnSearch.Caption := Res_CaptionSearch;
    end;
  end;
end;

procedure TformSearch.radgSearchFromSelectionChanged(Sender: TObject);
begin
  if radgSearchFrom.ItemIndex = 0 then
    radgSearchFrom.Font.Color := clDefault
  else
    radgSearchFrom.Font.Color := clRed;
end;

procedure TformSearch.combSearchKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    if ssCtrl in Shift then
      SearchEvent
    else if combReplace.Enabled then
      combReplace.SetFocus;
end;

procedure TformSearch.combReplaceKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    SearchEvent;
end;

procedure TformSearch.combSearchCloseUp(Sender: TObject);
begin
  if IsKeyDown(VK_SHIFT) and (combSearch.ItemIndex >= 0) then begin
    Config.RecentSearch.Delete(combSearch.ItemIndex);
    combSearch.Items.Delete(combSearch.ItemIndex);
  end;
end;

procedure TformSearch.combReplaceCloseUp(Sender: TObject);
begin
  if IsKeyDown(VK_SHIFT) and (combReplace.ItemIndex >= 0) then begin
    Config.RecentReplace.Delete(combReplace.ItemIndex);
    combReplace.Items.Delete(combReplace.ItemIndex);
  end;
end;

end.
