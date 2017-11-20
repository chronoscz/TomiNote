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
    chkbMultiLine      : TCheckBox;
    chkbUseRegexpr     : TCheckBox;
    chkbNonGreedy      : TCheckBox;
    chkbIgnoreCase     : TCheckBox;

    bttnSearch         : TButton;
    bttnCancel         : TButton;
    lablSpace          : TLabel;

    procedure chkbUseRegexprChange(Sender: TObject);
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
  fmain, uconfig, ucommon;

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
  chkbIgnoreCase.Caption    := Res_IgnoreCase;
  chkbMultiLine.Caption     := Res_MultiLine;
  chkbNonGreedy.Caption     := Res_NonGreedy;

  // 初始化控件状态
  radgSearchFrom.ItemIndex   := Config.SearchFrom;

  combSearch.Items    := Config.RecentSearch;
  combSearch.Text     := Config.SearchText;

  combReplace.Items   := Config.RecentReplace;
  combReplace.Text    := Config.ReplaceText;
  combReplace.Enabled := chkbDoReplace.Checked;

  chkbSearchName.Checked     := Config.SearchName;
  chkbSearchNote.Checked     := Config.SearchNote;
  chkbIgnoreCase.Checked     := Config.SearchIgnoreCase;
  chkbUseRegexpr.Checked     := True; // 用来保证 chkbMultiLine 和 chkbNonGreedy 的状态正常
  chkbUseRegexpr.Checked     := Config.UseRegExpr;
  chkbMultiLine.Checked      := Config.SearchMultiLine;
  chkbNonGreedy.Checked      := Config.SearchNonGreedy;

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
  Config.SearchFormRect      := BoundsRect;

  // 保存控件状态
  Config.SearchFrom          := radgSearchFrom.ItemIndex;

  Config.DoReplace           := chkbDoReplace.Checked;
  Config.SearchName          := chkbSearchName.Checked;
  Config.SearchNote          := chkbSearchNote.Checked;
  Config.SearchIgnoreCase    := chkbIgnoreCase.Checked;
  Config.UseRegExpr          := chkbUseRegexpr.Checked;
  Config.SearchMultiLine     := chkbMultiLine.Checked;
  Config.SearchNonGreedy     := chkbNonGreedy.Checked;

  Config.SearchText          := Escape(combSearch.Text);
  Config.ReplaceText         := Escape(combReplace.Text);

  CloseAction := caFree;
  formSearch  := nil;
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
  IgnoreCase: boolean;
  MultiLine: boolean;
  NonGreedy: boolean;
  HeadStr: string;
  SearchStr: string;
begin
  if (radgSearchFrom.ItemIndex <> 0) and chkbDoReplace.Checked and
    (Application.MessageBox(PChar(Res_SearchAllWarning), PChar(Caption), MB_YESNO + MB_ICONWARNING) <> IDYES) then
    Exit;

  IgnoreCase := chkbIgnoreCase.Checked;
  MultiLine  := chkbMultiLine.Enabled and chkbMultiLine.Checked;
  NonGreedy  := chkbNonGreedy.Enabled and chkbNonGreedy.Checked;

  Hide;

  formMain.SubmitNote;

  // 保存搜索内容
  if combSearch.Text <> '' then begin
    combSearch.Text := Escape(combSearch.Text);
    Index := Config.RecentSearch.IndexOf(combSearch.Text);
    if Index >= 0 then
      Config.RecentSearch.Delete(Index)
    else while Config.RecentSearch.Count >= 10 do
      Config.RecentSearch.Delete(Config.RecentSearch.Count - 1);

    Config.RecentSearch.Insert(0, combSearch.Text);
  end;

  // 保存替换内容
  if (combReplace.Text <> '') then begin
    combReplace.Text := Escape(combReplace.Text);
    Index := Config.RecentReplace.IndexOf(combReplace.Text);
    if Index >= 0 then
      Config.RecentReplace.Delete(Index)
    else while Config.RecentReplace.Count >= 10 do
      Config.RecentReplace.Delete(Config.RecentReplace.Count - 1);

    Config.RecentReplace.Insert(0, combReplace.Text);
  end;

  SearchStr := combSearch.Text;
  HeadStr := '';
  if chkbUseRegExpr.Checked and (SearchStr <> '') then begin
    if IgnoreCase then HeadStr := HeadStr + '(?i)'   else HeadStr := HeadStr + '(?-i)';
    if MultiLine  then HeadStr := HeadStr + '(?m-s)' else HeadStr := HeadStr + '(?s-m)';
    if NonGreedy  then HeadStr := HeadStr + '(?-g)'  else HeadStr := HeadStr + '(?g)';
    SearchStr := HeadStr + SearchStr;
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
        RegReplace(Node, SearchStr, combReplace.Text, chkbSearchName.Checked, chkbSearchNote.Checked, Depth)
      else
        Replace(Node, SearchStr, combReplace.Text, chkbSearchName.Checked, chkbSearchNote.Checked, Depth, IgnoreCase);
    end
    else
    begin
      if chkbUseRegExpr.Checked then
        RegSearch(Node, SearchStr, chkbSearchName.Checked, chkbSearchNote.Checked, Depth)
      else
        Search(Node, SearchStr, chkbSearchName.Checked, chkbSearchNote.Checked, Depth, IgnoreCase);
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

procedure TformSearch.chkbUseRegexprChange(Sender: TObject);
begin
  chkbMultiLine.Enabled := chkbUseRegexpr.Checked;
  chkbNonGreedy.Enabled := chkbUseRegexpr.Checked;
end;

end.
