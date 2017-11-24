unit fmain;

{.$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Graphics,
  Menus, ActnList, Dialogs, Types, LCLType, lazUTF8, Clipbrd, RegExpr,
  utreedb, uhistory, ucommon, LCLTranslator;

type

  PNodeData = ^TNodeData;

  TNodeData = record
    ID: integer;
    Loaded: boolean;
    Position: integer;
    History: THistory;
  end;

  TDragMode = (dmMove, dmCopy);

  { TformMain }

  TformMain = class(TForm)
    panlLeft              : TPanel;
    panlRight             : TPanel;
    spltMain              : TSplitter;
    spltLeft              : TSplitter;
    spltRight             : TSplitter;
    sbarMain              : TStatusBar;

    opdg1                 : TOpenDialog;
    svdg1                 : TSaveDialog;
    imglMain              : TImageList;
    timrMain              : TTimer;

    memoNote              : TMemo;
    tbtnWordWrap          : TToolButton;
    trevTree              : TTreeView;
    trevRecy              : TTreeView;
    lstbInfo              : TListBox;
    editRename            : TEdit;

    actlMain              : TActionList;
    actnNew               : TAction;
    actnOpen              : TAction;
    actnSave              : TAction;
    actnSaveAs            : TAction;
    actnClose             : TAction;
    actnExit              : TAction;
    actnSearch            : TAction;
    actnOptions           : TAction;
    actnRename            : TAction;
    actnInsert            : TAction;
    actnInsertBehind      : TAction;
    actnAddChildFirst     : TAction;
    actnAddChild          : TAction;
    actnDeleteNode        : TAction;
    actnRecycleNode       : TAction;
    actnRestoreNode       : TAction;
    actnEmptyRecycler     : TAction;
    actnMoveUp            : TAction;
    actnMoveDown          : TAction;
    actnExpand            : TAction;
    actnCollapse          : TAction;
    actnImport            : TAction;
    actnExport            : TAction;
    actnNodeUtils         : TAction;
    actnCut               : TAction;
    actnCopy              : TAction;
    actnPaste             : TAction;
    actnSelectAll         : TAction;
    actnDelete            : TAction;
    actnUndo              : TAction;
    actnRedo              : TAction;
    actnStripTailSpace    : TAction;
    actnTextUtils         : TAction;
    actnToggleMenuBar     : TAction;
    actnToggleToolBar     : TAction;
    actnToggleStatBar     : TAction;
    actnToggleTreeBar     : TAction;
    actnToggleInfoBar     : TAction;
    actnToggleRecyBar     : TAction;
    actnFullScreen        : TAction;
    actnFullWindow        : TAction;
    actnBrightTheme       : TAction;
    actnDarkTheme         : TAction;
    actnToggleTheme       : TAction;
    actnPrevNode          : TAction;
    actnNextNode          : TAction;
    actnWordWrap          : TAction;
    actnHelp              : TAction;
    actnRegExprHelp       : TAction;
    actnAbout             : TAction;

    menuMain              : TMainMenu;
    mmiFile               : TMenuItem;
    mmiNew                : TMenuItem;
    mmiOpen               : TMenuItem;
    mmiRecentFiles        : TMenuItem;
    mmiClearRecentFiles   : TMenuItem;
    mmiSave               : TMenuItem;
    mmiSaveAs             : TMenuItem;
    mmiClose              : TMenuItem;
    mmiExit               : TMenuItem;
    mmiSplitter11         : TMenuItem;
    mmiSplitter12         : TMenuItem;
    mmiEdit               : TMenuItem;
    mmiSearch             : TMenuItem;
    mmiOptions            : TMenuItem;
    mmiSplitter21         : TMenuItem;
    mmiNode               : TMenuItem;
    mmiRename             : TMenuItem;
    mmiInsert             : TMenuItem;
    mmiInsertBehind       : TMenuItem;
    mmiAddChildFirst      : TMenuItem;
    mmiAddChild           : TMenuItem;
    mmiDeleteNode         : TMenuItem;
    mmiRecycleNode        : TMenuItem;
    mmiEmptyRecycler      : TMenuItem;
    mmiMoveUp             : TMenuItem;
    mmiMoveDown           : TMenuItem;
    mmiExpand             : TMenuItem;
    mmiCollapse           : TMenuItem;
    mmiImport             : TMenuItem;
    mmiExport             : TMenuItem;
    mmiNodeUtils          : TMenuItem;
    mmiSplitter31         : TMenuItem;
    mmiSplitter32         : TMenuItem;
    mmiSplitter33         : TMenuItem;
    mmiSplitter34         : TMenuItem;
    mmiSplitter35         : TMenuItem;
    mmiSplitter36         : TMenuItem;
    mmiSplitter37         : TMenuItem;
    mmiNote               : TMenuItem;
    mmiCut                : TMenuItem;
    mmiCopy               : TMenuItem;
    mmiPaste              : TMenuItem;
    mmiSelectAll          : TMenuItem;
    mmiDelete             : TMenuItem;
    mmiUndo               : TMenuItem;
    mmiRedo               : TMenuItem;
    mmiStripTailSpace     : TMenuItem;
    mmiTextUtils          : TMenuItem;
    mmiSplitter41         : TMenuItem;
    mmiSplitter42         : TMenuItem;
    mmiSplitter43         : TMenuItem;
    mmiView               : TMenuItem;
    mmiLayout             : TMenuItem;
    mmiToggleMenuBar      : TMenuItem;
    mmiToggleToolBar      : TMenuItem;
    mmiToggleStatBar      : TMenuItem;
    mmiToggleTreeBar      : TMenuItem;
    mmiToggleInfoBar      : TMenuItem;
    mmiToggleRecyBar      : TMenuItem;
    mmiFullScreen         : TMenuItem;
    mmiFullWindow         : TMenuItem;
    mmiTheme              : TMenuItem;
    mmiBrightTheme        : TMenuItem;
    mmiDarkTheme          : TMenuItem;
    mmiToggleTheme        : TMenuItem;
    mmiSplitter51         : TMenuItem;
    mmiSplitter52         : TMenuItem;
    mmiSplitter53         : TMenuItem;
    mmiPrevNode           : TMenuItem;
    mmiNextNode           : TMenuItem;
    mmiWordWrap           : TMenuItem;
    mmiHelp               : TMenuItem;
    mmiHelpContent        : TMenuItem;
    mmiRegExprHelp        : TMenuItem;
    mmiAbout              : TMenuItem;

    menuTree              : TPopupMenu;
    pmiRename             : TMenuItem;
    pmiInsert             : TMenuItem;
    pmiInsertBehind       : TMenuItem;
    pmiAddChildFirst      : TMenuItem;
    pmiAddChild           : TMenuItem;
    pmiDeleteNode         : TMenuItem;
    pmiRecycleNode        : TMenuItem;
    pmiMoveUp             : TMenuItem;
    pmiMoveDown           : TMenuItem;
    pmiExpand             : TMenuItem;
    pmiCollapse           : TMenuItem;
    pmiImport             : TMenuItem;
    pmiExport             : TMenuItem;
    pmiNodeUtils          : TMenuItem;
    pmiSplitter11         : TMenuItem;
    pmiSplitter12         : TMenuItem;
    pmiSplitter13         : TMenuItem;
    pmiSplitter14         : TMenuItem;
    pmiSplitter15         : TMenuItem;
    pmiSplitter16         : TMenuItem;
    pmiSplitter17         : TMenuItem;

    menuRecy              : TPopupMenu;
    pmiRename2            : TMenuItem;
    pmiResotreNode        : TMenuItem;
    pmiEmptyRecy2         : TMenuItem;

    menuNote              : TPopupMenu;
    pmiCut                : TMenuItem;
    pmiCopy               : TMenuItem;
    pmiPaste              : TMenuItem;
    pmiSelectAll          : TMenuItem;
    pmiDelete             : TMenuItem;
    pmiUndo               : TMenuItem;
    pmiRedo               : TMenuItem;
    pmiStripTailSpace     : TMenuItem;
    pmiTextUtils          : TMenuItem;
    pmiToggleMenuBar      : TMenuItem;
    pmiSplitter31         : TMenuItem;
    pmiSplitter32         : TMenuItem;
    pmiSplitter33         : TMenuItem;

    menuEdit              : TPopupMenu;
    pmiCut2               : TMenuItem;
    pmiCopy2              : TMenuItem;
    pmiPaste2             : TMenuItem;
    pmiSelectAll2         : TMenuItem;
    pmiDelete2            : TMenuItem;
    pmiUndo2              : TMenuItem;
    pmiRedo2              : TMenuItem;
    pmiSplitter41         : TMenuItem;
    pmiSplitter42         : TMenuItem;

    tbarMain              : TToolBar;
    tbtnNew               : TToolButton;
    tbtnOpen              : TToolButton;
    tbtnSave              : TToolButton;
    tbtnSearch            : TToolButton;
    tbtnPrevNode          : TToolButton;
    tbtnNextNode          : TToolButton;
    tbtnUndo              : TToolButton;
    tbtnRedo              : TToolButton;
    tbtnBrightTheme       : TToolButton;
    tbtnDarkTheme         : TToolButton;
    tbtnToggleTreeBar     : TToolButton;
    tbtnToggleInfoBar     : TToolButton;
    tbtnToggleRecyBar     : TToolButton;
    tbtnFullScreen        : TToolButton;
    tbtnSplitter1         : TToolButton;
    tbtnSplitter2         : TToolButton;
    tbtnSplitter3         : TToolButton;
    tbtnSplitter4         : TToolButton;
    tbtnSplitter5         : TToolButton;
    tbtnSplitter6         : TToolButton;

    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);

    procedure memoNoteChange(Sender: TObject);
    procedure mmiClearRecentFilesClick(Sender: TObject);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure actnNewExecute(Sender: TObject);
    procedure actnOpenExecute(Sender: TObject);
    procedure actnSaveExecute(Sender: TObject);
    procedure actnSaveAsExecute(Sender: TObject);
    procedure actnCloseExecute(Sender: TObject);
    procedure actnExitExecute(Sender: TObject);

    procedure actnSearchExecute(Sender: TObject);
    procedure actnOptionsExecute(Sender: TObject);

    procedure actnRenameExecute(Sender: TObject);
    procedure actnInsertExecute(Sender: TObject);
    procedure actnInsertBehindExecute(Sender: TObject);
    procedure actnAddChildFirstExecute(Sender: TObject);
    procedure actnAddChildExecute(Sender: TObject);
    procedure actnDeleteNodeExecute(Sender: TObject);
    procedure actnRecycleNodeExecute(Sender: TObject);
    procedure actnRestoreNodeExecute(Sender: TObject);
    procedure actnEmptyRecyclerExecute(Sender: TObject);
    procedure actnMoveUpExecute(Sender: TObject);
    procedure actnMoveDownExecute(Sender: TObject);
    procedure actnExpandExecute(Sender: TObject);
    procedure actnCollapseExecute(Sender: TObject);
    procedure actnImportExecute(Sender: TObject);
    procedure actnExportExecute(Sender: TObject);
    procedure actnNodeUtilsExecute(Sender: TObject);

    procedure actnCutExecute(Sender: TObject);
    procedure actnCopyExecute(Sender: TObject);
    procedure actnPasteExecute(Sender: TObject);
    procedure actnSelectAllExecute(Sender: TObject);
    procedure actnDeleteExecute(Sender: TObject);
    procedure actnUndoExecute(Sender: TObject);
    procedure actnRedoExecute(Sender: TObject);
    procedure actnStripTailSpaceExecute(Sender: TObject);
    procedure actnTextUtilsExecute(Sender: TObject);
    procedure actnWordWrapExecute(Sender: TObject);
    procedure actnCutUpdate(Sender: TObject);
    procedure actnUndoUpdate(Sender: TObject);
    procedure actnStripTailSpaceUpdate(Sender: TObject);

    procedure actnToggleMenuBarExecute(Sender: TObject);
    procedure actnToggleToolBarExecute(Sender: TObject);
    procedure actnToggleStatBarExecute(Sender: TObject);
    procedure actnToggleTreeBarExecute(Sender: TObject);
    procedure actnToggleInfoBarExecute(Sender: TObject);
    procedure actnToggleRecyBarExecute(Sender: TObject);
    procedure actnFullScreenExecute(Sender: TObject);
    procedure actnFullWindowExecute(Sender: TObject);

    procedure actnBrightThemeExecute(Sender: TObject);
    procedure actnDarkThemeExecute(Sender: TObject);
    procedure actnToggleThemeExecute(Sender: TObject);

    procedure actnPrevNodeExecute(Sender: TObject);
    procedure actnNextNodeExecute(Sender: TObject);

    procedure actnHelpExecute(Sender: TObject);
    procedure actnRegExprHelpExecute(Sender: TObject);
    procedure actnAboutExecute(Sender: TObject);

    procedure trevTreeCustomDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
    procedure trevTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure trevTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure trevTreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure trevTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure trevTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure trevTreeSelectionChanged(Sender: TObject);
    procedure trevTreeEnter(Sender: TObject);

    procedure editRenameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure editRenameExit(Sender: TObject);

    procedure lstbInfoDblClick(Sender: TObject);
    procedure lstbInfoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure timrMainTimer(Sender: TObject);

  private

    FConfigDir        : string;          // 配置文件所在目录
    FLangDir          : string;          // 语言文件所在目录

    FTreeDB           : TTreeDB;         // 数据库管理器
    FDBFullName       : string;          // 数据库完整文件名
    FDBFileName       : string;          // 数据库文件名（去掉扩展名）
    FDBFileDir        : string;          // 数据库所在目录

    FLoading          : boolean;         // 用于避免触发 OnSelectionChanged 和 OnChange 事件
    FNoteChanged      : boolean;         // 用于在切换节点时提交前一个节点的笔记内容
    FLastNode         : TTreeNode;       // 用于在切换节点时提交前一个节点的笔记内容，还有其它用途
    FLastNodeText     : string;          // 用于重命名节点
    FLastTreeView     : TTreeView;       // 最后激活的 TreeView

    FDragMode         : TDragMode;       // 拖拽模式：移动或复制

    FEditHistory      : THistoryManager; // 重命名节点时的历史记录
    FMemoHistory      : THistoryManager; // 笔记内容的历史记录

    FSortParent       : TTreeNode;       // 要排序的节点的父节点
    FSortDesc         : Boolean;         // 标记是否进行倒序排序

    // 修补 GTK2 中 HiddenSelection:=False 无效的 BUG，同时激活编辑框更便于操作
    FFocusNote        : boolean;
    // 修补 GTK2 中 SelText:=Something 不触发 OnChange 事件的 BUG
    FFixSelTextBug    : boolean;

    procedure SaveControlState;
    procedure UpdateDBControlState;
    procedure UpdateTreeControlState;
    procedure UpdateRecyclerState;

    procedure LoadLastFile;
    procedure DBActiveChanged(Sender: TObject);
    procedure DataStateChanged(ADataChanged: Boolean);
    procedure UpdateCaption(ADataChanged: Boolean);
    procedure ShowAutoBackupInfo;

    procedure HistoryChanged(Sender: TObject);
    procedure ShowHistoryInfo;

    procedure LoadNote(ANode: TTreeNode);
    procedure StripTailSpace;
    procedure SetWordWrap(AWrap: Boolean);

    procedure SetTreeBarWidth(AWidth: integer);
    procedure SetRecyBarHeight(AHeight: integer);
    procedure SetInfoBarHeight(AHeight: integer);
    procedure ControlBarAutoSize;
    procedure ToggleMenuBar(AVisible: Boolean);
    procedure ToggleToolBar(AVisible: Boolean);
    procedure ToggleStatBar(AVisible: Boolean);
    procedure ToggleTreeBar(AVisible: Boolean);
    procedure ToggleInfoBar(AVisible: Boolean);
    procedure ToggleRecyBar(AVisible: Boolean);
    procedure FullScreen(AVisible: Boolean);
    procedure FullWindow(AVisible: Boolean);
    procedure RemoveMenuBarItem(ARemove: Boolean);
    procedure SetActiveTheme(AThemeID: Integer);
    procedure SetForeColor(AColor: TColor);
    procedure SetBackColor(AColor: TColor);
    procedure SetTreeBarFontName(AName: string); inline;
    procedure SetNoteBarFontName(AName: string); inline;
    procedure SetInfoBarFontName(AName: string); inline;
    procedure SetTreeBarFontSize(ASize: integer); inline;
    procedure SetNoteBarFontSize(ASize: integer); inline;
    procedure SetInfoBarFontSize(ASize: integer); inline;

    procedure AddRecentFile(AFileName: string);
    procedure DelRecentFile(AFileName: string);
    procedure LoadRecentFiles;
    procedure OpenRecentFile(Sender: TObject);
    function  CreateDB(AFileName: string = ''): boolean;
    function  OpenDB(AFileName: String = ''): boolean;
    function  SaveDBDialog: boolean;
    function  SaveDB: boolean;
    function  SaveDBAs(AFileName: String = ''): boolean;
    function  CloseDBDialog: boolean;
    function  CloseDB(ASave: boolean): boolean;
    function  BackupDB: boolean;

    procedure LoadTree(ATree: TTreeView; AID: integer);
    procedure UnloadTree(ATree: TTreeView);
    procedure LoadTrees;
    procedure UnLoadTrees;
    function  LoadNode(ATree: TTreeView; AID: integer; AToNode: TTreeNode; AMode: TAttachMode): TTreeNode;
    procedure LoadSubNodes(ATree: TTreeView; ANode: TTreeNode; Depth: integer);
    procedure UnLoadNode(ATree: TTreeView; ANode: TTreeNode);
    procedure UnLoadSubNodes(ATree: TTreeView; ANode: TTreeNode);
    function  ReLoadSubNodes(ATree: TTreeView; ANode: TTreeNode): TTreeNode;

    function  AddNode(AToTree: TTreeView; ANodeName, ANodeNote: string; AToNode: TTreeNode; AMode: TAttachMode): TTreeNode;
    function  DeleteNode: TTreeNode;
    function  RecycleNode: TTreeNode;
    function  RestoreNode: TTreeNode;
    procedure EmptyRecycler;
    function  MoveNode(AToTree: TTreeView; AFromNode, AToNode: TTreeNode; AMode: TAttachMode): TTreeNode;
    function  CopyNode(AToTree: TTreeView; AFromNode, AToNode: TTreeNode; AMode: TAttachMode): TTreeNode;     procedure UpDownNode(Up: boolean);
    procedure RenameNode;
    procedure SubmitRename;
    procedure SelectNextNode;
    procedure SelectPrevNode;
    procedure CompareEvent(Sender: TObject; Node1, Node2: TTreeNode; var Compare: Integer);

    function  GetNodeID(ANode: TTreeNode; DefaultID: Integer = RootID): integer;
    procedure SetNodeID(ANode: TTreeNode; AID: integer);
    function  GetNodeLoaded(ANode: TTreeNode): boolean;
    procedure SetNodeLoaded(ANode: TTreeNode; ALoaded: boolean);
    function  GetNodePosition(ANode: TTreeNode): integer;
    procedure SetNodePosition(ANode: TTreeNode; APosition: integer);
    function  GetNodeHistory(ANode: TTreeNode): THistory;
    procedure SetNodeHistory(ANode: TTreeNode; AHistory: THistory);
    procedure CleanNodeData(ANode: TTreeNode);
    function  IDToNode(ID: integer; AutoLoad: boolean): TTreeNode;
    function  SelectNodeByID(ID: integer): TTreeNode;

    procedure LoadSearchResult(UpdateHistory: boolean);
    procedure ReLoadNodeName;

  public

    procedure LoadControlState;
    function  GetTotalHistorySize: integer;
    procedure DiscardHistory(KeepSelected: boolean);
    procedure SubmitNote;
    procedure SortNode(InSibling, Desc: boolean);
    procedure SplitNote(SplitterPattern, TitlePattern: string; IncludeSplitter: boolean; PreNumLen, SufNumLen: integer);
    procedure ScriptReplace(Script: string; InSelection: boolean);

    procedure Search(ANode: TTreeNode; AText: string; IncludeName, IncludeNote: boolean; ADepth: integer; IgnoreCase: boolean);
    procedure Replace(ANode: TTreeNode; AFrom, ATo: string; IncludeName, IncludeNote: boolean; ADepth: integer; IgnoreCase: boolean);
    procedure RegSearch(ANode: TTreeNode; AText: string; IncludeName, IncludeNote: boolean; ADepth: integer);
    procedure RegReplace(ANode: TTreeNode; AFrom, ATo: string; IncludeName, IncludeNote: boolean; ADepth: integer);

    function  ImportFile(FromPath: string; IncludeExt: boolean; mode: TAttachMode): TTreeNode;
    function  ImportDir(FromPath: string; IncludeRoot: boolean; IncludeExt: boolean; mode: TAttachMode): TTreeNode;
    function  ImportDB(FromPath: string; mode: TAttachMode): TTreeNode;
    procedure ExportToFile(ToPath: string; Node: TTreeNode; Splitter: string; Depth: integer);
    procedure ExportToDir(ToPath: string; Node: TTreeNode; Ext: string; Depth: integer);
    function  ExportToDB(ToPath: string; Node: TTreeNode; Depth: integer): boolean;

    property  ConfigDir : string read FConfigDir;
    property  LangDir   : string read FLangDir;
    property  DBFullName: string read FDBFullName;
    property  DBDirName : string read FDBFileDir;
    property  DBFileName: string read FDBFileName;
    property  LastNode  : TTreeNode read FLastNode;

  end;

var
  formMain             : TformMain;
  SearchResult         : TSearchResult = nil;

const
  DefDBName            = 'new';
  DefDBDir             = 'data';
  DefBackupDir         = 'backup';
  DefLanguagesDir      = 'languages';
  DBFileExt            = '.tdb';
  EmptyRecyIcon        = 38;
  FullRecyIcon         = 39;

resourcestring
  Res_CreateDBFail        = 'Fail to create database!';
  Res_OpenDBFail          = 'Fail to open database!';
  Res_SaveDBFail          = 'Fail to save database!';
  Res_CloseDBFail         = 'Fail to close database!';
  Res_BackupDBFail        = 'Fail to backup database!';
  Res_RecentFileNotExists = 'The file doesn''t exists, do you want to remove the Menu Item of this recent file?';
  Res_DataChangedTip      = 'The data has been changed. Do you want to save it?';
  Res_OverwriteFileTip    = 'The file already exists. Do you want to overwrite it?';
  Res_OverwriteFileFail   = 'Fail to overwrite file!';
  Res_DelNodeWarning      = 'The node will not be restored after deleting, Are you sure to delete the node?';
  Res_EmptyRecyWarning    = 'The nodes will not be recovered after empty the recycler. Are you sure to empty the recycler?';
  Res_UnnamedNode         = 'Unnamed Node';
  Res_NodeIDInfo          = 'Node ID: %d';
  Res_HistoryDebug        = 'History [ Index:%d   Count:%d/%d   Size:%d/%d ]';
  Res_AutoBackupInfo      = 'Auto Backup [ %dm | %s ]';
  Res_LangStrings         = 'af=Afrikaans|am=Amharic|ar=Arabic|ar_ae=Arabic(United Arab Emirates)|ar_bh=Arabic(Bahrain)|ar_dz=Arabic(Algeria)|ar_eg=Arabic(Egypt)|ar_iq=Arabic(Iraq)|ar_jo=Arabic(Jordan)|ar_kw=Arabic(Kuwait)|ar_lb=Arabic(Lebanon)|ar_ly=Arabic(Libya)|ar_ma=Arabic(Morocco)|ar_om=Arabic(Oman)|ar_qa=Arabic(Qatar)|ar_sa=Arabic(Saudi Arabia)|ar_sy=Arabic(Syria)|ar_tn=Arabic(Tunisia)|ar_ye=Arabic(Yemen)|as=Assamese|az=Azeri|az_az=Azeri(Cyrillic)|be=Belarusian|bg=Bulgarian|bn=Bengali|bo=Tibetan|bs=Bosnian|ca=Catalan|cs=Czech|cy=Welsh|da=Danish|de=German|de_at=German(Austria)|de_ch=German(Switzerland)|de_de=German(Germany)|de_li=German(Liechtenstein)|de_lu=German(Luxembourg)|dv=Maldivian|el=Greek|en=English|en_au=English(Australia)|en_bz=English(Belize)|en_ca=English(Canada)|en_cb=English(Caribbean)|en_gb=English(Great Britain)|en_ie=English(Ireland)|en_in=English(India)|en_jm=English(Jamaica)|en_nz=English(New Zealand)|en_ph=English(Philippines)|en_tt=English(Trinidad)|en_us=English(United States)|en_za=English(Southern Africa)|es=Spanish|es_ar=Spanish(Argentina)|es_bo=Spanish(Bolivia)|es_cl=Spanish(Chile)|es_co=Spanish(Colombia)|es_cr=Spanish(Costa Rica)|es_do=Spanish(Dominican Republic)|es_ec=Spanish(Ecuador)|es_es=Spanish(Traditional)|es_gt=Spanish(Guatemala)|es_hn=Spanish(Honduras)|es_mx=Spanish(Mexico)|es_ni=Spanish(Nicaragua)|es_pa=Spanish(Panama)|es_pe=Spanish(Peru)|es_pr=Spanish(Puerto Rico)|es_py=Spanish(Paraguay)|es_sv=Spanish(ElSalvador)|es_uy=Spanish(Uruguay)|es_ve=Spanish(Venezuela)|et=Estonian|eu=Basque|fa=Farsi|fi=Finnish|fo=Faroese|fr=French|fr_be=French(Belgium)|fr_ca=French(Canada)|fr_ch=French(Switzerland)|fr_fr=French(France)|fr_lu=French(Luxembourg)|ga=Irish|gd=Gaelic(Scotland)|gd_ie=Gaelic(Ireland)|gl=Galician|gn=Guarani(Paraguay)|gu=Gujarati|he=Hebrew|hi=Hindi|hr=Croatian|hu=Hungarian|hy=Armenian|id=Indonesian|is=Icelandic|it=Italian|it_ch=Italian(Switzerland)|it_it=Italian(Italy)|ja=Japanese|ka=Georgian|kk=Kazakh|km=Khmer|kn=Kannada|ko=Korean|ks=Kashmiri|la=Latin|lo=Lao|lt=Lithuanian|lv=Latvian|mi=Maori|mk=FYRO Macedonia|ml=Malayalam|mn=Mongolian|mr=Marathi|ms=Malay|ms_bn=Malay(Brunei)|ms_my=Malay(Malaysia)|mt=Maltese|my=Burmese|nb=Norwegian(Bokml)|ne=Nepali|nl=Dutch|nl_be=Dutch(Belgium)|nl_nl=Dutch(Netherlands)|no=Norwegian|or=Oriya|pa=Punjabi|pl=Polish|pt=Portuguese|pt_br=Portuguese(Brazil)|pt_pt=Portuguese(Portugal)|rm=Raeto(Romance)|ro=Romanian|ro_mo=Romanian(Moldova)|ru=Russian|ru_mo=Russian(Moldova)|sa=Sanskrit|sb=Sorbian|sd=Sindhi|si=Sinhalese|sk=Slovak|sl=Slovenian|so=Somali|sq=Albanian|sr=Serbian(Latin)|sr_sp=Serbian(Cyrillic)|sv=Swedish|sv_fi=Swedish(Finland)|sv_se=Swedish(Sweden)|sw=Swahili|sz=Sami(lappish)|ta=Tamil|te=Telugu|tg=Tajik|th=Thai|tk=Turkmen|tn=Setswana|tr=Turkish|ts=Tsonga|tt=Tatar|uk=Ukrainian|ur=Urdu|uz=Uzbek(Latin)|uz_uz=Uzbek(Cyrillic)|ve=Venda|vi=Vietnamese|xh=Xhosa|yi=Yiddish|zh=Chinese|zh_cn=Chinese(China)|zh_hk=Chinese(Hong Kong SAR)|zh_mo=Chinese(Macau SAR)|zh_sg=Chinese(Singapore)|zh_tw=Chinese(Taiwan)|zu=Zulu';

  function GetInitDir: string;

implementation

uses
  fsearch, fimport, fexport, fnodeutils, ftextutils, foptions, fhelp, uconfig;

{$R *.lfm}

{ TformMain }

procedure TformMain.FormCreate(Sender: TObject);
begin
  // 初始化字段
  FDBFullName    := '';
  FDBFileDir     := '';
  FDBFileName    := '';
  FLoading       := False;
  FNoteChanged   := False;
  FLastNode      := nil;
  FLastNodeText  := '';
  FDragMode      := dmMove;
  FSortParent    := nil;
  FSortDesc      := False;
  FFocusNote     := False;
  FFixSelTextBug := False;

  FTreeDB        := TTreeDB.Create;
  FTreeDB.OnActiveChanged := @DBActiveChanged;

  if Application.HasOption('l', 'lang') then
    FLangDir := ExpandFileName(Application.GetOptionValue('l', 'lang'))
  else
    FLangDir := ConcatPaths([AppDir, DefLanguagesDir]);

  // TomiNote.ini 和 script.ini 都存放在 FConfigDir 目录中
  if Application.HasOption('c', 'config') then
    FConfigDir := ExpandFileName(Application.GetOptionValue('c', 'config'))
  else
    FConfigDir := AppDir;
  ForceDirectories(FConfigDir);
  Config         := TConfig.Create(ConcatPaths([FConfigDir, AppName + '.ini']));

  FEditHistory   := THistoryManager.Create(editRename);
  FEditHistory.CreateHistory('');

  FMemoHistory   := THistoryManager.Create(memoNote, Config.HistoryMaxSize, Config.HistoryMinCount);
  FMemoHistory.OnHistoryChanged := @HistoryChanged;

  SearchResult   := TSearchResult.Create;

  // 初始化窗口状态
  if Config.Language <> '' then
    SetDefaultLang(Config.Language, FLangDir);

  actnTextUtils.Caption := actnNodeUtils.Caption;

  if Config.KeepMainFormRect then begin
    BoundsRect := Config.MainFormRect;
    if Config.Maximized then
      WindowState := wsMaximized;
    FullScreen(Config.FullScreen);
  end else begin
    Config.MainFormRect.Left   := DefMainFormLeft;
    Config.MainFormRect.Top    := DefMainFormTop;
    Config.MainFormRect.Width  := DefMainFormWidth;
    Config.MainFormRect.Height := DefMainFormHeight;
  end;

  // 初始化控件
  timrMain.Interval := 60 * 1000;
  timrMain.Enabled  := False;

  // 去掉 tvoThemedDraw 选项才能修改背景色
  trevTree.Options  := trevTree.Options - [tvoThemedDraw];
  trevRecy.Options  := trevRecy.Options - [tvoThemedDraw];

  SetWordWrap(Config.WordWrap);

  LoadControlState;

  LoadRecentFiles;
  LoadLastFile;

  UpdateDBControlState;

  UpdateCaption(False);
end;

procedure TformMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  If CloseDBDialog then begin
    SaveControlState;

    FreeAndNil(SearchResult);
    FreeAndNil(FEditHistory);
    FreeAndNil(FMemoHistory);
    FreeAndNil(FTreeDB);
    FreeAndNil(Config);
  end else
    CloseAction := caNone;
end;

procedure TformMain.FormResize(Sender: TObject);
begin
  ControlBarAutoSize;
end;

procedure TformMain.FormWindowStateChange(Sender: TObject);
begin
  if WindowState <> wsNormal then begin
    Config.MainFormRect.Left := restoredLeft;
    Config.MainFormRect.Top := restoredTop;
    Config.MainFormRect.Width := restoredWidth;
    Config.MainFormRect.Height := restoredHeight;
  end;
end;

procedure TformMain.memoNoteChange(Sender: TObject);
begin
  if FFixSelTextBug then
    FFixSelTextBug := False;

  if FLoading then Exit;

  FNoteChanged := True;
  DataStateChanged(True);
end;

procedure TformMain.MouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  Size: integer;
begin
  if editRename.Visible then editRename.Hide;

  if not (ssCtrl in Shift) then Exit;

  if ssShift in Shift then Size := 10 else Size := 1;
  if WheelDelta < 0 then Size := -Size;

  Size := (Sender as TControl).Font.Size + Size;
  if Size <= 0 then Size := 1;

  case (Sender as TControl).Name of
    'memoNote': SetNoteBarFontSize(Size);
    'trevTree': SetTreeBarFontSize(Size);
    'trevRecy': SetTreeBarFontSize(Size);
    'lstbInfo': SetInfoBarFontSize(Size);
  end;
end;

procedure TformMain.actnNewExecute(Sender: TObject);
begin
  CreateDB;
end;

procedure TformMain.actnOpenExecute(Sender: TObject);
begin
  OpenDB;
end;

procedure TformMain.actnSaveExecute(Sender: TObject);
begin
  SaveDB;
end;

procedure TformMain.actnSaveAsExecute(Sender: TObject);
begin
  SaveDBAs;
end;

procedure TformMain.actnCloseExecute(Sender: TObject);
begin
  if CloseDBDialog then Config.LastFile := '';
end;

procedure TformMain.actnExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TformMain.actnSearchExecute(Sender: TObject);
begin
  SubmitNote;
  if formSearch <> nil then
    formSearch.BringToFront
  else begin
    Application.CreateForm(TformSearch, formSearch);
    formSearch.Show;
  end;
end;

procedure TformMain.actnOptionsExecute(Sender: TObject);
begin
  Application.CreateForm(TformOptions, formOptions);
  formOptions.ShowModal;
end;

procedure TformMain.actnRenameExecute(Sender: TObject);
begin
  RenameNode;
end;

procedure TformMain.actnInsertExecute(Sender: TObject);
begin
  SubmitRename;
  AddNode(trevTree, '', '', trevTree.Selected, naInsert).Selected := True;
  RenameNode;
end;

procedure TformMain.actnInsertBehindExecute(Sender: TObject);
begin
  SubmitRename;
  AddNode(trevTree, '', '', trevTree.Selected, naInsertBehind).Selected := True;
  RenameNode;
end;

procedure TformMain.actnAddChildFirstExecute(Sender: TObject);
begin
  SubmitRename;
  AddNode(trevTree, '', '', trevTree.Selected, naAddChildFirst).Selected := True;
  RenameNode;
end;

procedure TformMain.actnAddChildExecute(Sender: TObject);
begin
  SubmitRename;
  AddNode(trevTree, '', '', trevTree.Selected, naAddChild).Selected := True;
  RenameNode;
end;

procedure TformMain.actnDeleteNodeExecute(Sender: TObject);
begin
  DeleteNode;
end;

procedure TformMain.actnRecycleNodeExecute(Sender: TObject);
begin
  RecycleNode;
end;

procedure TformMain.actnRestoreNodeExecute(Sender: TObject);
begin
  RestoreNode;
end;

procedure TformMain.actnEmptyRecyclerExecute(Sender: TObject);
begin
  EmptyRecycler;
end;

procedure TformMain.actnMoveUpExecute(Sender: TObject);
begin
  UpDownNode(True);
end;

procedure TformMain.actnMoveDownExecute(Sender: TObject);
begin
  UpDownNode(False);
end;

procedure TformMain.actnExpandExecute(Sender: TObject);
begin
  if FLastNode <> nil then FLastNode.Expand(True);
end;

procedure TformMain.actnCollapseExecute(Sender: TObject);
var
  ANode: TTreeNode;
begin
  if FLastNode <> nil then begin
    ANode := FLastNode;
    if IsKeyDown(VK_SHIFT) then begin
      while ANode.Parent <> nil do
        ANode := ANode.Parent;
    end;
    ANode.Collapse(True);
    ANode.Selected := True;
  end;
end;

procedure TformMain.actnImportExecute(Sender: TObject);
begin
  SubmitNote;
  if formImport <> nil then
    formImport.BringToFront
  else begin
    Application.CreateForm(TformImport, formImport);
    formImport.Show;
  end;
end;

procedure TformMain.actnExportExecute(Sender: TObject);
begin
  SubmitNote;
  if formExport <> nil then
    formExport.BringToFront
  else begin
    Application.CreateForm(TformExport, formExport);
    formExport.Show;
  end;
end;

procedure TformMain.actnNodeUtilsExecute(Sender: TObject);
begin
  SubmitNote;
  if formNodeUtils <> nil then
    formNodeUtils.BringToFront
  else begin
    Application.CreateForm(TformNodeUtils, formNodeUtils);
    formNodeUtils.Show;
  end;
end;

// 不能使用标准的 TEditCut 动作，因为它会触发 OnChange 事件两次，导致历史记录出错
procedure TformMain.actnCutExecute(Sender: TObject);
begin
  if ActiveControl = editRename then
    FEditHistory.Cut
  else
    FMemoHistory.Cut;
end;

procedure TformMain.actnCopyExecute(Sender: TObject);
begin
  if ActiveControl = editRename then begin
    editRename.CopyToClipboard;
  end else begin
    memoNote.CopyToClipboard;
  end;
end;

// 不能使用标准的 TEditPaste 动作，因为它会触发 OnChange 事件两次，导致历史记录出错
procedure TformMain.actnPasteExecute(Sender: TObject);
begin
  if ActiveControl = editRename then
    FEditHistory.Paste
  else begin
    // 在“编辑框无内容”或“编辑框内容全选”的情况下，执行 SelText:=something 不会
    // 触发 OnChange 事件，所以需要用 FFixSelTextBug 修补
    FFixSelTextBug := True;
    FMemoHistory.Paste;
    if FFixSelTextBug then
      memoNoteChange(memoNote);
  end;
end;

procedure TformMain.actnSelectAllExecute(Sender: TObject);
begin
  if ActiveControl = editRename then begin
    editRename.SelectAll;
  end else begin
    memoNote.SelectAll;
  end;
end;

procedure TformMain.actnDeleteExecute(Sender: TObject);
begin
  // 在 Windows 中，执行 ClearSelection 无法在 OnChange 事件中获得正确的 SelStart
  if ActiveControl = editRename then
    FEditHistory.Delete
  else begin
    // 在 GTK2 中，在“编辑框无内容”或“编辑框内容全选”的情况下，执行
    // SelText:=something 不会触发 OnChange 事件，所以需要用 FFixSelTextBug 修补
    FFixSelTextBug := True;
    FMemoHistory.Delete;
    if FFixSelTextBug then
      memoNoteChange(memoNote);
  end;
end;

procedure TformMain.actnUndoExecute(Sender: TObject);
begin
  if editRename.Visible and FEditHistory.CanUndo then
    FEditHistory.Undo
  else
    FMemoHistory.Undo;
end;

procedure TformMain.actnRedoExecute(Sender: TObject);
begin
  if editRename.Visible and FEditHistory.CanRedo then
    FEditHistory.Redo
  else
    FMemoHistory.Redo;
end;

procedure TformMain.actnStripTailSpaceExecute(Sender: TObject);
begin
  StripTailSpace;
end;

procedure TformMain.actnTextUtilsExecute(Sender: TObject);
begin
  SubmitNote;
  if formTextUtils <> nil then
    formTextUtils.BringToFront
  else begin
    Application.CreateForm(TformTextUtils, formTextUtils);
    formTextUtils.Show;
  end;
end;

procedure TformMain.actnWordWrapExecute(Sender: TObject);
begin
  SetWordWrap(not actnWordWrap.Checked);
end;

procedure TformMain.actnCutUpdate(Sender: TObject);
begin
  if ActiveControl = editRename then begin
    actnCut       .Enabled := formMain.Active and (editRename.SelLength <> 0);
    actnCopy      .Enabled := actnCut.Enabled;
    actnPaste     .Enabled := formMain.Active and Clipboard.HasFormat(CF_TEXT) and not editRename.ReadOnly;
    actnSelectAll .Enabled := formMain.Active and (editRename.Text <> '');
    actnDelete    .Enabled := actnCut.Enabled;
  end else begin
    actnCut       .Enabled := formMain.Active and (memoNote.SelLength <> 0);
    actnCopy      .Enabled := actnCut.Enabled;
    actnPaste     .Enabled := formMain.Active and Clipboard.HasFormat(CF_TEXT) and not memoNote.ReadOnly;
    actnSelectAll .Enabled := formMain.Active and (memoNote.Text <> '');
    actnDelete    .Enabled := actnCut.Enabled;
  end;
end;

procedure TformMain.actnUndoUpdate(Sender: TObject);
begin
  if ActiveControl = editRename then begin
    actnUndo.Enabled := formMain.Active and FEditHistory.CanUndo;
    actnRedo.Enabled := formMain.Active and FEditHistory.CanRedo;
  end else begin
    actnUndo.Enabled := formMain.Active and FMemoHistory.CanUndo;
    actnRedo.Enabled := formMain.Active and FMemoHistory.CanRedo;
  end;
end;

procedure TformMain.actnStripTailSpaceUpdate(Sender: TObject);
begin
  actnStripTailSpace.Enabled := not memoNote.ReadOnly;
  actnTextUtils.Enabled := actnStripTailSpace.Enabled;
end;

procedure TformMain.actnToggleMenuBarExecute(Sender: TObject);
begin
  ToggleMenuBar(not actnToggleMenuBar.Checked);
end;

procedure TformMain.actnToggleToolBarExecute(Sender: TObject);
begin
  ToggleToolBar(not actnToggleToolBar.Checked);
end;

procedure TformMain.actnToggleStatBarExecute(Sender: TObject);
begin
  ToggleStatBar(not actnToggleStatBar.Checked);
end;

procedure TformMain.actnToggleTreeBarExecute(Sender: TObject);
begin
  ToggleTreeBar(not actnToggleTreeBar.Checked);
end;

procedure TformMain.actnToggleInfoBarExecute(Sender: TObject);
begin
  ToggleInfoBar(not actnToggleInfoBar.Checked);
end;

procedure TformMain.actnToggleRecyBarExecute(Sender: TObject);
begin
  ToggleRecyBar(not actnToggleRecyBar.Checked);
end;

procedure TformMain.actnFullScreenExecute(Sender: TObject);
begin
  FullScreen(not actnFullScreen.Checked);
end;

procedure TformMain.actnFullWindowExecute(Sender: TObject);
begin
  if Config.FWHideMenuBar then FullWindow(Menu = nil)
  else if Config.FWHideToolBar then FullWindow(not tbarMain.Visible)
  else if Config.FWHideStatBar then FullWindow(not sbarMain.Visible)
  else if Config.FWHideTreeBar then FullWindow(not panlLeft.Visible)
  else if Config.FWHideInfoBar then FullWindow(not lstbInfo.Visible)
  else if Config.FWHideRecyBar then FullWindow(not trevRecy.Visible);
end;

procedure TformMain.actnBrightThemeExecute(Sender: TObject);
begin
  SetActiveTheme(BrightThemeID);
end;

procedure TformMain.actnDarkThemeExecute(Sender: TObject);
begin
  SetActiveTheme(DarkThemeID);
end;

procedure TformMain.actnToggleThemeExecute(Sender: TObject);
begin
  if Config.ActiveTheme = BrightThemeID then
    SetActiveTheme(DarkThemeID)
  else
    SetActiveTheme(BrightThemeID);
end;

procedure TformMain.actnPrevNodeExecute(Sender: TObject);
begin
  SelectPrevNode;
end;

procedure TformMain.actnNextNodeExecute(Sender: TObject);
begin
  SelectNextNode;
end;

procedure TformMain.actnHelpExecute(Sender: TObject);
begin
  if formHelp <> nil then
    formHelp.BringToFront
  else begin
    Application.CreateForm(TformHelp, formHelp);
    formHelp.Caption := Res_HelpTitle;
    formHelp.memoContent.Text := Res_Help;
    formHelp.Show;
  end;
end;

procedure TformMain.actnRegExprHelpExecute(Sender: TObject);
begin
  if formHelp <> nil then
    formHelp.BringToFront
  else begin
    Application.CreateForm(TformHelp, formHelp);
    formHelp.Caption := Res_HelpTitle;
    formHelp.memoContent.Text := Res_RegExprHelp;
    formHelp.Show;
  end;
end;

procedure TformMain.actnAboutExecute(Sender: TObject);
var
  AVersion: string;
begin
  if formHelp <> nil then
    formHelp.BringToFront
  else begin
    Application.CreateForm(TformHelp, formHelp);
    formHelp.Caption := Res_AboutTitle;
    AVersion := Format('%s %s', [AppTitle, Version]);
    formHelp.memoContent.Text := #10 + AVersion + #10#10 + Res_About;
    formHelp.FormStyle := fsStayOnTop;
    formHelp.Show;
  end;
end;

procedure TformMain.trevTreeCustomDrawArrow(Sender: TCustomTreeView; const ARect: TRect; ACollapsed: Boolean);
begin
  Sender.Canvas.Rectangle(ARect);
  with ARect do
  begin
    Sender.Canvas.Line(Left + 2, Top + Height div 2, Right - 2, Top + Height div 2);
    if ACollapsed then
      Sender.Canvas.Line(Left + Width div 2, Top + 2, Left + Width div 2, Top + Height - 2);
  end;
end;

procedure TformMain.trevTreeMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if ((ssCtrl in Shift) or (ssAlt in Shift)) and (Button = mbLeft) and
    (htOnItem in (Sender as TTreeView).GetHitTestInfoAt(X, Y)) then
  begin
    (Sender as TTreeView).BeginDrag(False);

    if (ssAlt in Shift) and (Sender = trevTree) then
      FDragMode := dmCopy
    else
      FDragMode := dmMove;
  end;
end;

procedure TformMain.trevTreeStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  SubmitNote;
end;

procedure TformMain.trevTreeDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;

  if Sender = trevRecy then
    Accept := Source = trevTree;
end;

procedure TformMain.trevTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  FromNode, ToNode: TTreeNode;
  Mode: TAttachMode;
  IntoChildren: boolean;
begin
  FromNode := (Source as TTreeView).Selected;

  if Sender = trevRecy then begin
    ToNode := nil;
    Mode := naAdd;
  end
  else begin
    ToNode := (Sender as TTreeView).GetNodeAt(X, Y);
    Mode := naInsertBehind;

    // 判断是否移入目标节点的子节点中
    // 如果在 Drop 的时候仍然按住 Ctrl 或 Alt 键不放，则将节点移动到目标节点的兄弟节点中
    // 如果在 Drop 的时候没有按住 Ctrl 或 Alt 键，则将节点移动到目标节点的子节点中
    IntoChildren := not (IsKeyDown(VK_CONTROL) or IsKeyDown(VK_MENU));

    // 对于同级节点，向上移动，则移动到目标之前，向下移动，则移动到目标之后
    if (not IntoChildren) and (ToNode <> nil) and (FromNode.Parent = ToNode.Parent) and
      (FromNode.Index > ToNode.Index) then
      Mode := naInsert;

    // 如果在 Drop 的时候按住 Shift 键不放，则强制将节点移动到目标节点之前
    if IsKeyDown(VK_SHIFT) then
      Mode := naInsert;

    // 如果是移入子节点，则修正移动方式
    if IntoChildren then
      if Mode = naInsert then
        Mode := naAddChildFirst
      else
        Mode := naAddChild;
  end;

  if (Sender = trevTree) and (FDragMode = dmCopy) then
    CopyNode(Sender as TTreeView, FromNode, ToNode, Mode).Selected := True
  else if FromNode <> ToNode then
    MoveNode(Sender as TTreeView, FromNode, ToNode, Mode).Selected := True;
end;

procedure TformMain.trevTreeExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
var
  i: integer;
begin
  if GetNodeLoaded(Node) then Exit;

  for i := 0 to Node.Count - 1 do
    LoadSubNodes(Sender as TTreeView, Node.Items[i], 1);

  SetNodeLoaded(Node, True);
end;

procedure TformMain.trevTreeSelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
begin
  if FLoading then Exit;

  Node := (Sender as TTreeView).Selected;

  if Node <> FLastNode then
  begin
    SubmitNote;
    LoadNote(Node);
  end;

  // debug
  if Node <> nil then
    sbarMain.Panels[2].Text := Format(Res_NodeIDInfo, [GetNodeID(Node)]);

  UpdateTreeControlState;
end;

procedure TformMain.trevTreeEnter(Sender: TObject);
begin
  FLastTreeView := Sender as TTreeView;
  trevTreeSelectionChanged(Sender);
end;

procedure TformMain.editRenameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    editRename.OnExit(Sender);
  end else if Key = VK_ESCAPE then begin
    editRename.Hide;
    FEditHistory.Enabled := False;
    FLastNode.TreeView.SetFocus;
  end;
end;

procedure TformMain.editRenameExit(Sender: TObject);
begin
  // 需要检查重命名框是否可见，否则会出错
  if not editRename.Visible then Exit;
  SubmitRename;
  editRename.Hide;
  FLastNode.TreeView.SetFocus;
  FEditHistory.Enabled := False;
end;

procedure TformMain.lstbInfoDblClick(Sender: TObject);
var
  PSR: PSearchRecord;
  FixNum: Integer;
  Str: string;
begin
  if lstbInfo.ItemIndex = -1 then Exit;

  FFocusNote := True;

  PSR := SearchResult[lstbInfo.ItemIndex];

  SelectNodeByID(PSR^.ID);

  if PSR^.UStart + memoNote.SelLength > 0 then
  begin
    // 本程序使用的默认换行符是 #10，而 Windows 中的 TMemo 使用的换行符
    // 是 #13#10，长度不同，所以需要修补才能得到正确的 SelStart
    FixNum := 0;
    if Length(LineEnding) > 1 then begin
      Str := FTreeDB.GetNote(PSR^.ID);
      Str := UTF8Copy(Str, 1, PSR^.UStart);
      FixNum := Str.CountChar(#10);
    end;

    memoNote.SelStart := PSR^.UStart - 1 + FixNum;
    memoNote.SelLength := PSR^.ULength;
  end;
end;

procedure TformMain.lstbInfoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // 在 GTK2 中, TMemo.HideSelection := False 无效，所以需要修补
  if FFocusNote then memoNote.SetFocus;
  FFocusNote := False;
end;

procedure TformMain.timrMainTimer(Sender: TObject);
begin
  // 自动保存
  if Config.AutoSaveInterval > 0 then begin
    if Config.AutoSaveRemaining > Config.AutoSaveInterval then
      Config.AutoSaveRemaining := Config.AutoSaveInterval;
    Dec(Config.AutoSaveRemaining);

    if Config.AutoSaveRemaining <= 0 then begin
      Config.AutoSaveRemaining := Config.AutoSaveInterval;
      SaveDB;
    end;
  end;

  // 自动备份
  if (Config.AutoBackupInterval > 0) and (Config.AutoBackupCount > 0) and Config.ChangedAfterBackup then begin
    if Config.AutoBackupRemaining > Config.AutoBackupInterval then
      Config.AutoBackupRemaining := Config.AutoBackupInterval;
    Dec(Config.AutoBackupRemaining);

    if Config.AutoBackupRemaining <= 0 then begin
      Config.AutoBackupRemaining := Config.AutoBackupInterval;
      if not BackupDB then
        Application.MessageBox(PChar(Res_BackupDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    end;
    ShowAutoBackupInfo;
  end;
end;

procedure TformMain.LoadControlState;
begin
  // 设置窗口状态
  if Screen.Fonts.IndexOf(Config.WindowFontName) <> -1 then
    Font.Name := Config.WindowFontName;
  Font.Size := Config.WindowFontSize;

  // 设置控件状态
  ToggleMenuBar(Config.MenuBarVisible);
  ToggleToolBar(Config.ToolBarVisible);
  ToggleStatBar(Config.StatBarVisible);
  ToggleTreeBar(Config.TreeBarVisible);
  ToggleRecyBar(Config.RecyBarVisible);
  ToggleInfoBar(Config.InfoBarVisible);

  SetTreeBarFontName(Config.TreeBarFontName);
  SetNoteBarFontName(Config.NoteBarFontName);
  SetInfoBarFontName(Config.InfoBarFontName);

  SetTreeBarFontSize(Config.TreeBarFontSize);
  SetNoteBarFontSize(Config.NoteBarFontSize);
  SetInfoBarFontSize(Config.InfoBarFontSize);

  ControlBarAutoSize;

  UpdateRecyclerState;

  RemoveMenuBarItem(Config.RemoveMenuBarItem);

  // 设置主题
  if FTreeDB.Active then
    SetActiveTheme(Config.ActiveTheme);

  // 设置历史记录
  FMemoHistory.MaxSize := Config.HistoryMaxSize * 1024;
  FMemoHistory.MinCount := Config.HistoryMinCount;

  ShowAutoBackupInfo;
end;

procedure TformMain.SaveControlState;
begin
  if WindowState = wsNormal then
    Config.MainFormRect := BoundsRect;

  Config.Maximized := WindowState = wsMaximized;
end;

procedure TformMain.UpdateDBControlState;
var
  AEnabled: Boolean;
begin
  AEnabled := FTreeDB.Active;

  // 文件相关
  actnSave.Enabled           := AEnabled and FTreeDB.Changed;
  actnSaveAs.Enabled         := AEnabled;
  actnClose.Enabled          := AEnabled;

  // 目录树相关
  UpdateTreeControlState;

  actnBrightTheme.Enabled    := AEnabled;
  actnDarkTheme.Enabled      := AEnabled;
  actnToggleTheme.Enabled    := AEnabled;

  // 主题相关
  if AEnabled then
    SetActiveTheme(Config.ActiveTheme)
  else begin
    SetBackColor(clDkGray);
    memoNote.ReadOnly := True;
  end;
end;

procedure TformMain.UpdateTreeControlState;
var
  AEnabled: Boolean;
begin
  AEnabled := FTreeDB.Active and panlLeft.Visible;

  actnRename        .Enabled := AEnabled and (FLastNode <> nil);

  actnInsert        .Enabled := AEnabled and (FLastTreeView = trevTree);
  actnInsertBehind  .Enabled := actnInsert.Enabled;
  actnAddChildFirst .Enabled := actnInsert.Enabled;
  actnAddChild      .Enabled := actnInsert.Enabled;
  actnImport        .Enabled := actnInsert.Enabled;

  actnDeleteNode    .Enabled := actnRename.Enabled and (FLastTreeView = trevTree);
  actnRecycleNode   .Enabled := actnDeleteNode.Enabled;
  actnMoveUp        .Enabled := actnDeleteNode.Enabled;
  actnMoveDown      .Enabled := actnDeleteNode.Enabled;
  actnExpand        .Enabled := actnDeleteNode.Enabled;
  actnCollapse      .Enabled := actnDeleteNode.Enabled;
  actnExport        .Enabled := actnDeleteNode.Enabled;
  actnNodeUtils     .Enabled := actnDeleteNode.Enabled;

  actnPrevNode      .Enabled := trevTree.Items.Count > 0;
  actnNextNode      .Enabled := actnPrevNode.Enabled;
  actnSearch        .Enabled := actnPrevNode.Enabled;

  actnRestoreNode   .Enabled := trevRecy.Selected <> nil;
  actnEmptyRecycler .Enabled := trevRecy.Items.Count > 0;

  UpdateRecyclerState;
end;

procedure TformMain.UpdateRecyclerState;
begin
  if trevRecy.Items.Count = 0 then begin
    actnToggleRecyBar.ImageIndex := EmptyRecyIcon;
    actnEmptyRecycler.Enabled :=  False;
  end else begin
    actnToggleRecyBar.ImageIndex := FullRecyIcon;
    actnEmptyRecycler.Enabled :=  True;
  end;
end;

procedure TformMain.LoadLastFile;
begin
  if Config.LoadLastFile and FileExists(Config.LastFile) then begin
    OpenDB(Config.LastFile);

    if (FLastNode = nil) and (trevTree.Items.Count > 0) then
      trevTree.Items[0].Selected := True;

    if Config.AutoBackupRemaining > Config.AutoBackupInterval then
      Config.AutoBackupRemaining := Config.AutoBackupInterval;
  end;
end;

procedure TformMain.DBActiveChanged(Sender: TObject);
begin
  FDBFullName := FTreeDB.DatabaseName;
  FDBFileDir  := ExtractFileDir(FDBFullName);
  FDBFileName := ExtractFileName(FDBFullName);

  if FTreeDB.Active then begin
    Config.LastFile:= FDBFullName;
    Config.AutoSaveRemaining := Config.AutoSaveInterval;
    if FTreeDB.Config.AutoBackupRemaining = -1 then
      Config.AutoBackupRemaining := Config.AutoBackupInterval
    else
      Config.AutoBackupRemaining := FTreeDB.Config.AutoBackupRemaining;
    Config.ChangedAfterBackup := FTreeDB.Config.ChangedAfterBackup;
  end;

  timrMain.Enabled := FTreeDB.Active;

  sbarMain.Panels[0].Text := '';
  sbarMain.Panels[1].Text := '';
  sbarMain.Panels[2].Text := '';

  UpdateDBControlState;

  if editRename.Visible then editRename.Hide;
end;

procedure TformMain.DataStateChanged(ADataChanged: Boolean);
begin
  actnSave.Enabled := ADataChanged;
  UpdateCaption(ADataChanged);
end;

procedure TformMain.UpdateCaption(ADataChanged: Boolean);
var
  Str: string;
begin
  Str := Format('%s %s', [AppTitle, Version]);

  if FTreeDB.Active then begin
    Str := Str + ' [' + FDBFileName + ']';
    if ADataChanged then
      Str := Str + ' *';
  end;

  Caption := Str;
end;

// debug
procedure TformMain.ShowAutoBackupInfo;
var
  ActiveStr: string;
begin
  if Config.ChangedAfterBackup then
    ActiveStr := Res_Activated
  else
    ActiveStr := Res_NotActive;

  sbarMain.Panels[1].Text := Format(Res_AutoBackupInfo, [Config.AutoBackupRemaining, ActiveStr]);
end;

procedure TformMain.HistoryChanged(Sender: TObject);
begin
  ShowHistoryInfo;
end;

// debug
procedure TformMain.ShowHistoryInfo;
begin
  sbarMain.Panels[2].Text := Format(Res_HistoryDebug,
    [FMemoHistory.Index,
    FMemoHistory.Count, FMemoHistory.MinCount,
    FMemoHistory.Size, FMemoHistory.MaxSize]);
end;

function TformMain.GetTotalHistorySize: integer;
begin
  Result := FMemoHistory.TotalSize;
end;

procedure TformMain.DiscardHistory(KeepSelected: boolean);
var
  i: integer;
  ANode: TTreeNode;
  AHistory: THistory;
begin
  // 处理目录树中的非当前节点
  for i := 0 to trevTree.Items.Count - 1 do
  begin
    ANode := trevTree.Items[i];
    AHistory := GetNodeHistory(ANode);
    if (AHistory <> nil) and (FLastNode <> ANode) then
    begin
      SetNodeHistory(ANode, nil);
      AHistory.Free;
    end;
  end;

  // 处理回收站中的非当前节点
  for i := 0 to trevRecy.Items.Count - 1 do
  begin
    ANode := trevRecy.Items[i];
    AHistory := GetNodeHistory(ANode);
    if (AHistory <> nil) and (FLastNode <> ANode) then
    begin
      SetNodeHistory(ANode, nil);
      AHistory.Free;
    end;
  end;

  // 处理当前节点
  if (not KeepSelected) then
    FMemoHistory.Reset;
end;

procedure TformMain.SubmitNote;
begin
  if FNoteChanged and (FLastNode <> nil) then
    FTreeDB.SetNote(GetNodeID(FLastNode), ToLF(memoNote.Text)); // Clear #13

  FNoteChanged := False;
end;

procedure TformMain.LoadNote(ANode: TTreeNode);
var
  AOldLoading: boolean;
begin
  AOldLoading := FLoading;
  FLoading := True;

  FMemoHistory.Enabled := False;

  // 保存光标位置（滚动条位置设置无效）
  if FLastNode <> nil then
    SetNodePosition(FLastNode, memoNote.SelStart);

  // 根据需要销毁上一个节点的历史记录
  if (FLastNode <> nil) and (FLastNode <> ANode) and
    ((FMemoHistory.Size = 0) or not Config.KeepNodesHistory) then begin
    SetNodeHistory(FLastNode, nil);
    FMemoHistory.DestroyHistory;
  end;

  if (ANode = nil) then begin
    memoNote.Clear;
    memoNote.ReadOnly := True;
  end else begin
    memoNote.Text := FTreeDB.GetNote(GetNodeID(ANode));
    memoNote.ReadOnly := False;

    // 当前节点的历史记录没有创建，在这里创建它
    if GetNodeHistory(ANode) = nil then begin
      FMemoHistory.CreateHistory(memoNote.Text);
      SetNodeHistory(ANode, FMemoHistory.History);
    end else
      FMemoHistory.SwitchHistory(GetNodeHistory(ANode), memoNote.Text);
  end;

  FLastNode := ANode;

  if FMemoHistory.History <> nil then
    FMemoHistory.Enabled := True;

  FLoading := AOldLoading;

  // 恢复光标位置（滚动条位置设置无效）
  if FLastNode <> nil then
    memoNote.SelStart := GetNodePosition(FLastNode);
end;

procedure TformMain.StripTailSpace;
var
  APrevText: string;
begin
  if FLastNode = nil then Exit;

  APrevText := memoNote.Text;

  memoNote.Lines.BeginUpdate;
  FMemoHistory.Enabled := False;

  memoNote.Text := ReplaceRegExpr('(?m)[ 　\t]+$', memoNote.Text, '', False);

  FMemoHistory.Enabled := True;
  memoNote.Lines.EndUpdate;

  if APrevText.Length > Length(memoNote.Text) then
    FMemoHistory.AddRecordSimply(APrevText);
end;

procedure TformMain.SetWordWrap(AWrap: Boolean);
begin
  actnWordWrap.Checked := AWrap;
  Config.WordWrap := AWrap;

  memoNote.WordWrap := AWrap;
end;

procedure TformMain.SetTreeBarWidth(AWidth: integer);
begin
  Config.TreeBarWidth := AWidth;

  panlLeft.Width := AWidth;
  spltMain.Left := Width;  // 确保分隔条在 panlLeft 的后面
end;

procedure TformMain.SetInfoBarHeight(AHeight: integer);
begin
  Config.InfoBarHeight := AHeight;

  lstbInfo.Top := panlRight.Height - AHeight;
  lstbInfo.Height := AHeight;
  spltRight.Top := 0;  // 确保分隔条在 lstbInfo 的上面
end;

procedure TformMain.SetRecyBarHeight(AHeight: integer);
begin
  Config.RecyBarHeight := AHeight;

  trevRecy.Top := panlLeft.Height - AHeight;
  trevRecy.Height := AHeight;
  spltLeft.Top := 0;  // 确保分隔条在 trevRecy 的上面
end;

procedure TformMain.ControlBarAutoSize;
begin
  DisableAutoSizing;

  if Config.TreeBarAutoSize and (Config.TreeBarPercent > 0) then
    SetTreeBarWidth(Width * Config.TreeBarPercent div 100);

  if Config.RecyBarAutoSize and (Config.RecyBarPercent > 0) then
    SetRecyBarHeight(panlLeft.Height * Config.RecyBarPercent div 100);

  if Config.InfoBarAutoSize and (Config.InfoBarPercent > 0) then
    SetInfoBarHeight(panlRight.Height * Config.InfoBarPercent div 100);

  sbarMain.Panels[2].Width := sbarMain.Width;
  EnableAutoSizing;
end;

procedure TformMain.ToggleMenuBar(AVisible: Boolean);
begin
  actnToggleMenuBar.Checked := AVisible;
  Config.MenuBarVisible := AVisible;

  if AVisible then Menu := menuMain else Menu := nil;
end;

procedure TformMain.ToggleToolBar(AVisible: Boolean);
begin
  actnToggleToolBar.Checked := AVisible;
  Config.ToolBarVisible := AVisible;

  tbarMain.Visible := AVisible;
end;

procedure TformMain.ToggleStatBar(AVisible: Boolean);
begin
  actnToggleStatBar.Checked := AVisible;
  Config.StatBarVisible := AVisible;

  sbarMain.Visible := AVisible;
end;

procedure TformMain.ToggleTreeBar(AVisible: Boolean);
begin
  actnToggleTreeBar.Checked := AVisible;
  Config.TreeBarVisible := AVisible;

  panlLeft.Visible := AVisible;
  spltMain.Visible := AVisible;
  spltMain.Left := Width;  // 确保分隔条在 panlLeft 的后面

  UpdateTreeControlState;
end;

procedure TformMain.ToggleInfoBar(AVisible: Boolean);
begin
  actnToggleInfoBar.Checked := AVisible;
  Config.InfoBarVisible := AVisible;

  lstbInfo.Visible := AVisible;
  spltRight.Visible := AVisible;
  spltRight.Top := 0;  // 确保分隔条在 lstbInfo 的上面
end;

procedure TformMain.ToggleRecyBar(AVisible: Boolean);
begin
  actnToggleRecyBar.Checked := AVisible;
  Config.RecyBarVisible := AVisible;

  trevRecy.Visible := AVisible;
  spltLeft.Visible := AVisible;
  spltLeft.Top := 0;  // 确保分隔条在 trevRecy 的上面
end;

procedure TformMain.FullScreen(AVisible: Boolean);
var
  Form: TForm;
begin
  actnFullScreen.Checked := AVisible;
  Config.FullScreen := AVisible;

  if AVisible = (BorderStyle = bsNone) then Exit;

  if AVisible then begin
    if WindowState = wsNormal then
      Config.MainFormRect := BoundsRect;
    BorderStyle := bsNone;
    BoundsRect := Screen.DesktopRect;
    // 在 GTK2 中直接设置 BorderStyle := bsNone 会导致窗口消失，需要通过另一个窗体将其刷新。
    Form := TForm.Create(nil);
    Parent := Form;
    Parent := nil;
    Form.Free;
  end else begin
    WindowState := wsNormal;
    BorderStyle := bsSizeable;
    BoundsRect := Config.MainFormRect;
  end;
end;

procedure TformMain.FullWindow(AVisible: Boolean);
begin
  if Config.FWHideMenuBar then ToggleMenuBar(AVisible);
  if Config.FWHideToolBar then ToggleToolBar(AVisible);
  if Config.FWHideStatBar then ToggleStatBar(AVisible);
  if Config.FWHideTreeBar then ToggleTreeBar(AVisible);
  if Config.FWHideInfoBar then ToggleInfoBar(AVisible);
  if Config.FWHideRecyBar then ToggleRecyBar(AVisible);
end;

procedure TformMain.RemoveMenuBarItem(ARemove: Boolean);
var
  AExists: boolean;
begin
  AExists := menuNote.Items.IndexOf(pmiToggleMenuBar) <> -1;
  if ARemove = not AExists then Exit;

  if ARemove then
    menuNote.Items.Remove(pmiToggleMenuBar)
  else
    menuNote.Items.Add(pmiToggleMenuBar);
end;

procedure TformMain.SetActiveTheme(AThemeID: Integer);
begin
  actnBrightTheme.Checked := False;
  actnDarkTheme.Checked := False;
  Config.ActiveTheme := AThemeID;

  case AThemeID of
    BrightThemeID: begin
      actnBrightTheme.Checked := True;
      SetForeColor(Config.BrightFontColor);
      SetBackColor(Config.BrightBackColor);
    end;
    DarkThemeID: begin
      actnDarkTheme.Checked := True;
      SetForeColor(Config.DarkFontColor);
      SetBackColor(Config.DarkBackColor);
    end;
  end;
end;

procedure TformMain.SetForeColor(AColor: TColor);
begin
  trevTree.SeparatorColor := AColor;
  trevTree.TreeLineColor := AColor;
  trevTree.Font.Color := AColor;

  trevRecy.SeparatorColor := AColor;
  trevRecy.TreeLineColor := AColor;
  trevRecy.Font.Color := AColor;

  editRename.Font.Color := AColor;

  memoNote.Font.Color := AColor;
  lstbInfo.Font.Color := AColor;
end;

procedure TformMain.SetBackColor(AColor: TColor);
begin
  trevTree.BackGroundColor := AColor;
  trevRecy.BackGroundColor := AColor;

  editRename.Color := AColor;

  memoNote.Color := AColor;
  lstbInfo.Color := AColor;
end;

procedure TformMain.SetTreeBarFontName(AName: string); inline;
begin
  if Screen.Fonts.IndexOf(AName) = -1 then
    AName := 'default';
  Font.Name := AName;
  trevTree.Font.Name := AName;
  trevRecy.Font.Name := AName;
  editRename.Font.Name := AName;
  Config.TreeBarFontName := AName;
end;

procedure TformMain.SetNoteBarFontName(AName: string); inline;
begin
  if Screen.Fonts.IndexOf(AName) = -1 then
    AName := 'default';
  memoNote.Font.Name := AName;
  Config.NoteBarFontName := AName;
end;

procedure TformMain.SetInfoBarFontName(AName: string); inline;
begin
  if Screen.Fonts.IndexOf(AName) = -1 then
    AName := 'default';
  lstbInfo.Font.Name := AName;
  Config.InfoBarFontName := AName;
end;

procedure TformMain.SetTreeBarFontSize(ASize: integer); inline;
begin
  trevTree.Font.Size := ASize;
  trevRecy.Font.Size := ASize;
  editRename.Font.Size := ASize;
  editRename.OnExit(editRename);
  Config.TreeBarFontSize := ASize;
end;

procedure TformMain.SetNoteBarFontSize(ASize: integer); inline;
begin
  memoNote.Font.Size := ASize;
  Config.NoteBarFontSize := ASize;
end;

procedure TformMain.SetInfoBarFontSize(ASize: integer); inline;
begin
  lstbInfo.Font.Size := ASize;
  Config.InfoBarFontSize := ASize;
end;

function GetInitDir: string;
begin
  Result := ExtractFileDir(formMain.DBFullName);
  if DirectoryExists(Result) then Exit;

  Result := ConcatPaths([AppDir, DefDBDir]);
  if DirectoryExists(Result) then Exit;

  Result := AppDir;
  if FileIsReadOnly(Result) then Result := '';
end;

function OverwriteFileDialog(AFileName, ATitle: string): boolean;
begin
  Result := not FileExists(AFileName);
  if Result then Exit;

  Result := Application.MessageBox(PChar(Res_OverwriteFileTip), PChar(ATitle), MB_YESNO + MB_ICONQUESTION) = idYes;

  if Result and not DeleteFile(AFileName) then
  begin
    Result := False;
    Application.MessageBox(PChar(Res_OverwriteFileFail), PChar(ATitle), MB_OK + MB_ICONERROR);
  end;
end;

procedure TformMain.AddRecentFile(AFileName: string);
var
  i, n: Integer;
  Index, NewIndex: Integer;
  NewMenuItem: TMenuItem;
begin
  // 判断文件是否存在与列表中
  n := -1;
  for i := 0 to Config.RecentFiles.Count - 1 do
    if Config.RecentFiles.ValueFromIndex[i] = AFileName then begin
      n := i;
      break;
    end;

  // 如果文件在列表中，则将文件序号移到最前面
  if n >= 0 then begin
    // 获取文件序号，并将文件移到最前面
    Index := StrToIntDef(Config.RecentFiles.Names[n], 10);
    Config.RecentFiles[n] := '1=' + Config.RecentFiles.ValueFromIndex[i];
    // 将当前序号之前的文件后移
    for i := 0 to Config.RecentFiles.Count - 1 do begin
      if i = n then continue;
      NewIndex := StrToIntDef(Config.RecentFiles.Names[i], 10);
      if NewIndex < Index then
        Config.RecentFiles[i] := IntToStr(NewIndex + 1) + '=' + Config.RecentFiles.ValueFromIndex[i];
    end;
    Exit;
  end;

  // 如果文件不在列表中，则将所有文件后移
  for i := Config.RecentFiles.Count - 1 downto 0 do begin
    Index := StrToIntDef(Config.RecentFiles.Names[i], 10);
    // 删除超出范围的文件项
    if Index >= 10 then begin
      mmiRecentFiles.Delete(i);
      Config.RecentFiles.Delete(i);
    end
    else // 将其它文件后移
      Config.RecentFiles[i] := IntToStr(Index + 1) + '=' + Config.RecentFiles.ValueFromIndex[i];
  end;

  // 添加新的文件项
  NewMenuItem := TMenuItem.Create(Self);
  NewMenuItem.Caption := AFileName;
  NewMenuItem.OnClick := @OpenRecentFile;
  mmiRecentFiles.Insert(0, NewMenuItem);
  Config.RecentFiles.Insert(0, '1='+AFileName);
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

procedure TformMain.DelRecentFile(AFileName: string);
var
  i: integer;
  Index, NewIndex: integer;
begin
  // 查找文件索引
  for i := 0 to Config.RecentFiles.Count - 1 do
    if Config.RecentFiles.ValueFromIndex[i] = AFileName then
      break;

  // 文件不在列表中则退出
  if i >= Config.RecentFiles.Count then Exit;

  // 获取文件序号备用，然后删除文件项
  Index := StrToIntDef(Config.RecentFiles.Names[i], 10);
  mmiRecentFiles.Delete(i);
  Config.RecentFiles.Delete(i);

  // 将大于此序号的项目前移
  for i := 0 to Config.RecentFiles.Count - 1 do begin
    NewIndex := StrToIntDef(Config.RecentFiles.Names[i], 10);
    if NewIndex > Index then
      Config.RecentFiles[i] := IntToStr(NewIndex - 1) + '=' + Config.RecentFiles.ValueFromIndex[i];
  end;
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

procedure TformMain.LoadRecentFiles;
var
  i: Integer;
  NewMenuItem: TMenuItem;
begin
  for i := Config.RecentFiles.Count - 1 downto 0 do begin
    NewMenuItem := TMenuItem.Create(self);
    NewMenuItem.Caption := Config.RecentFiles.ValueFromIndex[i];
    NewMenuItem.OnClick := @OpenRecentFile;
    mmiRecentFiles.Insert(0, NewMenuItem);
  end;
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

procedure TformMain.OpenRecentFile(Sender: TObject);
var
  AFileName: String;
begin
  if Sender = nil then Exit;
  AFileName := (Sender as TMenuItem).Caption;
  if IsKeyDown(VK_SHIFT) then
    DelRecentFile(AFileName)
  else if FileExists(AFileName) then
    OpenDB(AFileName)
  else if Application.MessageBox(PChar(Res_RecentFileNotExists), PChar(AppTitle), MB_YESNO+MB_ICONWARNING) = ID_YES then
    DelRecentFile(AFileName);
end;

procedure TformMain.mmiClearRecentFilesClick(Sender: TObject);
begin
  while mmiRecentFiles.Count > 2 do begin
    mmiRecentFiles.Delete(0);
  end;
  Config.RecentFiles.Clear;
  mmiClearRecentFiles.Enabled := Config.RecentFiles.Count > 0;
end;

function TformMain.CreateDB(AFileName: string = ''): boolean;
var
  OverwriteFile: Boolean;
begin
  Result := False;

  if AFileName = '' then
  begin
    svdg1.FileName   := DefDBName + DBFileExt;
    svdg1.InitialDir := GetInitDir;
    if not svdg1.Execute then Exit;
    AFileName := svdg1.FileName;
  end;

  if not SaveDBDialog then Exit;

  OverwriteFile := False;
  if FileExists(AFileName) then begin
    OverwriteFile := Application.MessageBox(PChar(Res_OverwriteFileTip), PChar(AppTitle), MB_YESNO + MB_ICONQUESTION) = idYes;
    if not OverwriteFile then Exit;
  end;

  if not CloseDB(False) then Exit;

  if OverwriteFile and not DeleteFile(AFileName) then
  begin
    Application.MessageBox(PChar(Res_OverwriteFileFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  // 不要使用 Self.OpenDB, 它会检查文件是否存在，如果不存在就不会创建新文件
  Result := FTreeDB.OpenDB(AFileName);

  if not Result then
  begin
    Application.MessageBox(PChar(Res_CreateDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  LoadTrees;
  AddRecentFile(AFileName);
  DataStateChanged(False);

  ShowAutoBackupInfo;
end;

function TformMain.OpenDB(AFileName: String): boolean;
begin
  Result := False;

  if AFileName = '' then
  begin
    opdg1.InitialDir := GetInitDir;
    if not opdg1.Execute then Exit;
    AFileName := opdg1.FileName;
  end;

  // 防止用户在选择文件后，又通过其它方式删除了该文件
  if not FileExists(AFileName) then Exit;

  if not CloseDBDialog then Exit;

  Result := FTreeDB.OpenDB(AFileName);

  if not Result then
  begin
    Application.MessageBox(PChar(Res_OpenDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  LoadTrees;
  AddRecentFile(AFileName);
  DataStateChanged(False);

  if Config.SelectLastNode then
    SelectNodeByID(FTreeDB.Config.LastNodeID)
  else if trevTree.Items.Count > 0 then
    trevTree.Items[0].Selected := True;

  ShowAutoBackupInfo;
end;

function TformMain.SaveDBDialog: boolean;
begin
  SubmitNote;

  Result := not FTreeDB.Changed;
  if Result then Exit;

  case Application.MessageBox(PChar(Res_DataChangedTip), PChar(AppTitle), MB_YESNOCANCEL + MB_ICONQUESTION) of
    idYes    : Result := SaveDB;
    idNo     : Result := True;
    idCancel : Result := False;
  end;
end;

function TformMain.SaveDB: boolean;
begin
  SubmitNote;

  Config.AutoSaveRemaining := Config.AutoSaveInterval;
  FTreeDB.Config.AutoBackupRemaining := Config.AutoBackupRemaining;

  FTreeDB.SaveDB;

  Config.ChangedAfterBackup := FTreeDB.Config.ChangedAfterBackup;

  Result := True;

  DataStateChanged(False);

  ShowAutoBackupInfo;
end;

function TformMain.SaveDBAs(AFileName: String): boolean;
var
  OverwriteFile: Boolean;
begin
  Result := False;

  if AFileName = '' then
  begin
    svdg1.FileName   := FDBFileName;
    svdg1.InitialDir := GetInitDir;
    if not svdg1.Execute then Exit;
    AFileName := svdg1.FileName;
  end;

  OverwriteFile := False;
  if FileExists(AFileName) then begin
    OverwriteFile := Application.MessageBox(PChar(Res_OverwriteFileTip), PChar(AppTitle), MB_YESNO + MB_ICONQUESTION) = idYes;
    if not OverwriteFile then Exit;
  end;

  Result := FTreeDB.ExportToDB(RootID, RootID, AFileName + '.tmp', AllDepth) and
            FTreeDB.ExportToDB(RecyclerID, RecyclerID, AFileName + '.tmp', AllDepth);

  if not Result then begin
    Application.MessageBox(PChar(Res_SaveDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  if OverwriteFile then begin
    if AFileName = FDBFullName then
      CloseDB(False);
    if not DeleteFile(AFileName) then
    begin
      Application.MessageBox(PChar(Res_OverwriteFileFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
      Exit;
    end;
  end;

  CloseDB(False);

  Result := RenameFile(AFileName + '.tmp', AFileName);
  if not Result then begin
    Application.MessageBox(PChar(Res_OverwriteFileFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  OpenDB(AFileName);
end;

function TformMain.CloseDBDialog: boolean;
begin
  Result := False;

  SubmitNote;

  if not SaveDBDialog then Exit;

  Result := CloseDB(False);
end;

function TformMain.CloseDB(ASave: boolean): boolean;
begin
  Result := not FTreeDB.Active;

  if Result then Exit;

  SubmitNote;

  FTreeDB.Config.LastNodeID := GetNodeID(trevTree.Selected, FreeID);
  FTreeDB.Config.AutoBackupRemaining := Config.AutoBackupRemaining;
  Result := FTreeDB.CloseDB(ASave);

  if not Result then
  begin
    Application.MessageBox(PChar(Res_CloseDBFail), PChar(AppTitle), MB_OK + MB_ICONERROR);
    Exit;
  end;

  UnLoadTrees;

  DataStateChanged(False);
end;

function TformMain.BackupDB: boolean;
begin
  Result := FTreeDB.BackupDB(ConcatPaths([FDBFileDir, DefBackupDir]), Config.AutoBackupCount);
  Config.ChangedAfterBackup := FTreeDB.Config.ChangedAfterBackup;
end;

procedure TformMain.LoadTree(ATree: TTreeView; AID: integer);
var
  AOldLoading: boolean;
  Children: TBoundArray;
begin
  AOldLoading := FLoading;
  FLoading := True;

  UnloadTree(ATree);

  Children := FTreeDB.GetChildren(AID);
  for AID in Children do
    LoadNode(ATree, AID, nil, naAddChild);

  FLoading := AOldLoading;
end;

procedure TformMain.UnloadTree(ATree: TTreeView);
var
  AOldLoading: boolean;
begin
  AOldLoading := FLoading;
  FLoading := True;

  UnLoadSubNodes(ATree, nil);

  if FLastNode = nil then begin
    memoNote.Clear;
    lstbInfo.Clear;
    memoNote.ReadOnly := True;
  end;

  FLoading := AOldLoading;
end;

procedure TformMain.LoadTrees;
begin
  LoadTree(trevTree, RootID);
  LoadTree(trevRecy, RecyclerID);
end;

procedure TformMain.UnLoadTrees;
begin
  UnLoadTree(trevTree);
  UnLoadTree(trevRecy);
end;

function TformMain.LoadNode(ATree: TTreeView; AID: integer; AToNode: TTreeNode; AMode: TAttachMode): TTreeNode;
var
  AOldLoading: boolean;
begin
  AOldLoading := FLoading;
  FLoading := True;

  if AToNode = nil then AMode := naAdd;

  Result := ATree.Items.AddNode(nil, AToNode, FTreeDB.GetName(AID), nil, TNodeAttachMode(AMode));

  SetNodeID(Result, AID);
  SetNodeLoaded(Result, False);
  SetNodeHistory(Result, nil);

  // 如果新节点的父节点尚未动态加载子节点，则不在这里载入子节点，否则当新节点的父节点展开时会重复载入
  if (Result.Parent = nil) or (GetNodeLoaded(Result.Parent)) then
    LoadSubNodes(ATree, Result, 1);

  FLoading := AOldLoading;
end;

// Depth：需要载入的节点深度，1 表示只载入当前节点，AllDepth 表示载入当前节点及所其有子节点
procedure TformMain.LoadSubNodes(ATree: TTreeView; ANode: TTreeNode; Depth: integer);
var
  AOldLoading: boolean;

  procedure DoLoadSubNodes(ANode: TTreeNode; Depth: integer);
  var
    ID: integer;
    NewNode: TTreeNode;
    Children: TBoundArray;
  begin
    Dec(Depth);

    if ATree = trevTree then
      Children := FTreeDB.GetChildren(GetNodeID(ANode))
    else
      Children := FTreeDB.GetChildren(GetNodeID(ANode, RecyclerID));

    for ID in Children do
    begin
      NewNode := ATree.Items.AddChild(ANode, FTreeDB.GetName(ID));
      SetNodeID(NewNode, ID);
      SetNodeLoaded(NewNode, False);
      SetNodeHistory(NewNode, nil);

      if Depth = 0 then Continue;
      DoLoadSubNodes(NewNode, Depth);
    end;
  end;

begin
  AOldLoading := FLoading;
  FLoading := True;
  DoLoadSubNodes(ANode, Depth);
  FLoading := AOldLoading;
end;

procedure TformMain.UnLoadNode(ATree: TTreeView; ANode: TTreeNode);
var
  AOldLoading: boolean;
begin
  AOldLoading := FLoading;
  FLoading := True;

  UnLoadSubNodes(ATree, ANode);

  if ANode <> nil then begin
    CleanNodeData(ANode);
    ANode.Delete;
  end;

  FLoading := AOldLoading;
end;

procedure TformMain.UnLoadSubNodes(ATree: TTreeView; ANode: TTreeNode);
var
  AOldLoading: boolean;
  StartNode: TTreeNode;
  EndNode: TTreeNode;
begin
  AOldLoading := FLoading;
  FLoading := True;

  StartNode := nil;
  EndNode := nil;

  // 计算需要遍历的节点范围
  if ANode = nil then begin
    if ATree.Items.Count > 0 then
      StartNode := ATree.Items[0];
  end else begin
    if ANode.Count > 0 then
    begin
      StartNode := ANode.Items[0];
      EndNode := ANode.GetNextSkipChildren;
    end;
  end;

  // 遍历节点并清除其 Data 信息
  while StartNode <> EndNode do begin
    CleanNodeData(StartNode);
    StartNode := StartNode.GetNext;
  end;

  if ANode = nil then
    ATree.Items.Clear
  else
    ANode.DeleteChildren;

  FLoading := AOldLoading;
end;

// 重新载入子节点（忽略已经载入的节点）
// 返回第一个新载入的节点
function TformMain.ReLoadSubNodes(ATree: TTreeView; ANode: TTreeNode): TTreeNode;
var
  ID: integer;
  PrevNode, CurNode: TTreeNode;
  Children: TBoundArray;
  ADepth: Integer;
begin
  Result := nil;

  if ATree = trevTree then
    Children := FTreeDB.GetChildren(GetNodeID(ANode))
  else
    Children := FTreeDB.GetChildren(GetNodeID(ANode, RecyclerID));

  if ANode = nil then begin
    CurNode := ATree.Items[0];
    ADepth := 2;
  end else begin
    CurNode := ANode.GetFirstChild;
    if GetNodeLoaded(ANode) then
      ADepth := 2
    else
      ADepth := 1;
  end;

  // 没有已载入的子节点，需要全部载入
  if CurNode = nil then
  begin
    LoadSubNodes(ATree, ANode, ADepth);
    if ANode = nil then
      Result := ATree.Items[0]
    else
      Result := ANode.GetFirstChild;
    Exit;
  end;

  PrevNode := nil;

  // 遍历 Children ID，判断当前 ID 是否已经载入
  for ID in Children do
  begin
    // PrevNode 是最后一个已经载入的节点，接下来需要全部载入剩余的节点。
    if CurNode = nil then
    begin
      // 循环载入剩余的节点
      PrevNode := LoadNode(ATree, ID, PrevNode, naInsertBehind);
      if Result = nil then Result := PrevNode;
      Continue;
    end;

    // PrevNode 不是最后一个已载入的节点，需要检查当前 ID 是否已经载入。
    if GetNodeID(CurNode) = ID then
    begin
      // 跳过所有已载入的节点
      PrevNode := CurNode;
      CurNode := CurNode.GetNextSibling;
      Continue;
    end;

    // 当前 ID 没有被载入，而且 CurNode 不是 nil
    PrevNode := LoadNode(ATree, ID, CurNode, naInsert);
    if Result = nil then Result := PrevNode;
  end;
end;

function TformMain.AddNode(AToTree: TTreeView; ANodeName, ANodeNote: string;
  AToNode: TTreeNode; AMode: TAttachMode): TTreeNode;
var
  AOldLoading: boolean;
  ID, ToID: integer;
begin
  AOldLoading := FLoading;
  FLoading := True;

  if ANodeName = '' then ANodeName := Res_UnnamedNode;

  if AToTree = trevTree then
    ToID := GetNodeID(AToNode)
  else
    ToID := GetNodeID(AToNode, RecyclerID);

  // 先在数据库中添加节点
  ID := FTreeDB.AddNode(ANodeName, ANodeNote, ToID, AMode);

  // 然后在目录树中添加节点
  if AToNode = nil then AMode := naAdd;
  Result := AToTree.Items.AddNode(nil, AToNode, ANodeName, nil, TNodeAttachMode(AMode));

  SetNodeID(Result, ID);
  SetNodeLoaded(Result, False);
  SetNodeHistory(Result, nil);

  DataStateChanged(True);

  FLoading := AOldLoading;
end;

function TformMain.DeleteNode: TTreeNode;
begin
  Result := nil;

  if (trevTree.Selected = nil) then Exit;

  if not IsKeyDown(VK_SHIFT) then
    if Application.MessageBox(PChar(Res_DelNodeWarning), PChar(AppTitle),
      MB_YESNO + MB_ICONWARNING) <> idYes then
        Exit;

  Result := trevTree.Selected.GetNextSibling;

  if Result = nil then
    Result := trevTree.Selected.GetPrevSibling;

  if Result = nil then
    Result := trevTree.Selected.Parent;

  // 虽然该节点即将被删除，但还是把数据提交到数据库，便于将来想恢复数据的时候
  // 可以用第三方数据库打开数据库文件找到需要的数据。
  SubmitNote;

  FTreeDB.DelNode(GetNodeID(trevTree.Selected));
  UnLoadNode(trevTree, trevTree.Selected);

  if Result = nil then
    LoadNote(nil)
  else
    Result.Selected := True;

  DataStateChanged(True);
  UpdateTreeControlState;
end;

function TformMain.RecycleNode: TTreeNode;
begin
  Result := nil;

  if trevTree.Selected = nil then Exit;

  Result := trevTree.Selected.GetNextSibling;

  if Result = nil then
    Result := trevTree.Selected.GetPrevSibling;

  if Result = nil then
    Result := trevTree.Selected.Parent;

  SubmitNote;
  MoveNode(trevRecy, trevTree.Selected, nil, naAddChild);

  if Result = nil then
    LoadNote(nil)
  else
    Result.Selected := True;

  UpdateTreeControlState;
end;

function TformMain.RestoreNode: TTreeNode;
begin
  Result := nil;

  if trevRecy.Selected = nil then Exit;

  Result := trevRecy.Selected.GetNextSibling;

  if Result = nil then
    Result := trevRecy.Selected.GetPrevSibling;

  if Result = nil then
    Result := trevRecy.Selected.Parent;

  SubmitNote;
  MoveNode(trevTree, trevRecy.Selected, trevTree.Selected, naInsert);

  if Result = nil then
    LoadNote(nil)
  else
    Result.Selected := True;

  if trevRecy.Items.Count = 0 then begin
    actnEmptyRecycler.Enabled := False;
    actnToggleRecyBar.ImageIndex := EmptyRecyIcon;
  end;
  UpdateTreeControlState;
end;

procedure TformMain.EmptyRecycler;
begin
  if trevRecy.Items.Count = 0 then Exit;

  if (Application.MessageBox(PChar(Res_EmptyRecyWarning), PChar(AppTitle),
    MB_YESNO + MB_ICONWARNING) <> idYes) then
      Exit;

  FTreeDB.EmptyRecycler;
  UnLoadTree(trevRecy);

  DataStateChanged(True);

  LoadNote(nil);

  UpdateTreeControlState;
end;

function TformMain.MoveNode(AToTree: TTreeView; AFromNode, AToNode: TTreeNode; AMode: TAttachMode): TTreeNode;
var
  AOldLoading: boolean;
  FromID, ToID: integer;
begin
  AOldLoading := FLoading;
  FLoading := True;

  FromID := GetNodeID(AFromNode);
  // AToNode 可能是 nil，所以不能通过 AToNode.TreeView 来获取 AToTree
  // AToTree 必须通过参数提供
  if AToTree = trevTree then
    ToID := GetNodeID(AToNode)
  else
    ToID := GetNodeID(AToNode, RecyclerID);

  // 先在数据库中移动节点
  FTreeDB.MoveNode(FromID, ToID, AMode);

  // 然后在目录树中移动节点
  if AFromNode.TreeView <> AToTree then begin
    // 在不同 TreeView 之间移动节点
    if AToNode = nil then
    case AMode of
      naAddChild, naInsertBehind: AMode := naAdd;
      naAddChildFirst, naInsert: AMode := naAddFirst;
    end;

    Result := LoadNode(AToTree, FromID, AToNode, AMode);

    UnLoadNode(TTreeView(AFromNode.TreeView), AFromNode);
  end else begin
    // 在同一 TreeView 内移动节点
    if AToNode = nil then begin
      // 如果将节点拖拽到 TreeView 的空白区域，则 AToNode 将会是 nil
      AToNode := AToTree.Items.GetLastNode;
      case AMode of
        naAddChildFirst: AMode := naInsert;
        naAddChild: AMode := naAdd;
      end;
    end;

    if AFromNode <> AToNode then
      AFromNode.MoveTo(AToNode, TNodeAttachMode(AMode));

    Result := AFromNode;
  end;

  DataStateChanged(True);

  FLoading := AOldLoading;
end;

function TformMain.CopyNode(AToTree: TTreeView; AFromNode, AToNode: TTreeNode;
  AMode: TAttachMode): TTreeNode;
var
  FromID, ToID, NewID: Integer;
begin
  FromID := GetNodeID(AFromNode);
  // AToNode 可能是 nil，所以不能通过 AToNode.TreeView 来获取 AToTree
  // AToTree 必须通过参数提供
  if AToTree = trevTree then
    ToID := GetNodeID(AToNode)
  else
    ToID := GetNodeID(AToNode, RecyclerID);

  // 先在数据库中复制节点
  NewID := FTreeDB.CopyNode(FromID, ToID, AMode);

  // 然后在目录树中复制节点
  Result := LoadNode(AToTree, NewID, AToNode, AMode);
end;

procedure TformMain.UpDownNode(Up: boolean);
var
  ID: integer;
  FromNode, ToNode: TTreeNode;
  Mode: TAttachMode;
begin
  FromNode := trevTree.Selected;
  if FromNode = nil then Exit;

  ID := GetNodeID(trevTree.Selected);

  FTreeDB.UpDownNode(ID, Up);

  if Up then begin
    ToNode := FromNode.GetPrevSibling;
    Mode := naInsert;

    if ToNode = nil then begin
      ToNode := FromNode.GetLastSibling;
      Mode := naInsertBehind;
    end;
  end else begin
    ToNode := FromNode.GetNextSibling;
    Mode := naInsertBehind;

    if ToNode = nil then begin
      if FromNode.Parent = nil then
        ToNode := FromNode.TreeView.Items[0]
      else
        ToNode := FromNode.Parent.GetFirstChild;
      Mode := naInsert;
    end;
  end;

  FromNode.MoveTo(ToNode, TNodeAttachMode(Mode));

  DataStateChanged(True);
end;

procedure TformMain.RenameNode;
var
  ARect: TRect;
begin
  if FLastNode = nil then Exit;

  FLastNodeText := FLastNode.Text;

  // 如果使用 TreeView 的编辑节点功能，则在编辑节点时，编辑框的背景色无法改
  // 变，所以我使用 TEdit 代替默认的编辑框实现重命名功能。
  editRename.Parent := FLastNode.TreeView;

  ARect := FLastNode.DisplayRect(True);
  ARect.Right := editRename.Parent.ClientRect.Right;
  editRename.BoundsRect := ARect;

  editRename.Text := FLastNode.Text;
  editRename.Show;
  editRename.SetFocus;
  editRename.SelectAll;

  FEditHistory.Reset;
  FEditHistory.Enabled := True;
end;

procedure TformMain.SubmitRename;
begin
  if editRename.Visible and (FLastNode.Text <> editRename.Text) then begin
    FTreeDB.SetName(GetNodeID(FLastNode), editRename.Text);
    FLastNode.Text := editRename.Text;
    DataStateChanged(True);
  end;
  FEditHistory.Enabled := False;
end;

procedure TformMain.SelectPrevNode;
var
  Node: TTreeNode;
begin
  if FLastNode = nil then Exit;

  Node := FLastNode.GetPrev;

  if Node <> nil then
    Node.Selected := True;
end;

procedure TformMain.SelectNextNode;
var
  Node: TTreeNode;
begin
  if FLastNode = nil then Exit;

  Node := FLastNode.GetNext;

  if Node <> nil then
    Node.Selected := True;
end;

procedure TformMain.CompareEvent(Sender: TObject; Node1, Node2: TTreeNode; var Compare: Integer);
begin
  if (Node1.Parent <> FSortParent) or (Node2.Parent <> FSortParent) then
    Compare := 0
  else if FSortDesc then
    Compare := Node2.Text.CompareTo(Node1.Text)
  else
    Compare := Node1.Text.CompareTo(Node2.Text)
end;

procedure TformMain.SortNode(InSibling, Desc: boolean);
var
  i: integer;
  ANode: TTreeNode;
  ParentID: integer;
  Children: TBoundArray;
begin
  if trevTree.Selected = nil then Exit;

  if InSibling then
    FSortParent := trevTree.Selected.Parent
  else
    FSortParent := trevTree.Selected;

  FSortDesc := Desc;

  trevTree.OnCompare := @CompareEvent;
  trevTree.SortType := stText;
  trevTree.SortType := stNone;
  trevTree.OnCompare := nil;

  ParentID := GetNodeID(FSortParent);

  Children := FTreeDB.GetChildren(ParentID);
  if FSortParent <> nil then
    ANode := FSortParent.GetFirstChild
  else
    ANode := trevTree.Items[0];

  i := 0;
  while ANode <> nil do begin
    Children[i] := GetNodeID(ANode);
    Inc(i);
    ANode := ANode.GetNextSibling;
  end;

  FTreeDB.SetChildren(ParentID, Children);
  DataStateChanged(True);
end;

procedure TformMain.SplitNote(SplitterPattern, TitlePattern: string; IncludeSplitter: boolean; PreNumLen, SufNumLen: integer);
var
  AID: integer;
  ANote: string;
  ATitle, AContent: string;
  Index: integer;

  Expr, SubExpr: TRegExpr;
  Found: Boolean;
  AStart, ALength: integer;

  function DoAddNode: boolean;
  begin
    Result := False;
    AContent := UTF8Copy(ANote, AStart, ALength);
    if UTF8TrimSpace(AContent) = '' then Exit;

    if TitlePattern = '' then
      ATitle := Res_UnnamedNode
    else if SubExpr.Exec(AContent) then
      ATitle := SubExpr.Match[0]
    else
      ATitle := TitlePattern;

    if PreNumLen > 0 then
      ATitle := Format('%.' + PreNumLen.ToString + 'd', [Index]) + ATitle;
    if SufNumLen > 0 then
      ATitle := ATitle + Format('%.' + SufNumLen.ToString + 'd', [Index]);

    FTreeDB.AddNode(ATitle, AContent, AID, naAddChild);
    Result := True;
  end;

begin
  if trevTree.Selected = nil then Exit;
  if SplitterPattern = '' then Exit;

  AID := GetNodeID(trevTree.Selected);
  ANote := FTreeDB.GetNote(AID);

  Expr := TRegExpr.Create(SplitterPattern);
  try
    Found := Expr.Exec(ANote); // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符
    if not Found then Exit;

    AStart := 1;
    Index := 1;

    SubExpr := TRegExpr.Create(TitlePattern);
    try
      while Found do begin
        ALength := Expr.MatchPos[0] - AStart;

        if DoAddNode then Inc(Index);

        AStart := Expr.MatchPos[0];
        if not IncludeSplitter then
          Inc(AStart, Expr.MatchLen[0]);

        Found := Expr.ExecNext; // 需要 use lazUTF8 或 {$CODEPAGE UTF8} 以支持 Unicode 字符
      end;
      ALength := MaxInt;

      DoAddNode;
    finally
      SubExpr.Free;
    end;
  finally
    Expr.Free;
  end;
  ReLoadSubNodes(trevTree, trevTree.Selected);
  DataStateChanged(True);
end;

procedure TformMain.ScriptReplace(Script: string; InSelection: boolean);
var
  Strs: TStringList;
  AText, Line, Sch, Rep: string;
  APrevText: string;
  AChanged: Boolean;
  i: integer;
begin
  if InSelection and (memoNote.SelLength <> 0) then
    AText := memoNote.SelText
  else
    AText := memoNote.Text;

  APrevText := memoNote.Text;

  Strs := TStringList.Create;
  try
    Strs.Text := Script;
    AChanged := False;
    for i := 0 to Strs.Count - 1 do begin
      Line := Strs[i];
      if Line.Length >= 4 then begin
        case LowerCase(Line.Substring(0, 4)) of
          'sch=': Sch := Line.Substring(4);
          'rep=': if Sch <> '' then begin
            Rep := Line.Substring(4);
            AText := ReplaceRegExpr(Sch, AText, Rep, True);
            AChanged := True;
          end;
        end;
      end;
    end;
    if AChanged then begin
      FMemoHistory.Enabled := False;
      if InSelection and (memoNote.SelLength <> 0) then begin
        FMemoHistory.History.AddRecord(memoNote.SelStart, memoNote.SelLength, memoNote.SelText, memoNote.SelStart, UTF8Length(AText), AText);
        memoNote.SelText := AText
      end else begin
        memoNote.Text := AText;
        FMemoHistory.AddRecordSimply(APrevText);
      end;
      FMemoHistory.Enabled := True;
    end;
  finally
    Strs.Free;
  end;
end;

function TformMain.GetNodeID(ANode: TTreeNode; DefaultID: Integer = RootID): integer;
begin
  if ANode = nil then
    Result := DefaultID
  else
    Result := PNodeData(ANode.Data)^.ID;
end;

procedure TformMain.SetNodeID(ANode: TTreeNode; AID: integer);
begin
  if ANode.Data = nil then
    ANode.Data := new(PNodeData);

  PNodeData(ANode.Data)^.ID := AID;
end;

function TformMain.GetNodeLoaded(ANode: TTreeNode): boolean;
begin
  Result := PNodeData(ANode.Data)^.Loaded;
end;

procedure TformMain.SetNodeLoaded(ANode: TTreeNode; ALoaded: boolean);
begin
  if ANode.Data = nil then
    ANode.Data := new(PNodeData);

  PNodeData(ANode.Data)^.Loaded := ALoaded;
end;

function TformMain.GetNodePosition(ANode: TTreeNode): integer;
begin
  Result := PNodeData(ANode.Data)^.Position;
end;

procedure TformMain.SetNodePosition(ANode: TTreeNode; APosition: integer);
begin
  if ANode.Data = nil then
    ANode.Data := new(PNodeData);

  PNodeData(ANode.Data)^.Position := APosition;
end;

function TformMain.GetNodeHistory(ANode: TTreeNode): THistory;
begin
  Result := PNodeData(ANode.Data)^.History;
end;

procedure TformMain.SetNodeHistory(ANode: TTreeNode; AHistory: THistory);
begin
  if ANode.Data = nil then
    ANode.Data := new(PNodeData);

  PNodeData(ANode.Data)^.History := AHistory;
end;

procedure TformMain.CleanNodeData(ANode: TTreeNode);
var
  AHistory: THistory;
begin
  if ANode = nil then Exit;

  AHistory := GetNodeHistory(ANode);
  if AHistory <> nil then begin
    // 如果即将删除当前节点，则同时销毁 FMemoHistory 的历史记录
    if FMemoHistory.History = AHistory then
      FMemoHistory.DestroyHistory
    else
      AHistory.Free;
  end;

  dispose(PNodeData(ANode.Data));

  // 当前节点即将被删除，将其设置为 nil
  if FLastNode = ANode then FLastNode := nil;
end;

function TformMain.IDToNode(ID: integer; AutoLoad: boolean): TTreeNode;
var
  i: integer;
  IDLink: TBoundArray;
begin
  Result := nil;

  if ID <= ReserveID then Exit;

  IDLink := nil;

  while not (ID in [FreeID, RootID, RecyclerID, ReserveID]) do
  begin
    SetLength(IDLink, Length(IDLink) + 1);
    IDLink[High(IDLink)] := ID;
    ID := FTreeDB.GetParent(ID);
  end;

  case ID of
    RootID     : if trevTree.Items.Count > 0 then Result := trevTree.Items[0];
    RecyclerID : if trevRecy.Items.Count > 0 then Result := trevRecy.Items[0];
  end;

  for i := High(IDLink) downto 0 do
  begin
    while (Result <> nil) and (GetNodeID(Result) <> IDLink[i]) do
      Result := Result.GetNextSibling;

    if (Result = nil) or (i = 0) then Break;

    if AutoLoad and not GetNodeLoaded(Result) then
      Result.Expand(False);

    Result := Result.GetFirstChild;
  end;
end;

function TformMain.SelectNodeByID(ID: integer): TTreeNode;
begin
  Result := IDToNode(ID, True);

  if Result <> nil then
    Result.Selected := True;
end;

procedure TformMain.LoadSearchResult(UpdateHistory: boolean);
var
  i: integer;
  PSR: PSearchRecord;
  AName, ANote, Sample: string;
  ANode: TTreeNode;
  AHistory: THistory;
  LastID: Integer;
begin
  lstbInfo.Items.BeginUpdate;
  lstbInfo.Clear;
  LastID := 0;
  for i := 0 to SearchResult.Count - 1 do begin
    PSR := SearchResult[i];
    AName := FTreeDB.GetName(PSR^.ID);

    if PSR^.UStart + PSR^.ULength = 0 then // 搜索结果在节点名称中
      lstbInfo.Items.Add(Format('[%s]', [AName]))
    else begin                           // 搜索结果在节点内容中
      ANote := FTreeDB.GetNote(PSR^.ID);

      if PSR^.UStart < 10 then
        Sample := UTF8Copy(ANote, 1, PSR^.ULength + 10)
      else
        Sample := UTF8Copy(ANote, PSR^.UStart - 10, PSR^.ULength + 20);
      Sample := Sample.Replace(#10, ' ');

      lstbInfo.Items.Add(Format('[%s] %s | (%d %d)', [AName, Sample, PSR^.UStart, PSR^.ULength]));

      // 由于数据发生了完全的改变，历史记录不再有效，所以需要销毁相应的历史记录
      if UpdateHistory and (LastID <> PSR^.ID) then begin
        LastID := PSR^.ID;

        ANode := IDToNode(LastID, False);

        if (ANode <> nil) and (ANode <> FLastNode) then begin
          AHistory := GetNodeHistory(ANode);

          if AHistory <> nil then begin
            SetNodeHistory(ANode, nil);
            if FMemoHistory.History = AHistory then
              FMemoHistory.DestroyHistory
            else
              FreeAndNil(AHistory);
          end;
        end;
      end;
    end;
  end;

  lstbInfo.Items.EndUpdate;

  ToggleInfoBar(True);
end;

procedure TformMain.ReLoadNodeName;
var
  i: integer;
  PSR: PSearchRecord;
  Node: TTreeNode;
begin
  trevTree.BeginUpdate;

  for i := 0 to SearchResult.Count - 1 do begin
    PSR := SearchResult[i];
    Node := IDToNode(PSR^.ID, False);

    // 只修改已经加载的节点
    if Node <> nil then
      Node.Text := FTreeDB.GetName(PSR^.ID);
  end;

  trevTree.EndUpdate;
end;

procedure TformMain.Search(ANode: TTreeNode; AText: string; IncludeName, IncludeNote: boolean; ADepth: integer; IgnoreCase: boolean);
var
  ID: integer;
begin
  SubmitNote;

  ID := GetNodeID(ANode, RootID);

  SearchResult.Clear;
  FTreeDB.Search(ID, AText, IncludeName, IncludeNote, ADepth, IgnoreCase, SearchResult);
  LoadSearchResult(False);
end;

procedure TformMain.Replace(ANode: TTreeNode; AFrom, ATo: string; IncludeName, IncludeNote: boolean; ADepth: integer; IgnoreCase: boolean);
var
  ID: integer;
  APrevText: String;
begin
  SubmitNote;

  ID := GetNodeID(ANode, RootID);

  SearchResult.Clear;
  FTreeDB.Replace(ID, AFrom, ATo, IncludeName, IncludeNote, ADepth, IgnoreCase, SearchResult);
  LoadSearchResult(True);

  if SearchResult.Count > 0 then begin
    if IncludeName then
      ReLoadNodeName;

    DataStateChanged(True);
  end;

  APrevText := memoNote.Text;
  LoadNote(FLastNode);
  if (ADepth = 1) and (ANode = FLastNode) then
    FMemoHistory.AddRecordSimply(APrevText);
end;

procedure TformMain.RegSearch(ANode: TTreeNode; AText: string; IncludeName, IncludeNote: boolean; ADepth: integer);
var
  ID: integer;
begin
  SubmitNote;

  ID := GetNodeID(ANode, RootID);

  SearchResult.Clear;
  FTreeDB.RegSearch(ID, AText, IncludeName, IncludeNote, ADepth, SearchResult);
  LoadSearchResult(False);
end;

procedure TformMain.RegReplace(ANode: TTreeNode; AFrom, ATo: string; IncludeName, IncludeNote: boolean; ADepth: integer);
var
  ID: integer;
  APrevText: String;
begin
  SubmitNote;

  ID := GetNodeID(ANode, RootID);

  SearchResult.Clear;
  FTreeDB.RegReplace(ID, AFrom, ATo, IncludeName, IncludeNote, ADepth, SearchResult);
  LoadSearchResult(False);

  if SearchResult.Count > 0 then begin
    if IncludeName then
      ReLoadNodeName;

    DataStateChanged(True);
  end;

  APrevText := memoNote.Text;
  LoadNote(FLastNode);
  if (ADepth = 1) and (ANode = FLastNode) then
    FMemoHistory.AddRecordSimply(APrevText);
end;

function TformMain.ImportFile(FromPath: string; IncludeExt: boolean; mode: TAttachMode): TTreeNode;
var
  NewID, ToID: integer;
begin
  if not FileExists(FromPath) then Exit;

  ToID  := GetNodeID(trevTree.Selected, RootID);
  NewID := FTreeDB.ImportFile(FromPath, IncludeExt, ToID, mode);

  if trevTree.Selected = nil then begin
    LoadTree(trevTree, RootID);
    Result := trevTree.Items[0];
  end else
    Result := LoadNode(trevTree, NewID, trevTree.Selected, Mode);

  Result.Selected := True;

  DataStateChanged(True);
end;

function TformMain.ImportDir(FromPath: string; IncludeRoot: boolean; IncludeExt: boolean; mode: TAttachMode): TTreeNode;
var
  NewID, ToID: integer;
begin
  if not DirectoryExists(FromPath) then Exit;

  ToID := GetNodeID(trevTree.Selected, RootID);
  NewID := FTreeDB.ImportDir(FromPath, IncludeRoot, IncludeExt, ToID, mode);

  if trevTree.Selected = nil then begin
    LoadTree(trevTree, RootID);
    Result := trevTree.Items[0];
  end else if NewID <> ToID then
    Result := LoadNode(trevTree, NewID, trevTree.Selected, Mode)
  else begin
    Result := trevTree.Selected;

    case Mode of
      naAddChild, naAddChildFirst:
        Result := ReLoadSubNodes(trevTree, Result);

      else
        if Result.Level = 0 then
          Result := ReLoadSubNodes(trevTree, nil)
        else
          Result := ReLoadSubNodes(trevTree, Result.Parent)
    end;
  end;

  Result.Selected := True;

  DataStateChanged(True);
end;

function TformMain.ImportDB(FromPath: string; mode: TAttachMode): TTreeNode;
var
  NewID, ToID: integer;
begin
  if not FileExists(FromPath) then Exit;

  ToID := GetNodeID(trevTree.Selected, RootID);
  NewID := FTreeDB.ImportDB(FromPath, RootID, ToID, mode);

  if trevTree.Selected = nil then begin
    LoadTree(trevTree, RootID);
    Result := trevTree.Items[0];

  end else if NewID <> ToID then
    Result := LoadNode(trevTree, NewID, trevTree.Selected, Mode)

  else begin
    Result := trevTree.Selected;

    case Mode of
      naAddChild, naAddChildFirst:
        Result := ReLoadSubNodes(trevTree, Result);

      else
        if Result.Level = 0 then
          Result := ReLoadSubNodes(trevTree, nil)
        else
          Result := ReLoadSubNodes(trevTree, Result.Parent)
    end;
  end;

  Result.Selected := True;

  DataStateChanged(True);
end;

procedure TformMain.ExportToFile(ToPath: string; Node: TTreeNode; Splitter: string; Depth: integer);
var
  ID: integer;
begin
  ID := GetNodeID(Node, RootID);
  FTreeDB.ExportToFile(ID, ToPath, Splitter, Depth);
end;

procedure TformMain.ExportToDir(ToPath: string; Node: TTreeNode; Ext: string; Depth: integer);
var
  ID: integer;
begin
  ID := GetNodeID(Node, RootID);
  FTreeDB.ExportToDir(ID, ToPath, Ext, Depth);
end;

function TformMain.ExportToDB(ToPath: string; Node: TTreeNode; Depth: integer): boolean;
var
  ID: integer;
begin
  ID := GetNodeID(Node, RootID);
  Result := FTreeDB.ExportToDB(ID, RootID, ToPath, Depth);
end;

end.
