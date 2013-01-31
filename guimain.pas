unit guiMain;

// scarping del sito 'http://elezionistorico.interno.it'; cfr etcScraping.pas: K_BASE_URL

// la gerarchia delle pagine, i risultati di lista e dei candidati vengono memorizzati
// nel database eleScrap.db3 che deve essere nello stesso folder dell'eseguibile eleScrap.exe
// per la successva interrogazione interativa su questa pagina mediane i bottoni
// drill down (disaggrega la url selezionata) e up (torna al livello precedente)

// Testato SOLO per l'elezione dell'Assemblea Costituente
//                                  =====================
// altre elezioni mostrano aggregazioni di lista che per adesso non sono ancora supportate
//
// esistono 8 tipi di elezione (cfr etcScraping.pas: getEleTypeDescription)
// per ogni tipo esistono una o più date in cui si sono svolte le elezioni
// per ogni data esiste una classificazione geografica dei dati che si articola
// sempre in 4 livelli; il 4° livello (massima disaggregazione) è quello dei Comuni
// il 3° è quello delle provincie; il 2° e il primo variano a seconda del tipo di elezione;
//
// il software è agnostico rispetto alla gerarchia territoriale, nel senso che ogni
// elezione (tipo / data) ha la sua gerarchia territoriale indipendente (e certamente duplicata).
//
// questa implementazione è nata PRIMA dell'hackaton del 19/01/13 a Bologna
// e NON tiene conto della struttura SQL definita in quella sede. Ahimè !

// sorgenti in freePascal / lazarus 1.0.4 aperti a chiunque in licenza CC

// librerie:
// =========
// http://www.ararat.cz/synapse/ per l' http get della pagina
// (cfr httpsend.pas) che nelle opzioni | compilatore | percorsi | librerie
// è indicato come residente in E:\lazarus\3p\synapse40\source\lib\
// quindi va opportunamente MODIFICATO in base al VOSTRO setup

// ToDo:
// =====
// 1) Adottare la struttura della base dati definita all'hackaton (e rifare un bel po' di roba)
// 2) oppure raffinare quella attuale in modo che la descrizione di liste e condidati
// sia univoca a livello di elezione
// 3) adattare lo scraping di liste e candidati agli altri tipi di elezione

{$mode objfpc}{$H+}

interface uses
  Classes, SysUtils, db, BufDataset, sqlite3conn, sqldb, FileUtil, Ipfilebroker,
  Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids, DbCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnBoot: TButton;
    btnDrillDown: TButton;
    btnUp: TButton;
    btnSQLite: TButton;
    Button1: TButton;
    cbRecursive: TCheckBox;
    cbVerbose: TCheckBox;
    dgbRiep: TDBGrid;
    dsCandOfData: TDatasource;
    dsURLs: TDatasource;
    dbgURLs: TDBGrid;
    dbgCandidati: TDBGrid;
    dbgListe: TDBGrid;
    DBNavigator1: TDBNavigator;
    dsDataOfUrl: TDatasource;
    EditNodeID: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    procedure btnBootClick(Sender: TObject);
    procedure btnDrillDownClick(Sender: TObject);
    procedure btnSQLiteClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure dbgURLsDblClick(Sender: TObject);
    procedure dsDataOfUrlDataChange(Sender: TObject; Field: TField);
    procedure dsURLsDataChange(Sender: TObject; Field: TField);
  private
    procedure chkVerbose;
  public
  end;

var
  Form1: TForm1;

implementation uses etcScraping, guiSQLite;

{$R *.lfm}



{ TForm1 }

procedure TForm1.btnUpClick(Sender: TObject);
begin FormSQLite.doUrlsUp end;

procedure TForm1.Button1Click(Sender: TObject);
var iID: integer;
begin chkVerbose;
  iID := strToIntDef(editNodeID.Text, 0);   editNodeID.text := intToStr(iID);
  developNodeID(iID, cbVerbose.checked, cbRecursive.checked);
end;

procedure TForm1.btnDrillDownClick(Sender: TObject);
begin
  FormSQLite.doUrlsDrillDown;
end;

procedure TForm1.btnSQLiteClick(Sender: TObject);
begin
  FormSQLite.show
end;

procedure TForm1.btnBootClick(Sender: TObject);
begin   chkVerbose;
  doBoot(cbVerbose.checked);
end;

procedure TForm1.dbgURLsDblClick(Sender: TObject);
begin
  FormSQLite.doUrlsDrillDown;
end;

procedure TForm1.dsDataOfUrlDataChange(Sender: TObject; Field: TField);
begin
  FormSQLite.doCandDrillDown;
end;

procedure TForm1.dsURLsDataChange(Sender: TObject; Field: TField);
begin
  FormSQLite.doDataDrillDown
end;

procedure TForm1.chkVerbose;
begin
  if cbVerbose.checked then FormSQLite.show
end;


end.

