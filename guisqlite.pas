unit guiSQLite;   // interazione con il database

{$mode objfpc}{$H+}

interface uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, Types;

type
  // Questi record semplificano le operazioni sulle TABLE
  // riproducendone la struttura

  TEleUrlRec = record
  // nodo della gerachia, un record per ogni pagina del sito da scrapare
  // ID_PARENT punta alla pagina che contiene l'URL a "questa" pagina
    ID, ID_PARENT, ELETTORI, VOTANTI, NULLE, DI_CUI_BIANCHE: integer;
    DSCR, URL, ELE_TYPE, TITLE, IS_DATA: string;   // url della pagina index.php ...
    ELE_DATE: TDateTime;
  end;

  TEleDataRec = record
  // dettaglio voti, un record per ogni lista elettorale (DSCR)
  // non tutti gli eleUrlRec espongono liste (le "foglie" certamente sì)
    ID, ID_URL, VOTI: integer;   // ID_URL punta eleUrlRec
    DSCR, CAND_URL: string;            // url della pagina candidati.php ...
  end;

  TEleCandRec = record
  // eletti appartenenti ad una lista (ID_DATA punta a EleDataRec)
    ID, ID_DATA, PREFERENZE: integer;
    DSCR, BORN_WHERE, ELETTO: string;
    BORN_WHEN: TDateTime;
  end;



  { TFormSQLite }

  TFormSQLite = class(TForm)
    cnx: TSQLite3Connection;
    MemoLOG: TMemo;
    Panel1: TPanel;
    qdsCandOfData: TSQLQuery;
    qdsDataOfUrlCAND_URL: TStringField;
    qdsDataOfUrlDSCR: TStringField;
    qdsDataOfUrlID: TLongintField;
    qdsDataOfUrlID_URL: TLongintField;
    qdsDataOfUrlVOTI: TLongintField;
    qdsFindEleCandByID_DATA_DSCR: TSQLQuery;
    qdsFindEleUrlsByURL: TSQLQuery;
    qdsFindEleUrlsByID: TSQLQuery;
    qdsFindEleUrlsByID_PARENT: TSQLQuery;
    qdsInsertData: TSQLQuery;
    qdsFindEleDataByID_URL_DSCR: TSQLQuery;
    qdsInsertCand: TSQLQuery;
    qdsUpdateUrl: TSQLQuery;
    qdsURLs: TSQLQuery;
    qdsDataOfUrl: TSQLQuery;
    qdsURLsDI_CUI_BIANCHE: TLongintField;
    qdsURLsDSCR: TStringField;
    qdsURLsELETTORI: TLongintField;
    qdsURLsELE_DATE: TDateField;
    qdsURLsELE_TYPE: TStringField;
    qdsURLsID: TLongintField;
    qdsURLsID_PARENT: TLongintField;
    qdsURLsIS_DATA: TStringField;
    qdsURLsNULLE: TLongintField;
    qdsURLssEleDate1: TStringField;
    qdsURLsTITLE: TStringField;
    qdsURLsURL: TStringField;
    qdsURLsVOTANTI: TLongintField;
    SQLQuery1dataElezione2: TDateField;
    SQLQuery1descrizione2: TStringField;
    SQLQuery1DSCR1: TMemoField;
    SQLQuery1ELE_DATE1: TMemoField;
    SQLQuery1ELE_TYPE1: TMemoField;
    SQLQuery1ID1: TLongintField;
    SQLQuery1ID_PARENT1: TLongintField;
    SQLQuery1IS_DATA1: TMemoField;
    SQLQuery1URL1: TMemoField;
    SQLQueryUp: TSQLQuery;
    qdsInsertUrl: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure FormActivate(Sender: TObject);
    procedure qdsURLsBeforeOpen(DataSet: TDataSet);
    procedure qdsURLsCalcFields(DataSet: TDataSet);
  private
    { private declarations }
  public
    procedure clearAll;
    procedure doUrlsDrillDown;
    procedure doDataDrillDown;
    procedure doCandDrillDown;
    procedure doUrlsUp;
  end;

var
  FormSQLite: TFormSQLite;
  curr_ID_parent: integer = 0;

procedure logga(s: string);
function insertEleUrlRecAndGetBackID(var pRec: TEleUrlRec): integer;
procedure updateEleUrlRec(const pRec: TEleUrlRec);
function getEleUrlRecOfURL(pURL: string): TEleUrlRec;
function getEleUrlRecOfID(pID: integer): TEleUrlRec;
function getEleUrlListOfID_Parent(pID: integer): TStringDynArray;
procedure populateEleUrlRecFromQds(var pRec: TEleUrlRec; qds: TSQLQuery);
function insertEleDataRecAndGetBackID(var pRec: TEleDataRec): integer;
function insertEleCandRecAndGetBackID(var pRec: TEleCandRec): integer;

implementation

{$R *.lfm}

var
  _booting: boolean = true;

procedure logga(s: string);
begin
  FormSQLite.memoLOG.lines.add(formatDateTime('hh:nn:ss   ', now) + s)
end;



// ==============
// u r l s
// ==============

procedure populateQdsParamsFromEleUrlRec(pRec: TEleUrlRec; qds: TSQLQuery);
begin with qds do begin
  params.paramByName('DSCR'     ).asString   := pRec.DSCR;
  params.paramByName('URL'      ).asString   := pRec.URL;
  params.paramByName('ELE_TYPE' ).asString   := pRec.ELE_TYPE;
  params.paramByName('ELE_DATE' ).asDateTime := pRec.ELE_DATE;
  params.paramByName('IS_DATA'  ).asString   := pRec.IS_DATA;
  params.paramByName('ID_PARENT').asInteger  := pRec.ID_PARENT;
  params.paramByName('TITLE'    ).asString   := pRec.TITLE;
  params.paramByName('ELETTORI' ).asInteger  := pRec.ELETTORI;
  params.paramByName('VOTANTI'  ).asInteger  := pRec.VOTANTI;
  params.paramByName('NULLE'    ).asInteger  := pRec.NULLE;
  params.paramByName('DI_CUI_BIANCHE').asInteger  := pRec.DI_CUI_BIANCHE;
  if pRec.ELE_DATE = 0 then
    params.paramByName('ELE_DATE' ).asString := '';
end end;

procedure populateEleUrlRecFromQds(var pRec: TEleUrlRec; qds: TSQLQuery);
// helper: dalla TABLE al record
begin with qds do begin
  pRec.ID        := fieldByName('ID'       ).AsInteger;
  pRec.ID_PARENT := fieldByName('ID_PARENT').AsInteger;
  pRec.DSCR      := fieldByName('DSCR'     ).AsString;
  pRec.URL       := fieldByName('URL'      ).AsString;
  pRec.ELE_TYPE  := fieldByName('ELE_TYPE' ).AsString;
  pRec.ELE_DATE  := fieldByName('ELE_DATE' ).AsDateTime;
  pRec.IS_DATA   := fieldByName('IS_DATA'  ).AsString;
  pRec.TITLE     := fieldByName('TITLE'    ).asString;
  pRec.ELETTORI  := fieldByName('ELETTORI' ).AsInteger;
  pRec.VOTANTI   := fieldByName('VOTANTI'  ).AsInteger;
  pRec.NULLE     := fieldByName('NULLE'    ).AsInteger;
  pRec.DI_CUI_BIANCHE := fieldByName('DI_CUI_BIANCHE').AsInteger;
end end;

function insertEleUrlRecAndGetBackID(var pRec: TEleUrlRec): integer;
begin
  if copy(pRec.URL, length(pRec.URL) - 3, 4) = 'ms=S' then begin
    pRec.IS_DATA := '*'
  end else pRec.IS_DATA := '';
  populateQdsParamsFromEleUrlRec(pRec, FormSQLite.qdsInsertUrl);
  FormSQLite.qdsInsertUrl.execSQL;
  application.processMessages;
  FormSQLite.SQLTransaction1.commit;
  with getEleUrlRecOfURL(pRec.URL) do begin
    result := ID;   pRec.ID := ID
  end;
end;

procedure updateEleUrlRec(const pRec: TEleUrlRec);
begin
  populateQdsParamsFromEleUrlRec(pRec, FormSQLite.qdsUpdateUrl);
  with FormSQLite.qdsUpdateUrl do begin
    params.paramByName('ID').asInteger  := pRec.ID;
    execSQL;
  end;
  application.processMessages;
  FormSQLite.SQLTransaction1.commit;
end;

function getEleUrlRecOfURL(pURL: string): TEleUrlRec;
// permette di recuperare l'autoInc ID dopo un inserimento,
// dato che l'URL è univoco
begin
  with FormSQLite.qdsFindEleUrlsByURL do try
    close;
    params.ParamByName('URL').AsString := pURL;
    open;
    if EOF or (FieldByName('URL').AsString <> pURL) then begin
      result.id := 0;
    end else populateEleUrlRecFromQds(result, FormSQLite.qdsFindEleUrlsByURL);
  finally
    close;
  end;
end;

function getEleUrlRecOfID(pID: integer): TEleUrlRec;    // ID: PKey
begin
  with FormSQLite.qdsFindEleUrlsByID do try
    close;
    params.ParamByName('ID').AsInteger := pID;
    open;
    if EOF or (FieldByName('ID').AsInteger<> pID) then begin
      result.ID := 0;
    end else populateEleUrlRecFromQds(result, FormSQLite.qdsFindEleUrlsByID);
  finally
    close;
  end;
end;

function getEleUrlListOfID_Parent(pID: integer): TStringDynArray;
// restituisce tutti gli url gerarchicamente sotto l'URL ID
var elle: integer;
begin
  result := nil;
  with FormSQLite.qdsFindEleUrlsByID_PARENT do try
    close;
    params.ParamByName('ID_PARENT').AsInteger := pID;
    open;
    while not EOF do begin
      elle := Length(result);
      setLength(result, elle + 1);
      result[elle] := intToStr(fieldByName('ID').AsInteger);
      next
    end;
  finally
    close;
  end;
end;



// ==============
// d a t a
// ==============

function insertEleDataRecAndGetBackID(var pRec: TEleDataRec): integer;
// le liste (DSCR) che compaiono in una certa (ID_URL) pagina,
// quanti VOTI hanno preso
begin
  with FormSQLite.qdsInsertData do begin
    params.paramByName('DSCR'    ).asString   := pRec.DSCR;
    params.paramByName('CAND_URL').asString   := pRec.CAND_URL;
    params.paramByName('VOTI'    ).asInteger  := pRec.VOTI;
    params.paramByName('ID_URL'  ).asInteger  := pRec.ID_URL;
    execSQL;
  end;
  application.processMessages;
  FormSQLite.SQLTransaction1.commit;
  with FormSQLite.qdsFindEleDataByID_URL_DSCR do try
    // select * from ELECTION_DATA where (ID_URL=:ID_URL) and (DSCR=:DSCR)
    close;
    params.ParamByName('ID_URL').AsInteger := pRec.ID_URL;
    params.ParamByName('DSCR'  ).AsString  := pRec.DSCR;
    open;
    pRec.ID := fieldByName('ID').AsInteger;
    result := pRec.ID;
  finally
    close
  end;
end;



// ==============
// c a n d
// ==============

function insertEleCandRecAndGetBackID(var pRec: TEleCandRec): integer;
begin
  // I candidati (DSCR) legati ad certa (ID_DATA) lista,
  // quante PREFERENZE hanno preso
  with FormSQLite.qdsInsertCand do begin
    params.paramByName('DSCR'    ).asString     := pRec.DSCR;
    params.paramByName('BORN_WHERE').asString   := pRec.BORN_WHERE;
    params.paramByName('BORN_WHEN' ).asDateTime := pRec.BORN_WHEN;
    params.paramByName('ELETTO').asString       := pRec.ELETTO;
    params.paramByName('PREFERENZE').asInteger  := pRec.PREFERENZE;
    params.paramByName('ID_DATA'   ).asInteger  := pRec.ID_DATA;
    if pRec.BORN_WHEN = 0 then
      params.paramByName('BORN_WHEN' ).asString := '';
    execSQL;
  end;
  application.processMessages;
  FormSQLite.SQLTransaction1.commit;
  with FormSQLite.qdsFindEleCandByID_DATA_DSCR do try
    // select * from ELECTION_CAND where (ID_DTA = :ID_DATA) and (DSCR=:DSCR)
    close;
    params.ParamByName('ID_DATA').AsInteger := pRec.ID_DATA;
    params.ParamByName('DSCR'  ).AsString  := pRec.DSCR;
    open;
    pRec.ID := fieldByName('ID').AsInteger;
    result := pRec.ID;
  finally
    close
  end;
end;

{ TFormSQLite }

procedure TFormSQLite.FormActivate(Sender: TObject);
begin   if not _booting then exit;
  _booting := false;
  cnx.connected := true;
  SQLTransaction1.Active := True;
  qdsURLs.active := true;
end;

procedure TFormSQLite.qdsURLsBeforeOpen(DataSet: TDataSet);
begin
  qdsURLs.params.paramByName('ID_PARENT').asInteger := curr_ID_parent
end;

procedure TFormSQLite.qdsURLsCalcFields(DataSet: TDataSet);
// helper per evitare che le date vuote (0.0) vengano fuori come 31/12/1899
var d: TDate;
begin with DataSet do begin
  d := fieldByName('ELE_DATE').asDateTime;
  if d = 0 then begin
    fieldByName('sEleDate').asString := '';
  end else fieldByName('sEleDate').asString := DateToStr(d)
end end;

procedure TFormSQLite.clearAll;
// per la rigenerazione della base dati
begin
  cnx.ExecuteDirect('DELETE FROM ELECTION_URLS ', SQLTransaction1);
  cnx.ExecuteDirect('DELETE FROM ELECTION_DATA ', SQLTransaction1);
  cnx.ExecuteDirect('DELETE FROM ELECTION_CAND ', SQLTransaction1);
  SQLTransaction1.commit;
  logga('tables are now empty');
  application.processMessages;
  FormSQLite.SQLTransaction1.commit;
end;


procedure TFormSQLite.doUrlsDrillDown;
// pagina principale: chi c'è sotto questa pagina ? approfondiamo ...
begin
  with qdsURLs do try
    // let's update the next parent
    curr_ID_parent := fieldByName('ID').asInteger;
    close;
  finally
    open   // fa scattare l'evento beforeOpen, cfr qdsURLsBeforeOpen qui sopra
  end;
end;

procedure TFormSQLite.doDataDrillDown;
begin
  if qdsURLs.Active then with qdsDataOfUrl do try
    close;
    params.paramByName('ID_URL').asInteger := qdsURLs.fieldByName('ID').asInteger;
  finally
    open;
  end
end;

procedure TFormSQLite.doCandDrillDown;
begin
  if qdsDataOfUrl.Active then with qdsCandOfData do try
    close;
    params.paramByName('ID_DATA').asInteger := qdsDataOfUrl.fieldByName('ID').asInteger;
  finally
    open;
  end
end;

procedure TFormSQLite.doUrlsUp;
// pagina principale: tornare su di un livello
begin
  if curr_ID_parent = 0 then exit;   // siamo già in cima alla gerarchia delle pagine
  with SQLQueryUp do try
    close;
    // who's the parent of the current parent ?
    params.paramByName('ID').asInteger := curr_ID_parent;
    open;
    curr_ID_parent := fieldByName('ID_PARENT').asInteger;
    qdsURLs.close;
  finally
    close;
    qdsURLs.open;   // fa scattare l'evento beforeOpen, cfr qdsURLsBeforeOpen qui sopra
  end;
end;


end.

