unit etcScraping;

{$mode objfpc}{$H+}

///////////////////////////////////////////////////////////////////////////////////
//   q u i   c i   s o n o   g l i   a l g o r i t m i   d i   s c r a p i n g   //
///////////////////////////////////////////////////////////////////////////////////

interface uses Classes, SysUtils, guiSQLite;

const K_BASE_URL = 'http://elezionistorico.interno.it';

procedure doBoot(verbose: boolean = false);
procedure developNodeID(pID: integer; verbose, recursive: boolean);
procedure scrapeData(pUrlRec: TEleUrlRec; pPage: string; verbose: boolean);
procedure ScrapeCand(pEleDataRec: TEleDataRec; verbose: boolean);

implementation uses Types, Dialogs, Controls, Forms, httpsend;



// ---------------------------
// s t r i n g   u t i l s
// ---------------------------

procedure swapApos(var s: string);
// in una stringa sostituisco gli apostrofi con le virgolette e viceversa
var i: integer;
begin
  for i := 1 to length(s) do begin
    if s[i] = '''' then s[i] := '"'  else
    if s[i] = '"'  then s[i] := '''';
  end;
end;

function justDigits(s: string): integer;
// rimuove tutto ciò che non è 0÷9 prima di convertire in integer
// NON USARE SE C'è un separatore decimale !!!
var digits: string; i: integer;
begin
  digits := '';
  for i := 1 to length(s) do if (s[i] >= '0') and (s[i] <= '9') then digits := digits + s[i];
  result := strToIntDef(digits, 0)                           ;
end;

function getStringInBetween(sSource, sBegin, sEnd: string; sSkipTo: string = ''): string;
// poorman regExp
var
  iBegin, iEnd: integer;
begin
  iBegin := pos(sBegin, sSource);
  if iBegin <= 0 then begin swapApos(sBegin); iBegin := pos(sBegin, sSource) end;
  if iBegin <= 0 then raise Exception.Create('getStringBetween: can''t find |' + sbegin + '| in |' + sSource + '|');
  result := copy(sSource, iBegin + length(sBegin), length(sSource));
  iEnd := pos(sEnd, result);
  if iEnd <= 0 then begin swapApos(sEnd); iEnd := pos(sEnd, result) end;
  if iEnd < 1 then raise Exception.Create('getStringBetween: can''t find |' + sEnd + '| in |' + result + '|');
  result := copy(result, 1, iEnd - 1);
  if sSkipTo <> '' then begin
    iBegin := pos(sSkipTo, result);
    if iBegin > 0 then result := copy(result, iBegin + 1, length(result));
  end;
end;

function SplitStringOnTag(const SSource, sTag: string): TStringDynArray;
// utile dentro un tag <ul> o <table> per estrarre le sottostringhe su cui iterare
var
  iBegin, elle: Integer;
  sBefore, sAfter: string;
begin
  Result := nil; sAfter := SSource;
  repeat
    iBegin := pos(sTag, sAfter);
    if iBegin > 0 then begin
      sBefore := copy(sAfter, 1, iBegin);
      sAfter := copy(sAfter, iBegin + length(sTag), length(sAfter));
    end else sBefore := sAfter;
    elle := Length(result);
    setLength(result, elle + 1);
    result[elle] := sBefore;
  until iBegin = 0;
end;

function getPageAtUrl(const URL: string; var sl: TStringList): boolean;
// scarica la pagina e la ritorna dentro una serie di linee
var myUrl: string;
begin
  myUrl := StringReplace(URL, '&amp;', '&', [rfReplaceAll, rfIgnoreCase]);
  result := false;
  with THTTPSend.Create do try
    if HTTPMethod('GET', myURL) then try
      sl.loadFromStream(Document);
      result := true;
    except
      raise;
    end;
  finally
    free
  end;
end;



// ---------------------------
// e l e c t i o n   t y p e s
// ---------------------------

const K_ElectionTypeValues = 'ACSERPGF';   // ecco gli 8 tipi di elezione

function getEleTypeCount: integer;
begin
  result := length(K_ElectionTypeValues);
end;

function getEleTypeDescription(i: integer): string;
begin
  if (i < 0) or (i > getEleTypeCount) then
    raise Exception.Create('getElectionTypeDescription out of bound: ' + intToStr(i));
  case i of
    1: result := 'Assemblea costituente';
    2: result := 'Camera';
    3: result := 'Senato';
    4: result := 'Europee';
    5: result := 'Regionali';
    6: result := 'Provinciali';
    7: result := 'Comunali';
    8: result := 'Referendum';
  end;
end;

function getEleTypeValue(i: integer): string;
begin
  if (i < 0) or (i > getEleTypeCount) then
    raise Exception.Create('getElectionTypeValue out of bound: ' + intToStr(i));
  result := K_ElectionTypeValues[i];
end;

function getEleTypeUrl(i: integer): string;
begin
  result := K_BASE_URL + '/index.php?tpel=' + getEleTypeValue(i);
end;



// ---------------------------
// e l e c t i o n   d a t e s
// ---------------------------

function extractDatesFromSource(pSource: string; verbose: boolean = false): TStringDynArray;
var
  elle: integer;
  sSource, s, sTag: string;
begin   Result := nil;
  sSource := getStringInBetween(pSource, '<div class="lista_date">', '</div>');
  for s in SplitStringOnTag(SSource, '</li><li>') do begin
    sTag := getStringInBetween(s, 'title="', '"');
    elle := Length(result);
    setLength(result, elle + 1);
    result[elle] := sTag;
    if verbose then logga(sTag);
  end;
end;

function deSlaRev(const s: string): TDateTime;
// invece della strToDate ...
var yyyy, mm, dd: word;
begin                                         // dd/mm/yyyy
  yyyy := strToIntDef(copy(s, 7, 4), 0);
  mm   := strToIntDef(copy(s, 4, 2), 0);
  dd   := strToIntDef(copy(s, 1, 2), 0);
  try
    result := encodeDate(yyyy, mm, dd);
  except
    result := 0;
  end;
end;



// -------------------------------
// b a t c h   p r o c e d u r e s
// -------------------------------

procedure logIns(pEleUrlRec: TEleUrlRec);
begin with pEleUrlRec do logga(format('%d) ins: "%s"', [ID, DSCR]));
end;

procedure logUpd(pEleUrlRec: TEleUrlRec);
begin with pEleUrlRec do logga(format('%d) upd: "%s"', [ID, TITLE]));
end;

procedure doBoot(verbose: boolean = false);
// svuota il database e genera i primi 2 livelli della gerarchia generale:
// tipo e data elezione
// il resto della gerarchia, cioè i 4 livelli territoriali sono a parte
// generati della procedure developNodeID() più avanti
var
  i, idParent: integer; sUrl, sDate, sDscrParent: string;   sl: TStringList;
  myEleUrlRec: TEleUrlRec;
  t: TDateTime;

begin
  if messageDlg('this will empty the database !!!' + #13 +
    'PLEASE CONFIRM', mtConfirmation, [mbOK, mbCancel], 0) <> mrOK then exit;
  sl := TStringList.Create;   screen.Cursor := crHourGlass;    t := now;
  try
    FormSQLite.clearAll;
    for i := 1 to getEleTypeCount do begin
      sUrl := getEleTypeUrl(i);
      getPageAtUrl(sUrl, sl);
      with myEleUrlRec do begin
        DSCR      := getEleTypeDescription(i);
        URL       := getEleTypeUrl(i);
        ELE_TYPE  := getEleTypeValue(i);
        TITLE     := '';
        ELE_DATE  := 0;
        ID_PARENT := 0;
        ELETTORI  := 0;
        VOTANTI   := 0;
        NULLE     := 0;   DI_CUI_BIANCHE := 0;
        sDscrParent := DSCR;
      end;
      idParent := insertEleUrlRecAndGetBackID(myEleUrlRec);
      if verbose then logIns(myEleUrlRec);
      for sDate in extractDatesFromSource(sl.Text, false) do begin
        with myEleUrlRec do begin
          DSCR      := sDscrParent + ' ' + sDate;
          URL       := format('%s&amp;dtel=%s', [getEleTypeUrl(i), sDate]);
          ELE_TYPE  := getEleTypeValue(i);
          ELE_DATE  := deSlaRev(sDate);
          ID_PARENT := idParent;
        end;
        insertEleUrlRecAndGetBackID(myEleUrlRec);
        if verbose then logIns(myEleUrlRec);
      end;
    end;
  finally
    sl.Free;
    screen.Cursor := crDefault;
    logga(''); logga('DONE in ' + formatDateTime('hh:nn:ss', now - t))
  end;
end;



// ==============
// u r l s
// ==============

procedure developChildrenOfNodeID(pID: integer; verbose, recursive: boolean);
// dato il nodo pID crea tutti i nodi sottostanti, se esistono
// cfr click nella colonna di destra del sito
// es.: Assemblea costituente, Area | Circoscrizione | Provincia | Comune
// (Comune però sono FOGLIE dell'albero e NON si può drillarle)
var s: string;
begin
  for s in getEleUrlListOfID_Parent(pID) do begin
    developNodeID(strToIntDef(s, 0), verbose, recursive);
  end;
end;

procedure developPageAsChildrenOF(pID: integer; pPage: string; verbose: boolean);
// dal sorgente HTML in pPage cerco gli URL del livello successivo
// se li trovo genero nuovi nodi gerarchicamente sotto il pID nella TABLE degli URL
var sPage, s: string;
  myParentRec, myRec, myChkRec: TEleUrlRec;
begin   // 1. cercare la <div class="sezione">   2. loop tra </li><li>   3. estrarre href e title
  myParentRec := getEleUrlRecOfID(pID);
  sPage := getStringInBetween(pPage, '<div class="sezione">', '</div>');
  for s in SplitStringOnTag(sPage, '</li><li>') do begin
    myRec.ID_PARENT := pID;
    myRec.DSCR := getStringInBetween(s, 'title="', '"');
    myRec.URL  := K_BASE_URL + getStringInBetween(s, 'href="', '"');
    if myRec.URL = myParentRec.URL then begin
      // quando sono su un Comune ritrovo a destra la lista dei Comuni "peer" di questo
      if verbose then logga(format('%d) %s:   LEAF NODE', [pID, myRec.DSCR]));
      exit
    end;
    myChkRec := getEleUrlRecOfURL(myRec.URL);
    if myRec.URL = myChkRec.URL then begin
      // questo non dovrebbe mai succedere ...
      if verbose then logga(format('%d) %s:   EXISTING URL (%d)', [pID, myRec.DSCR, myChkRec.ID]));
      exit
    end;
    with myRec do begin
      ELE_TYPE := myParentRec.ELE_TYPE;
      ELE_DATE := myParentRec.ELE_DATE;
      ELETTORI := 0; VOTANTI := 0; NULLE := 0; DI_CUI_BIANCHE := 0;   // later
    end;
    insertEleUrlRecAndGetBackID(myRec);   // INSERITO NUOVO NODO nella table degli URL!
    if verbose then logIns(myRec);
  end;
end;

procedure developNodeID(pID: integer; verbose, recursive: boolean);
// procedura da cui parte lo scraping dei dati territoriali
// pID infatti deve puntare ad un nodo del 2° livello generale o un nodo territoriale
// non produce niente se lanciato su un nodo Comune
var sl: TStringList;   t: TDateTime;   myUrlRec: TEleUrlRec;
begin   // 0. solo per i nodi NON ROOT   1. [scaricare pagina]   2. sviluppare i figli
  sl := TStringList.Create;   screen.Cursor := crHourGlass;    t := now;
  myUrlRec := getEleUrlRecOfID(pID);
  with myUrlRec do try
    if (ID = 0) or (ID <> pID) then raise Exception.Create('developNodeID: not found ' + intToStr(pID));
    if ID_PARENT = 0 then raise Exception.Create('developNodeID: ROOT NODE ' + intToStr(pID));
    getPageAtUrl(URL, sl);   // scarico la pagina da Internet ... CONNECTION REQUIRED !!!
    sl.saveToFile(extractFilePath(application.exeName) + 'currPage.txt');   // DEGUB ONLY, commentabile
    developPageAsChildrenOF(ID, sl.text, verbose);
    if IS_DATA <> '' then scrapeData(myUrlRec, sl.text, verbose);   // sviluppiamo le liste ...
  finally
    sl.Free;
    screen.Cursor := crDefault;
    logga(''); logga('DONE in ' + formatDateTime('hh:nn:ss', now - t))
  end;
  if recursive then developChildrenOfNodeID(pID, verbose, recursive);   // da capo su tutte le pagine figlie
end;



// ==============
// d a t a
// ==============

procedure scrapeData(pUrlRec: TEleUrlRec; pPage: string; verbose: boolean = false);
var
  s, sData, sTag: string;
  listaIterator: integer;
  myDataRec: TEleDataRec;
begin
  pUrlRec.TITLE          :=            getStringInBetween(pPage, '<div class="titolo_pagina">', '</div>');
  pUrlRec.ELETTORI       := justDigits(getStringInBetween(pPage, 'headers="helettori">'       , '</td>')); // headers='helettori'>
  pUrlRec.VOTANTI        := justDigits(getStringInBetween(pPage, 'headers="hvotanti">'        , '</td>'));
  pUrlRec.NULLE          := justDigits(getStringInBetween(pPage, 'headers="hsknonvaliderER">' , '</td>'));
  pUrlRec.DI_CUI_BIANCHE := justDigits(getStringInBetween(pPage, 'headers="hskbianche">'      , '</td>'));
  updateEleUrlRec(pUrlRec);   myDataRec.ID_URL := pUrlRec.ID;   // record aggiornato
  if verbose then logUpd(pUrlRec);
  myDataRec.ID_URL := pUrlRec.ID;   // una tantum
  listaIterator := 0;   // vediamo se ci sono le liste elettorali e i voti (dovrebbero proprio, cfr ms=S nell'URL)
  sData :=  getStringInBetween(pPage, 'summary="Risultati elezione">', '</table>');
  for s in SplitStringOnTag(sData, '</tr><tr') do begin
    sTag := format('headers="hvoti lista%d">', [listaIterator]);
    if pos(sTag, s) > 0 then begin
      myDataRec.DSCR :=       getStringInBetween(s, 'class="candidato"', '</th>', '>');
      // toDo: la DSCR così potrebbe contenere un ulterioe URL che punta alla pagina dei candidati di lista ...
      if compareText(copy(myDataRec.DSCR, 1, 2), '<a') = 0 then begin
        myDataRec.cand_url := getStringInBetween(myDataRec.DSCR, '<a href="', '"');
        if myDataRec.cand_url[1] <> '/' then myDataRec.cand_url := '/' + myDataRec.cand_url;
        myDataRec.cand_url := K_BASE_URL + myDataRec.cand_url;
        myDataRec.DSCR := getStringInBetween(myDataRec.DSCR, 'title="', '"');
      end else myDataRec.cand_url := '';
      myDataRec.VOTI := justDigits(getStringInBetween(s, sTag, '</td>'));
      insertEleDataRecAndGetBackID(myDataRec);   // dati inseriti nella rispettiva table !
      if verbose then logga(format('%d) upd: "%s"', [myDataRec.ID, myDataRec.DSCR]));
      if myDataRec.cand_url <> '' then begin
        ScrapeCand(myDataRec, verbose);
      end;
      inc(listaIterator);
    end;
  end;
  if verbose then begin
    logga(format('--- created summary for URL ID %d with %d parties', [pUrlRec.ID, listaIterator]))
  end;
end;



// ==============
// c a n d
// ==============

procedure ScrapeCand(pEleDataRec: TEleDataRec; verbose: boolean);
var
  s, sTbl, sBegin1, sBegin2, sCand, sBornWhen, sBornWhere, sEletto, sPage: string;
  candIterator, iPreferenze: integer;
  myCandRec: TEleCandRec;
  sl: TStringList;
begin
  sl := TStringList.create;   sPage := '';
  try
    getPageAtUrl(pEleDataRec.cand_url, sl);
    sPage := sl.Text;
  finally
    sl.free;
  end;
  sBegin1 := 'headers=''hcandidato''>';
  sBegin2 := 'headers="hcandidato">';
  candIterator := 0;
  sTbl := getStringInBetween(sPage, 'summary="Elenco candidati">', '</table>');
  for s in SplitStringOnTag(sTbl, '</tr><tr') do begin
    if (pos(sBegin1, s) > 0) or (pos(sBegin2, s) > 0) then begin
      sCand       := getStringInBetween(s, sBegin1, '<');
      sBornWhen   := getStringInBetween(s, format('hdatanascita candidato%d">'  , [candIterator]), '<');
      sBornWhere  := getStringInBetween(s, format('hluogonascita candidato%d">', [candIterator]), '<');
      sEletto     := getStringInBetween(s, 'eletto">', '<');
      try
      iPreferenze := justDigits(
                     getStringInBetween(s, format('hpreferenze candidato%d">', [candIterator]), '<')
                     );
      except iPreferenze := 0 end;

      with myCandRec do begin
        ID_DATA    := pEleDataRec.ID;
        PREFERENZE := iPreferenze;
        DSCR       := sCand;
        BORN_WHEN  := deSlaRev(sBornWhen);
        BORN_WHERE := sBornWhere;
        ELETTO     := sEletto;
      end;
      insertEleCandRecAndGetBackID(myCandRec);
      if verbose then begin
        logga(format('  %d) ins: "%s"', [candIterator, sCand]));
      end;
      inc(candIterator);
    end;
  end;
end;

end.

