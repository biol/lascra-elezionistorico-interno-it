lascra-elezionistorico-interno-it
=================================

implementazione in Lazarus/objectPascal di uno scraper per http://elezionistorico.interno.it 

(cfr etcScraping.pas: K_BASE_URL)

la gerarchia delle pagine, i risultati di lista e dei candidati vengono memorizzati          
nel database eleScrap.db3 che deve essere nello stesso folder dell'eseguibile eleScrap.exe   
per la successva interrogazione interativa su questa pagina mediane i bottoni                
drill down (disaggrega la url selezionata) e up (torna al livello precedente)                
                                                                                             
Testato SOLO per l'elezione dell'Assemblea Costituente                                       
                                 =====================                                       
altre elezioni mostrano aggregazioni di lista che per adesso non sono ancora supportate      
                                                                                             
esistono 8 tipi di elezione (cfr etcScraping.pas: getEleTypeDescription)                     
per ogni tipo esistono una o più date in cui si sono svolte le elezioni                      
per ogni data esiste una classificazione geografica dei dati che si articola                 
sempre in 4 livelli; il 4° livello (massima disaggregazione) è quello dei Comuni             
il 3° è quello delle provincie; il 2° e il primo variano a seconda del tipo di elezione;     
                                                                                             
il software è agnostico rispetto alla gerarchia territoriale, nel senso che ogni             
elezione (tipo / data) ha la sua gerarchia territoriale indipendente (e certamente duplicata)
                                                                                             
questa implementazione è nata PRIMA dell'hackaton del 19/01/13 a Bologna                     
e NON tiene conto della struttura SQL definita in quella sede. Ahimè !                       
                                                                                             
sorgenti in freePascal / lazarus 1.0.4 aperti a chiunque in licenza CC                       
                                                                                             
librerie:                                                                                    
=========                                                                                    
http://www.ararat.cz/synapse/ per l' http get della pagina                                   
(cfr httpsend.pas) che nelle opzioni | compilatore | percorsi | librerie                     
è indicato come residente in E:\lazarus\3p\synapse40\source\lib\                             
quindi va opportunamente MODIFICATO in base al VOSTRO setup                                  
                                                                                             
ToDo:                                                                                        
=====                                                                                        
1) Adottare la struttura della base dati definita all'hackaton (e rifare un bel po' di roba) 
2) oppure raffinare quella attuale in modo che la descrizione di liste e condidati           
sia univoca a livello di elezione                                                            
3) adattare lo scraping di liste e candidati agli altri tipi di elezione 
