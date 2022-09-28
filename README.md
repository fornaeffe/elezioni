# elezioni
Simulatore per le elezioni politiche italiane

Lo script principale è elezioni.qmd.

Questo script va a cercare alcuni dati:

- elenco di liste che si presenteranno alle elezioni e loro percentuali
- risultati delle passate elezioni
- dati relativi ai collegi elettorali

in base a questi dati, simula i voti ricevuti da ogni lista in ogni collegio,
dopodiché chiama la funzione Scrutinio, presente nel file scrutinio.R, per 
stabilire i seggi attribuiti a ogni coalizione e lista in base ai voti ottenuti.

Infine presenta i risultati in un report chiamato elezioni.html.

## Descrizione dei dati di input

### File di scenario

Nella cartella dati/2022 è presente il file "liste.xlsx"
con i dati delle liste.

Il file excel di scenario deve contenere due fogli:

- il primo foglio, chiamato "liste", deve contenere una tabella con queste
colonne:

  - LISTA (stringa): nome della lista

  - ABBREV (stringa): abbreviazione della lista

  - COALIZIONE (stringa): nome della coalizione a cui la lista appartiene. Poiché
le coalizioni non hanno, di solito, un nome ufficiale, è possibile scriverci
quello che si vuole, basta che la stringa sia uguale per tutte e sole le liste
appartenenti alla stessa coalizione.

  - MINORANZA (logico): VERO soltanto se si tratta di una lista rappresentativa
di minoranze linguistiche riconosciute, presentata esclusivamente in una regione
ad autonomia speciale il cui statuto o le relative norme di attuazione prevedano una particolare tutela di tali minoranze linguistiche, altrimento FALSO.
ATTENZIONE: al momento non ho implementato la simulazione di liste presentate in
una sola regione, quindi meglio mettere FALSO a tutte le liste.

  - COLORE (numerico): tonalità del colore usato per rappresentare la lista,
da 0 a 360, nella notazione di colore HSV. Se questo campo viene lasciato vuoto
la lista non sarà elencata nelle tabelle del report, e nei grafici verrà
rappresentata con il colore grigio. Utile per indicare liste fittizie aggiunte
solo per portare il totale della colonna PERCENTUALE a 1 (100%).

  - AREA (stringa): area politica a cui la lista appartiene. Serve per collegare
le liste che si presenteranno alle prossime elezioni con le liste che si sono
presentate alle scorse elezioni politiche, regionali ed europee. Poiché non
sempre i partiti presentano una lista con lo stesso nome, a volte non si
presentano affatto o altre volte si presentano in una lista unica con altri
partiti, è necessario un modo per capire quali liste si rivolgono alla stessa
area di elettori. Di conseguenza ho assegnato (in modo SOGGETTIVO!) ciascuna
lista ad una "area politica" che mi aspetto di ritrovare pressoché in tutte
le scorse elezioni, anche se sotto liste di nome diverso. Essendo una scelta
soggettiva è opportuno che chi si appresta all'analisi riveda queste
assegnazioni e le espliciti nel riportare i risultati.

- il secondo foglio, chiamato "liste_sondaggi", deve contenere
una tabella con queste colonne:

  - SONDAGGIO (stringa): il nome del sondaggio
  
  - DATA (data): la data del sondaggio
  
  - ABBREV (stringa): l'abbreviazione della lista, 
  come scritta nel primo foglio.
  Non è necessario che siano presenti tutte le liste elencate nel primo foglio,
  e non è necessario che tutte le liste scritte qui siano presenti anche nel
  primo foglio (quindi si può mettere "altri"). Inoltre nei sondaggi
  che riportano anche la stima degli astenuti bisogna aggiungere una riga
  con "astenuti" al posto dell'abbreviazione.
  
  - PERCENTUALE (decimale): la percentuale attesa. Nel caso delle liste,
  è la percentuale rispetto al totale dei voti validi.
  Nel caso della riga "astenuti", è la percentuale di astenuti sul totale
  degli elettori. Importante che ci siano almeno due sondaggi che riportano
  gli astenuti.

### File di associazione liste - aree

Nella cartella "dati" deve essere presente un file "liste_precedenti_elezioni.xlsx".

Il file deve contenere un foglio chiamato "liste", con una tabella che
contenga almeno le seguenti colonne:

- LISTA (stringa): nome di tutte le liste presenti in almeno una delle scorse 
elezioni politiche, europee, o regionali dal 2018 a oggi, scritto esattamente
come appare scritto nei file dei risultati di quelle elezioni.

- AREA (stringa): nome dell'area a cui la lista appartiene (scelta in modo
soggettivo come descritto sopra). Il nome di ogni area deve essere sempre
scritto nello stesso modo, sia qui che nel file di scenario.

### Dati delle precedenti elezioni politiche

Il file "dati/2018/camera-20180304_2" contiene i risultati delle scorse elezioni
politiche della Camera dei Deputati, scaricati da
[Eligendo](https://elezioni.interno.gov.it/) e salvati con codifica UTF-8.

### Dati delle precedenti elezioni regionali ed europee

Nella cartella "dati/eur_reg/" sono contenuti i file con i risultati
delle elezioni europee del 2019 e delle elezioni regionali dal 2018 a oggi,
scaricati da [Eligendo](https://elezioni.interno.gov.it/) e salvati con codifica
UTF-8.

### Dati relativi ai collegi

Nella cartella "dati" è presente il file "BaseDati_Proposta_Commissione.csv"
(ricavata dalla pagina 
<https://www.riformeistituzionali.gov.it/it/i-nuovi-collegi-elettorali/>)

### Dati dei candidati

Dati sui candidati da [Elezioni trasparenti](https://dait.interno.gov.it/elezioni/trasparenza/elezioni-politiche-2022)

## Altro script: a_posteriori.qmd

Questo script è in lavorazione.
Serve a convalidare la funzione scrutinio,
stimando gli eletti a partire dai risultati reali.

Dati sugli scrutini delle elezioni da 
https://github.com/ondata/elezioni-politiche-2022
