# elezioni
Simulatore per le elezioni politiche italiane

Lo script principale è elezioni.Rmd.

Questo script va a cercare alcuni dati:

- elenco di liste che si presenteranno alle elezioni e loro percentuali, dalla 
cartella dati_2023
- risultati delle passate elezioni, dalle cartelle dati_2018 e 
dati_amministrative
- dati relativi ai collegi elettorali, dalla cartella dati_collegi

in base a questi dati, simula i voti ricevuti da ogni lista in ogni collegio,
dopodiché chiama la funzione Scrutinio, presente nel file scrutinio.R, per 
stabilire i seggi attribuiti a ogni coalizione e lista in base ai voti ottenuti.

Infine presenta i risultati in un report chiamato elezioni.html.

## Descrizione dei dati di input

### File di scenario

Nella cartella dati_2023/ sono presenti uno o più file excel
di scenario, il cui nome è una striga di testo che funge da nome di scenario
seguita da ".xlsx", ad esempio "01.xlsx", "02.xlsx" e così via.

All'inizio del chunk "main", nella sezione "Parametri", è possibile indicare
quale scenario considerare, impostando la variabile `scenario`. Ad esempio,
se la variabile `scenario` è "01", verrà usato il file "01.xlsx".

Il file excel di scenario deve contenere due fogli:

- il primo foglio, chiamato "liste_naz", deve contenere una tabella con queste
colonne:

  - LISTA (stringa): nome della lista

  - ABBREV (stringa): abbreviazione della lista

  - PERCENTUALE (numerico): frazione (da 0 a 1) attesa dei voti alla lista a
livello nazionale rispetto al totale di voti validi. E' la percentuale mostrata
dai sondaggi politici, e generalmente la copio proprio dal sondaggio più
recente. NOTA: il numero deve essere una frazione, non un numero tra 0 e 100.
Questo non è un problema se si usa Excel perché le percentuali mostrate da Excel
sono memorizzate come frazioni, e come tali passate a R.

  - COALIZIONE (stringa): nome della coalizione a cui la lista appartiene. Poiché
le coalizioni non hanno, di solito, un nome ufficiale, è possibile scriverci
quello che si vuole, basta che la stringa sia uguale per tutte e sole le liste
appartenenti alla stessa coalizione.

  - MINORANZA (logico): VERO soltanto se si tratta di una lista rappresentativa
di minoranze linguistiche riconosciute, presentata esclusivamente in una regione
ad autonomia speciale il cui statuto o le relative norme di attuazione prevedano una particolare tutela di tali minoranze linguistiche, altrimento FALSO.
ATTENZIONE: al momento non ho implementato la simulazione di liste presentate in
una sola regione, quindi meglio non inserire nessuna lista rappresentativa di
minoranze linguistiche.

  - FRAZ_UNI (numerico): frazione di candidati uninominali di coalizione che
potrebbero essere anche candidati plurinominali della lista. Se il totale nella
coalizione non è 1, questi valori verranno scalati in modo che il totale
faccia 1. Non sapendo a priori come le liste di una coalizione si spartiranno i
candidati uninominali, io di solito copio in questa colonna la colonna
PERCENTUALE, dando per scontato che si divideranno i candidati uninominali
proporzionalmente alla percentuale attesa di ciascuna lista.

  - FRAZ_PLURICAND (numerico): frazione di candidati nei collegi plurinominali
che sono già candidati in collegi uninominali o altri collegi plurinominali.
Io ho inserito valori basandomi sulla frazione di pluricandidature, nelle scorse
elezioni politiche, della stessa lista o di liste analoghe.

  - GRAFICI (logico): colonna attualmente non utilizzata dallo script.
Probabilmente la toglierò.

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

- il secondo foglio, chiamato "altri_dati", contiene una tabella con queste
colonne:

  - Sondaggio (stringa): una unica riga con indicata la fonte delle percentuali

  - Astensione (numerico): una unica riga con la frazione attesa di astenuti,
schede bianche o nulle sul totale degli elettori.

Per ipotizzare scenari diversi è sufficiente modificare il file di scenario,
o crearne uno nuovo (e impostare correttamente la variabile `scenario`)

### File di associazione liste - aree

Sempre nella cartella "dati_2023" deve essere presente un file "liste.xlsx".

Il file deve contenere un foglio chiamato "liste", con una tabella che
contenga almeno le seguenti colonne:

- LISTA (stringa): nome di tutte le liste presenti in almeno una delle scorse 
elezioni politiche, europee, o regionali dal 2018 a oggi, scritto esattamente
come appare scritto nei file dei risultati di quelle elezioni.

- AREA (stringa): nome dell'area a cui la lista appartiene (scelta in modo
soggettivo come descritto sopra). Il nome di ogni area deve essere sempre
scritto nello stesso modo, sia qui che nel file di scenario.

### Dati delle precedenti elezioni politiche

Il file "dati_2018/camera-20180304_2" contiene i risultati delle scorse elezioni
politiche della Camera dei Deputati, scaricati da
[Eligendo](https://elezioni.interno.gov.it/) e salvati con codifica UTF-8.

### Dati delle precedenti elezioni regionali ed europee

Nella cartella "dati_amministrative/" sono contenuti i file con i risultati
delle elezioni europee del 2019 e delle elezioni regionali dal 2018 a oggi,
scaricati da [Eligendo](https://elezioni.interno.gov.it/) e salvati con codifica
UTF-8.

### Dati relativi ai collegi

Nel file "dati_collegi/collegi.RData" sono presenti i dati relativi ai nuovi
collegi elettorali, divisi in due liste, `camera` e `senato`, ciascuna di esse
contenente i seguenti componenti:

- `$circoscrizioni`, data frame con le seguenti colonne:
  - `$CIRCOSCRIZIONE` (factor), nome della circoscrizione (o regione nel caso 
  del Senato);
  - `$POP_2011` (integer), popolazione risultante dal censimento 2011;
- `$collegi_pluri`, data frame con le seguenti colonne:
  - `$CIRCOSCRIZIONE` (factor), nome della circoscrizione (o regione nel caso 
  del Senato);
  - `$COLLEGIOPLURINOMINALE` (factor), nome del collegio plurinominale;
  - `$POP_2011` (integer), popolazione risultante dal censimento 2011;
  - `$SEGGI_PLURI` (numeric), numero di seggi da eleggere con metodo
  proporzionale;
- `$collegi_uni`, data frame con le seguenti colonne:
  - `$CIRCOSCRIZIONE` (factor), nome della circoscrizione (o regione nel caso 
  del Senato);
  - `$COLLEGIOPLURINOMINALE` (factor), nome del collegio plurinominale;
  - `$COLLEGIOUNINOMINALE` (factor), nome del collegio uninominale;
  - `$POP_2011` (integer), popolazione risultante dal censimento 2011.

Il file è stato creato attraverso lo script "conversione_collegi.R" (vedi in
fondo).

## Altri script

Oltre a questo script sono presenti script secondari, in particolare:

- elaborazione_2018_camera.R ed elaborazione_2018_senato.R utilizzano la 
funzione Scrutinio sui risultati delle precedenti elezioni politiche, per 
verificare la correttezza dei risultati.
- conversione_collegi.R è uno script che, a partire dalla base di dati della
proposta della Commissione per la definizione dei nuovi collegi elettorali 
(ricavata dalla pagina 
<https://www.riformeistituzionali.gov.it/it/i-nuovi-collegi-elettorali/>)
prepara i dataframe relativi ai collegi di Camera e Senato, e in particolare
stima il numero di seggi nei collegi plurinominali.

Dati sugli scrutini delle elezioni 2018 da 
https://github.com/ondata/elezionipolitiche2018
