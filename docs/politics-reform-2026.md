# Norme per la simulazione delle elezioni politiche secondo la riforma del 2026

Trascrizione operativa aggiornata al **16 ottobre 2026**, limitata alle disposizioni che
determinano dati, vincoli e calcoli della simulazione iniziale dei seggi nazionali di
Camera e Senato. Il testo dei due testi unici incorpora la legge 9 ottobre 2026, n. 180,
con entrata in vigore il 24 ottobre 2026. In caso di contrasto prevalgono il testo
promulgato e le rettifiche ufficiali.

- Per ordine di lavoro e verifiche: [piano operativo](politics-reform-2026-plan.md).
- Per funzioni, test e interpretazioni provvisorie: [specifica](politics-reform-2026-specification.md).
- Per le scelte statistiche: [decisioni di modellazione](politics-reform-2026-modelling.md).
- Per le lacune che impediscono uno scrutinio completo e le deroghe approvate: [registro delle lacune](politics-reform-2026-legal-gaps.md).

Le parentesi `[...]` delimitano parti estranee al perimetro. Si assumono voti e
preferenze validi e candidature già ammesse quanto ai requisiti amministrativi e alle
cause personali di ineleggibilità diverse dai vincoli anagrafici e di candidatura qui
modellati. Non sono trascritte procedure di deposito e controllo dei contrassegni,
raccolta delle firme, contenzioso, scrutinio Estero o elezioni suppletive successive. Le
regole di attribuzione dei voti e i vincoli sulle preferenze restano inclusi perché
incidono sui calcoli e sulla generazione di input validi. Le interpretazioni del
simulatore restano nella specifica.

Fonti: [legge promulgata e allegati](https://www.gazzettaufficiale.it/eli/id/2026/10/09/26G00201/sg),
[testo a fronte dei testi unici](testo_a_fronte.pdf),
[Costituzione, Presidenza della Repubblica](https://www.quirinale.it/allegati_statici/costituzione/costituzione.pdf),
[Codice civile, art. 2](https://www.normattiva.it/atto/caricaDettaglioAtto?atto.codiceRedazionale=075U0039&atto.dataPubblicazioneGazzetta=1975-03-10&tipoDettaglio=vigente).
Per i cataloghi territoriali completi e i dati censuari, usare le tabelle ufficiali
richiamate dalle norme; i dati già disponibili non vanno ricopiati come nuove regole.

## Costituzione della Repubblica italiana

### Art. 48, primo comma

Sono elettori tutti i cittadini, uomini e donne, che hanno raggiunto la maggiore età.

### Art. 56

La Camera dei deputati è eletta a suffragio universale e diretto.

Il numero dei deputati è di quattrocento, otto dei quali eletti nella circoscrizione
Estero.

Sono eleggibili a deputati tutti gli elettori che nel giorno della elezione hanno
compiuto i venticinque anni di età.

La ripartizione dei seggi tra le circoscrizioni, fatto salvo il numero dei seggi
assegnati alla circoscrizione Estero, si effettua dividendo il numero degli abitanti
della Repubblica, quale risulta dall'ultimo censimento generale della popolazione, per
trecentonovantadue e distribuendo i seggi in proporzione alla popolazione di ogni
circoscrizione, sulla base dei quozienti interi e dei più alti resti.

### Art. 57

Il Senato della Repubblica è eletto a base regionale, salvi i seggi assegnati alla
circoscrizione Estero.

Il numero dei senatori elettivi è di duecento, quattro dei quali eletti nella
circoscrizione Estero.

Nessuna Regione o Provincia autonoma può avere un numero di senatori inferiore a tre; il
Molise ne ha due, la Valle d'Aosta uno.

La ripartizione dei seggi tra le Regioni o le Province autonome, previa applicazione
delle disposizioni del precedente comma, si effettua in proporzione alla loro
popolazione, quale risulta dall'ultimo censimento generale, sulla base dei quozienti
interi e dei più alti resti.

### Art. 58

I senatori sono eletti a suffragio universale e diretto.

Sono eleggibili a senatori gli elettori che hanno compiuto il quarantesimo anno.

## Codice civile

### Art. 2, primo comma, primo periodo

La maggiore età è fissata al compimento del diciottesimo anno.

## Camera dei deputati

DPR 30 marzo 1957, n. 361

### Art. 1

1. La Camera dei deputati è eletta a suffragio universale, con voto diretto ed eguale,
libero e segreto, espresso in favore di liste, anche coalizzate tra loro, di candidati,
nei confronti dei quali, ad eccezione del capolista, l'elettore può esprimere fino a tre
preferenze.

2. Il territorio nazionale è diviso nelle circoscrizioni elettorali indicate nella
tabella A allegata al presente testo unico.

3. Le circoscrizioni sono ripartite nei collegi plurinominali di cui alla tabella A.2
allegata al decreto legislativo 23 dicembre 2020, n. 177.

4. Salvi i seggi assegnati alla circoscrizione Estero e fermo restando quanto previsto
per le circoscrizioni di cui all’articolo 2, i seggi sono ripartiti tra le liste e le
coalizioni di liste con metodo proporzionale, ai sensi degli articoli 83 e 83-bis, con
l’eventuale attribuzione di un premio di governabilità da assegnare, alle condizioni
previste dall’articolo 83, alla lista singola o alla coalizione di liste che abbia
ottenuto il maggior numero di voti validi a livello nazionale in entrambe le Camere e
che abbia conseguito almeno il 42 per cento di voti validi in ciascuna di esse. Il
premio di governabilità è assegnato, nelle diverse circoscrizioni, a liste
circoscrizionali appositamente presentate, ai sensi dell’articolo 18-bis, comma 2-bis,
per un totale di settanta seggi complessivi. Alla lista singola o alla coalizione di
liste assegnataria del premio di governabilità non possono comunque essere attribuiti
più di duecentoventi seggi complessivi, ad esclusione dei seggi conseguiti nella
circoscrizione Estero.

### Art. 2

1. Le elezioni nelle circoscrizioni Valle d’Aosta/Vallée d’Aoste e Trentino-Alto
Adige/Südtirol sono regolate dalle norme contenute nel titolo VI del presente testo
unico. La circoscrizione Valle d’Aosta/Vallée d’Aoste è costituita in un unico collegio
uninominale. La circoscrizione Trentino-Alto Adige/Südtirol è ripartita nei collegi
uninominali di cui alla tabella A.1 – Regione: Trentino-Alto Adige/Südtirol –
Circoscrizione: Trentino-Alto Adige/Südtirol, allegata al decreto legislativo 23
dicembre 2020, n. 177; i restanti seggi sono assegnati con metodo proporzionale in un
unico collegio plurinominale, secondo quanto disposto dall’articolo 93-septies, comma 2.
I voti espressi nelle circoscrizioni di cui al presente articolo sono computati nella
determinazione delle cifre elettorali nazionali delle liste e delle coalizioni di liste
nonché del totale dei voti validi nazionali esclusivamente ai fini indicati
dall’articolo 83, comma 1-sexies. I seggi attribuiti nelle medesime circoscrizioni sono
computati ai fini della verifica di cui all’articolo 83, comma 1, lettera f-bis).

2. Con riferimento alla circoscrizione Valle d’Aosta/Vallée d’Aoste, con il termine
“lista” si intende il partito o gruppo politico organizzato che abbia depositato il
contrassegno ai sensi dell’articolo 14, cui i candidati nel collegio uninominale della
circoscrizione sono tenuti a collegarsi ai fini di cui all’articolo 83, comma 1-sexies,
anche nel caso in cui tale partito o gruppo politico organizzato non presenti
candidature in altre circoscrizioni.

### Art. 3

1. Con decreto del Presidente della Repubblica, su proposta del Ministro dell’interno,
da emanare contestualmente al decreto di convocazione dei comizi, sulla base dei
risultati riportati nell’ultimo decreto del Presidente della Repubblica che fissa, a
scadenza quinquennale e ai soli fini elettorali, i dati del censimento permanente della
popolazione a livello comunale, sono effettuate:

a) l’assegnazione del numero dei seggi alle singole circoscrizioni di cui alla tabella A
allegata al presente testo unico;

b) la distribuzione tra le circoscrizioni di cui alla medesima tabella A, a esclusione
delle circoscrizioni di cui all’articolo 2, di un numero complessivo di settanta seggi,
da assegnare quale premio di governabilità, alle condizioni previste dall’articolo 83.

2. Con il decreto del Presidente della Repubblica di cui al comma 1, per ciascuna
circoscrizione di cui alla tabella A allegata al presente testo unico, a esclusione
delle circoscrizioni di cui all’articolo 2, sulla base dei risultati riportati
nell’ultimo decreto del Presidente della Repubblica che fissa, a scadenza quinquennale e
ai soli fini elettorali, i dati del censimento permanente della popolazione a livello
comunale, sono altresì determinate:

a) per il caso di attribuzione del premio di governabilità, la distribuzione tra i
collegi plurinominali di ciascuna circoscrizione dei seggi a questa spettanti ai sensi
della distribuzione di cui al comma 1, lettera a), detratti quelli a questa spettanti ai
sensi della distribuzione di cui al comma 1, lettera b);

b) per il caso di mancata attribuzione del premio di governabilità, la distribuzione tra
i collegi plurinominali di ciascuna circoscrizione di tutti i seggi a questa spettanti
ai sensi della distribuzione di cui al comma 1, lettera a).

### Art. 4

[...]

2. Ogni elettore dispone di un voto da esprimere su un'unica scheda recante il
contrassegno di ciascuna lista, corredato dei nomi dei candidati nel collegio
plurinominale e dei candidati delle liste circoscrizionali presentate ai fini
dell'eventuale attribuzione del premio di governabilità. Ogni elettore, inoltre, può
esprimere fino a tre preferenze in favore di candidati nel collegio plurinominale della
lista votata tra quelli che non sono capolista. Il voto di preferenza si esprime
tracciando un segno nel quadrato posto a fianco del nome e cognome del candidato
prescelto, riportato nell'apposita colonna a fianco del contrassegno di lista. Nel caso
di più preferenze espresse, queste devono riguardare candidati di sesso diverso, pena
l'annullamento della seconda e della terza preferenza nell'ordine di lista.

[...]

### Art. 14-bis

1. I partiti o i gruppi politici organizzati possono dichiarare il collegamento in una
coalizione delle liste da essi rispettivamente presentate. Le dichiarazioni di
collegamento devono essere reciproche.

2. [...] Le dichiarazioni di collegamento hanno effetto per tutte le liste aventi lo
stesso contrassegno.

[...]

3-bis. Nel medesimo atto del deposito del contrassegno, i partiti o i gruppi politici
organizzati, che si presentano singolarmente o collegati tra loro ai sensi del comma 1,
se intendono concorrere all’attribuzione del premio di governabilità di cui all’articolo
1, comma 4, dichiarano altresì il collegamento con partiti o gruppi politici organizzati
che si presentano, singolarmente o collegati tra loro, per l’elezione del Senato della
Repubblica, ai sensi dell’articolo 8 del testo unico delle leggi recanti norme per
l’elezione del Senato della Repubblica, di cui al decreto legislativo 20 dicembre 1993,
n. 533.

[...]

4-bis. Gli adempimenti di cui ai commi 3 e 3-bis si applicano ai partiti o gruppi
politici organizzati rappresentativi di minoranze linguistiche riconosciute, che
intendono presentare candidature esclusivamente in una regione ad autonomia speciale il
cui statuto o le relative norme di attuazione prevedano una particolare tutela di tali
minoranze linguistiche, solo ove tali partiti o gruppi politici abbiano dichiarato il
collegamento in una coalizione, ai sensi dei commi 1 e 2, con partiti o gruppi politici
che intendono presentare liste in altre circoscrizioni.

[...]

### Art. 18-bis

1. [...] Ciascuna lista deve presentare candidature in almeno la metà delle
circoscrizioni di cui alla tabella A allegata al presente testo unico, ad esclusione
delle liste o dei candidati collegati a liste rappresentative di minoranze linguistiche
riconosciute, presentati esclusivamente in una regione ad autonomia speciale il cui
statuto o le relative norme di attuazione prevedano una particolare tutela di tali
minoranze linguistiche. Ciascuna lista deve presentare candidature in almeno due terzi
dei collegi plurinominali della circoscrizione, a pena di inammissibilità.

[...]

2-bis. Le liste singole e le coalizioni di liste che si siano presentate ai sensi del
comma 1 presentano altresì, in ciascuna circoscrizione in cui si siano presentate,
limitatamente a quelle cui spettino seggi da assegnare quale premio di governabilità ad
esito della distribuzione di cui all'articolo 3, comma 1, lettera b), liste di candidati
circoscrizionali ai fini dell'attribuzione del premio di governabilità. In caso di
coalizione di liste, le liste circoscrizionali di cui al primo periodo sono le medesime
per la coalizione nel suo complesso e sono presentate singolarmente da ciascuna delle
liste coalizzate ai sensi dell'articolo 14-bis, comma 1, che si siano presentate nella
circoscrizione. [...] Ciascuna lista singola o coalizione di liste che intende
concorrere all'attribuzione del premio di governabilità è tenuta a presentare un numero
di candidati circoscrizionali pari al numero dei seggi da attribuire nella
circoscrizione, quale premio di governabilità, ai sensi dell'articolo 3, comma 1,
lettera b), a pena di inammissibilità della lista circoscrizionale presentata ai fini
dell'attribuzione del premio di governabilità.

[...]

2-ter. Nel caso di elezioni per il rinnovo della sola Camera dei deputati, i partiti o
gruppi politici organizzati presentano esclusivamente liste di candidati per
l’attribuzione dei seggi nei collegi plurinominali ai sensi del comma 1 e non presentano
liste di candidati circoscrizionali ai fini dell’attribuzione del premio di
governabilità ai sensi del comma 2-bis.

3. In ogni collegio plurinominale ciascuna lista, all'atto della presentazione, è
composta da un elenco di candidati presentati secondo un ordine numerico. Il numero dei
candidati è, in ogni caso, pari a sette, compreso il capolista. A pena di
inammissibilità, i candidati, compreso il capolista, sono collocati secondo un ordine
alternato di genere.

3.1. Nel complesso delle candidature delle liste circoscrizionali di cui al comma 2-bis
presentate da ogni lista o coalizione di liste a livello nazionale, nessuno dei due
generi può essere rappresentato in misura superiore al 60 per cento, con arrotondamento
all'unità più prossima. [...]

### Art. 19

1. Nessun candidato può presentarsi con diversi contrassegni nei collegi plurinominali o
nelle liste circoscrizionali presentate ai fini dell’attribuzione del premio di
governabilità, a pena di nullità.

2. Nessun candidato può essere incluso in liste con lo stesso contrassegno in più di
cinque collegi plurinominali, in posizione di capolista o in una diversa posizione, a
pena di nullità.

3. Nessun candidato può essere incluso in più di una lista circoscrizionale presentata
ai fini dell’attribuzione del premio di governabilità collegata alle medesime liste o
coalizioni, a pena di nullità.

4. Il candidato nelle liste circoscrizionali presentate ai fini dell’attribuzione del
premio di governabilità è tenuto a candidarsi, in una lista presentata nei collegi
plurinominali che sia collegata alla lista circoscrizionale in cui è candidato, in
almeno un collegio plurinominale della circoscrizione in cui è presentata la lista
circoscrizionale in cui è candidato, in posizione di capolista, ovvero nelle posizioni
successive a quella di capolista a condizione che anche tutti i candidati che lo
precedono nella medesima lista di collegio plurinominale siano candidati nelle liste
circoscrizionali presentate ai fini dell’attribuzione del premio di governabilità. Fermo
restando quanto disposto dal primo periodo, il candidato nelle liste circoscrizionali
presentate ai fini dell’attribuzione del premio di governabilità può essere candidato,
con il medesimo contrassegno, anche in circoscrizioni diverse da quella di cui al primo
periodo, in collegi plurinominali, fino ad un massimo di cinque, ivi comprese le
candidature in collegi plurinominali di cui al medesimo primo periodo.

5. Il candidato nella circoscrizione Estero non può essere candidato in nessuna delle
liste di cui all’articolo 18-bis.

6. Nessun candidato può accettare la candidatura contestuale alla Camera dei deputati e
al Senato della Repubblica, a pena di nullità.

[...]

### Art. 58

[...]

Nei casi in cui il segno sia tracciato solo nel rettangolo contenente i nomi dei
candidati delle liste circoscrizionali presentate ai fini dell’attribuzione del premio
di governabilità, i voti sono validi a favore della lista. Nel caso di più liste
collegate in coalizione, i voti sono ripartiti tra le liste della coalizione in
proporzione ai voti ottenuti da ciascuna nel collegio plurinominale.

### Art. 77

1. L’Ufficio centrale circoscrizionale, compiute le operazioni di cui all’articolo 76,
facendosi assistere, ove lo ritenga opportuno, da uno o più esperti scelti dal
presidente:

c) determina la cifra elettorale di collegio plurinominale di ciascuna lista. Tale cifra
è data dalla somma dei voti validi conseguiti dalla lista stessa nelle singole sezioni
elettorali del collegio plurinominale e dei voti espressi nel solo rettangolo contenente
i nomi dei candidati delle liste circoscrizionali presentate ai fini dell'attribuzione
del premio di governabilità collegati a più liste in coalizione di cui all'articolo 58,
terzo comma, ultimo periodo, che sono attribuiti alla lista a seguito delle seguenti
operazioni: l'Ufficio divide il totale dei voti validi conseguiti da tutte le liste
della coalizione nel collegio plurinominale per il numero dei voti espressi a favore dei
soli candidati delle liste circoscrizionali presentate ai fini dell'attribuzione del
premio di governabilità, ottenendo il quoziente di ripartizione. Divide poi il totale
dei voti validi conseguiti da ciascuna lista per tale quoziente. La parte intera del
quoziente così ottenuto rappresenta il numero dei voti da assegnare a ciascuna lista; i
voti che rimangono ancora da attribuire sono rispettivamente assegnati alle liste per le
quali queste ultime divisioni abbiano dato i maggiori resti, secondo l'ordine
decrescente dei resti medesimi;

d) per ciascuna lista determina la cifra elettorale individuale di collegio
plurinominale di ciascun candidato, costituita dal numero di voti di preferenza validi
conseguiti nelle sezioni elettorali del collegio plurinominale. Determina quindi la
graduatoria dei candidati di ciascuna lista, escluso il capolista, sulla base delle
rispettive cifre individuali. A parità di cifre individuali, prevale l'ordine di
presentazione nella lista.

e) determina la cifra elettorale percentuale di collegio plurinominale di ciascuna
lista. Tale cifra è data dal quoziente risultante dalla divisione della cifra elettorale
di collegio plurinominale di ciascuna lista per il totale dei voti validi del rispettivo
collegio plurinominale, moltiplicato per cento;

e-bis) determina la cifra elettorale individuale percentuale di collegio plurinominale
di ciascun candidato. Tale cifra è data dal quoziente risultante dalla divisione della
cifra elettorale individuale di collegio plurinominale di ciascun candidato per il
totale dei voti validi del rispettivo collegio plurinominale, moltiplicato per cento;

f) determina la cifra elettorale circoscrizionale di ciascuna lista. Tale cifra è data
dalla somma delle cifre elettorali di collegio plurinominale della lista stessa;

i) determina il totale dei voti validi della circoscrizione. Tale totale è dato dalla
somma delle cifre elettorali circoscrizionali di tutte le liste;

l) comunica all’Ufficio centrale nazionale, a mezzo di estratto del verbale, la cifra
elettorale circoscrizionale di ciascuna lista nonché il totale dei voti validi della
circoscrizione.

[...]

### Art. 83

1. L’Ufficio centrale nazionale, ricevuti gli estratti dei verbali da tutti gli Uffici
centrali circoscrizionali e fermo restando quanto previsto dal comma 1-sexies, facendosi
assistere, ove lo ritenga opportuno, da uno o più esperti scelti dal presidente:

a) determina la cifra elettorale nazionale di ciascuna lista. Tale cifra è data dalla
somma delle cifre elettorali circoscrizionali conseguite nelle singole circoscrizioni
dalle liste aventi il medesimo contrassegno;

b) determina il totale nazionale dei voti validi. Esso è dato dalla somma delle cifre
elettorali circoscrizionali di tutte le liste;

c) determina la cifra elettorale nazionale di ciascuna coalizione di liste. Tale cifra è
data dalla somma delle cifre elettorali nazionali delle liste collegate in coalizione;

d) determina la cifra elettorale circoscrizionale di ciascuna coalizione di liste. Tale
cifra è data dalla somma delle cifre elettorali circoscrizionali delle liste collegate
tra loro in coalizione;

d-bis) calcola i valori di cui alle lettere a) e c) escludendo i voti espressi nelle
circoscrizioni Valle d’Aosta/Vallée d’Aoste e Trentino-Alto Adige/Südtirol, ai fini
della ripartizione dei seggi assegnati nella restante parte del territorio nazionale di
cui alle lettere f) e g) del presente comma e ai commi 1-bis e 1-ter;

e) individua quindi le coalizioni di liste e le liste ammesse alla ripartizione dei
seggi, identificandole nelle seguenti:

1) le coalizioni di liste che abbiano conseguito sul piano nazionale almeno il 10 per
cento dei voti validi espressi e che comprendano almeno una lista collegata che abbia
conseguito sul piano nazionale almeno il 3 per cento dei voti validi espressi ovvero una
lista collegata rappresentativa di minoranze linguistiche riconosciute, presentata
esclusivamente in una regione ad autonomia speciale il cui statuto o le relative norme
di attuazione prevedano una particolare tutela di tali minoranze linguistiche, che abbia
conseguito almeno il 20 per cento dei voti validi espressi nella regione medesima;

2) le singole liste non collegate, o collegate in coalizioni che non abbiano raggiunto
la percentuale di cui al numero 1), che abbiano conseguito sul piano nazionale almeno il
3 per cento dei voti validi espressi, nonché le singole liste non collegate in
coalizione, rappresentative di minoranze linguistiche riconosciute, presentate
esclusivamente in una regione ad autonomia speciale il cui statuto o le relative norme
di attuazione prevedano una particolare tutela di tali minoranze linguistiche, che
abbiano conseguito almeno il 20 per cento dei voti validi espressi nella regione
medesima o i cui candidati siano stati proclamati eletti in almeno un quarto dei collegi
uninominali della circoscrizione, con arrotondamento all’unità superiore;

2-bis) le liste collegate con altre nelle coalizioni di cui al numero 1), che abbiano
conseguito sul piano nazionale almeno il 3 per cento dei voti validi espressi, nonché le
liste collegate in coalizione rappresentative di minoranze linguistiche riconosciute,
presentate esclusivamente in una regione ad autonomia speciale il cui statuto o le
relative norme di attuazione prevedano una particolare tutela di tali minoranze
linguistiche, che abbiano conseguito almeno il 20 per cento dei voti validi espressi
nella regione medesima;

2-ter) per ciascuna coalizione di liste di cui al numero 1), la lista che abbia
conseguito la maggiore cifra elettorale nazionale tra quelle non ammesse alla
ripartizione dei seggi ai sensi del numero 2-bis);

e-bis) comunica all'Ufficio centrale circoscrizionale della circoscrizione Trentino-Alto
Adige/Südtirol le cifre elettorali circoscrizionali delle coalizioni di liste calcolate
ai sensi della lettera d), nonché le liste e le coalizioni ammesse alla ripartizione dei
seggi ai sensi della lettera e);

e-ter) individua quindi tra le liste singole e le coalizioni di liste di cui alla
lettera e), numeri 1) e 2), quella che ha ottenuto la maggiore cifra elettorale
nazionale;

e-quater) verifica se la cifra elettorale nazionale della lista singola o della
coalizione di liste che ha ottenuto la maggiore cifra elettorale nazionale, individuata
ai sensi della lettera e-ter), corrisponda ad almeno il 42 per cento del totale
nazionale dei voti validi;

e-quinquies) qualora la verifica di cui alla lettera e-quater) abbia dato esito
positivo, verifica se nell'elezione del Senato della Repubblica la lista singola o la
coalizione di liste che ha ottenuto la maggiore cifra elettorale nazionale, individuata
ai sensi dell'articolo 16-bis, comma 1, lettera e-bis), del testo unico delle leggi
recanti norme per l’elezione del Senato della Repubblica di cui al decreto legislativo
20 dicembre 1993, n. 533, sia quella collegata, ai sensi dell'articolo 14-bis, comma
3-bis, del presente testo unico, alla lista singola o alla coalizione di liste che ha
ottenuto la maggiore cifra elettorale nazionale individuata ai sensi della lettera
e-ter) del presente comma, e verifica altresì se essa abbia conseguito almeno il 42 per
cento del totale nazionale dei voti validi espressi nell'elezione del Senato della
Repubblica;

e-sexies) qualora le verifiche di cui alla lettera e-quinquies) abbiano dato
congiuntamente esito positivo, assegna alla lista singola o alla coalizione di liste che
ha ottenuto la maggiore cifra elettorale nazionale, individuata ai sensi della lettera
e-ter), il premio di governabilità di cui all’articolo 1, comma 4, pari a settanta
seggi, e procede ai sensi della lettera f);

e-septies) qualora la verifica di cui alla lettera e-quater) abbia dato esito negativo,
ovvero una o entrambe le verifiche di cui alla lettera e-quinquies) abbiano dato esito
negativo, procede ai sensi del comma 1-bis;

f) procede al riparto provvisorio dei seggi di cui all’articolo 3, comma 2, lettera a),
tra le coalizioni di liste e le singole liste di cui alla lettera e), numeri 1) e 2),
del presente comma in base alla cifra elettorale nazionale di ciascuna di queste,
determinata ai sensi della lettera d-bis) del presente comma. A tale fine divide il
totale delle cifre elettorali nazionali di tali coalizioni di liste e singole liste per
il numero dei seggi da attribuire, ottenendo così il quoziente elettorale nazionale.
Nell’effettuare tale divisione non tiene conto dell’eventuale parte frazionaria del
quoziente. Divide poi la cifra elettorale nazionale di ciascuna coalizione di liste o
singola lista per tale quoziente. La parte intera del quoziente così ottenuto
rappresenta il numero dei seggi da assegnare a ciascuna coalizione di liste o singola
lista. I seggi che rimangono ancora da attribuire sono rispettivamente assegnati alle
coalizioni di liste o singole liste per le quali queste ultime divisioni abbiano dato i
maggiori resti, secondo l’ordine decrescente dei resti medesimi, e, in caso di parità di
resti, a quelle che abbiano conseguito la maggiore cifra elettorale nazionale; a parità
di quest’ultima si procede a sorteggio;

f-bis) verifica se il totale dei seggi assegnati alla coalizione o alla lista singola
cui è stato attribuito il premio di governabilità, ivi compresi quelli assegnati ai
sensi della lettera f), sia superiore a duecentoventi seggi complessivi; in questo
totale sono compresi i seggi assegnati a tale coalizione o lista singola nelle
circoscrizioni Valle d'Aosta/Vallée d'Aoste e Trentino-Alto Adige/Südtirol, come
risultanti dalle comunicazioni di cui agli articoli 93, comma 3-bis, lettera c), e
93-septies, comma 3;

f-ter) qualora la verifica di cui alla lettera f-bis) abbia dato esito negativo,
conferma il riparto dei seggi effettuato ai sensi della lettera f) e prosegue ai sensi
delle lettere g) e seguenti; qualora la verifica di cui alla lettera f-bis) abbia dato
esito positivo, prosegue ai sensi del comma 1-ter;

g) procede, per ciascuna coalizione di liste, al riparto dei seggi fra le liste
collegate di cui alla lettera e), numeri 2-bis) e 2-ter). A tal fine divide la somma
delle cifre elettorali nazionali di tali liste, determinate ai sensi della lettera
d-bis), per il numero di seggi già individuato ai sensi della lettera f).
Nell’effettuare tale divisione non tiene conto dell’eventuale parte frazionaria del
quoziente così ottenuto. Divide poi la cifra elettorale nazionale di ciascuna lista
ammessa al riparto per tale quoziente. La parte intera del quoziente così ottenuto
rappresenta il numero dei seggi da assegnare a ciascuna lista. I seggi che rimangono
ancora da attribuire sono rispettivamente assegnati alle liste per le quali queste
ultime divisioni abbiano dato i maggiori resti e, in caso di parità di resti, alle liste
che abbiano conseguito la maggiore cifra elettorale nazionale; a parità di quest’ultima
si procede a sorteggio;

h) procede quindi alla distribuzione nelle singole circoscrizioni dei seggi assegnati
alle coalizioni di liste o singole liste ai sensi della lettera f). A tale fine divide
la somma delle cifre elettorali circoscrizionali delle coalizioni di liste e delle
singole liste ammesse al riparto per il numero di seggi da attribuire nella
circoscrizione, di cui all’articolo 3, comma 2, lettera a), ottenendo così il quoziente
elettorale circoscrizionale. Nell’effettuare tale divisione non tiene conto
dell’eventuale parte frazionaria del quoziente così ottenuto. Divide poi la cifra
elettorale circoscrizionale di ciascuna coalizione di liste o singola lista per il
quoziente elettorale circoscrizionale, ottenendo così il quoziente di attribuzione. La
parte intera del quoziente di attribuzione rappresenta il numero dei seggi da assegnare
a ciascuna coalizione di liste o singola lista. I seggi che rimangono ancora da
attribuire sono rispettivamente assegnati alle coalizioni di liste o singole liste per
le quali queste ultime divisioni hanno dato le maggiori parti decimali e, in caso di
parità, alle coalizioni di liste o singole liste che hanno conseguito la maggiore cifra
elettorale nazionale; a parità di quest’ultima si procede a sorteggio. Esclude
dall’attribuzione di cui al periodo precedente le coalizioni di liste o singole liste
alle quali è stato già attribuito il numero di seggi ad esse assegnato a seguito delle
operazioni di cui alla lettera f). Successivamente l’Ufficio accerta se il numero dei
seggi assegnati in tutte le circoscrizioni a ciascuna coalizione di liste o singola
lista corrisponda al numero di seggi determinato ai sensi della lettera f).
<!-- Periodo nono: -->
In caso
negativo, procede alle seguenti operazioni, iniziando dalla coalizione di liste o
singola lista che abbia il maggior numero di seggi eccedenti e, in caso di parità di
seggi eccedenti da parte di più coalizioni di liste o singole liste, da quella che abbia
ottenuto la maggiore cifra elettorale nazionale, proseguendo poi con le altre coalizioni
di liste o singole liste in ordine decrescente di seggi eccedenti: sottrae i seggi
eccedenti alla coalizione di liste o singola lista nelle circoscrizioni nelle quali essa
li ha ottenuti con le parti decimali dei quozienti di attribuzione, secondo il loro
ordine crescente, e nelle quali inoltre le coalizioni di liste o singole liste, che non
abbiano ottenuto il numero di seggi spettante, abbiano parti decimali dei quozienti non
utilizzate. Conseguentemente, assegna i seggi a tali coalizioni di liste o singole
liste. Qualora nella medesima circoscrizione due o più coalizioni di liste o singole
liste abbiano parti decimali dei quozienti non utilizzate, il seggio è attribuito alla
coalizione di liste o alla singola lista con la più alta parte decimale del quoziente
non utilizzata o, in caso di parità, a quella con la maggiore cifra elettorale
nazionale. Nel caso in cui non sia possibile attribuire il seggio eccedentario nella
medesima circoscrizione, in quanto non vi siano coalizioni di liste o singole liste
deficitarie con parti decimali di quozienti non utilizzate, l’Ufficio prosegue, per la
stessa coalizione di liste o singola lista eccedentaria, nell’ordine dei decimali
crescenti, a individuare un’altra circoscrizione, fino a quando non sia possibile
sottrarre il seggio eccedentario e attribuirlo ad una coalizione di liste o singola
lista deficitaria nella medesima circoscrizione. Nel caso in cui non sia possibile fare
riferimento alla medesima circoscrizione ai fini del completamento delle operazioni
precedenti, fino a concorrenza dei seggi ancora da cedere, alla coalizione di liste o
singola lista eccedentaria vengono sottratti i seggi nelle circoscrizioni nelle quali li
ha ottenuti con le minori parti decimali del quoziente di attribuzione e alla coalizione
di liste o singola lista deficitaria sono conseguentemente attribuiti seggi nelle altre
circoscrizioni nelle quali abbia le maggiori parti decimali del quoziente di
attribuzione non utilizzate;

i) procede quindi all’attribuzione nelle singole circoscrizioni dei seggi spettanti alle
liste di ciascuna coalizione. A tale fine, determina il quoziente circoscrizionale di
ciascuna coalizione di liste dividendo il totale delle cifre elettorali circoscrizionali
delle liste ammesse alla ripartizione ai sensi della lettera e), numeri 2-bis) e 2-ter),
per il numero dei seggi assegnati alla coalizione nella circoscrizione ai sensi della
lettera h). Nell’effettuare la divisione di cui al periodo precedente non tiene conto
dell’eventuale parte frazionaria del quoziente. Divide quindi la cifra elettorale
circoscrizionale di ciascuna lista della coalizione per tale quoziente circoscrizionale.
La parte intera del quoziente così ottenuto rappresenta il numero dei seggi da assegnare
a ciascuna lista. I seggi che rimangono ancora da attribuire sono assegnati alle liste
seguendo la graduatoria decrescente delle parti decimali dei quozienti così ottenuti; in
caso di parità, sono attribuiti alle liste con la maggiore cifra elettorale
circoscrizionale; a parità di quest’ultima, si procede a sorteggio. Esclude
dall’attribuzione di cui al periodo precedente le liste alle quali è stato attribuito il
numero di seggi ad esse assegnato a seguito delle operazioni di cui alla lettera g).
Successivamente l’ufficio accerta se il numero dei seggi assegnati in tutte le
circoscrizioni a ciascuna lista corrisponda al numero dei seggi ad essa attribuito ai
sensi della lettera g). In caso negativo, con riferimento alle liste eccedentarie e alle
liste deficitarie, procede ai sensi della lettera h), periodi nono e seguenti.

1-bis. Nei casi di cui alla lettera e-septies) del comma 1, l’Ufficio centrale nazionale
distribuisce i seggi di cui all’articolo 3, comma 2, lettera b), secondo quanto disposto
ai sensi delle lettere f), g), h) ed i) del comma 1 del presente articolo;

1-ter. Qualora la verifica di cui alla lettera f-bis) del comma 1 abbia dato esito
positivo, l'Ufficio centrale nazionale, ferma restando l'applicazione del premio di
governabilità ai sensi del comma 1, lettera e-sexies), procede come segue:

a) assegna alla coalizione o alla lista singola cui è stato attribuito il premio di
governabilità un numero di seggi pari alla differenza tra il numero centocinquanta e il
numero dei seggi attribuiti alla medesima coalizione o alla lista singola nelle
circoscrizioni Valle d'Aosta/Vallée d'Aoste e Trentino-Alto Adige/Südtirol; divide
quindi la cifra elettorale nazionale di tale coalizione o lista singola per il numero di
seggi così determinato, ottenendo così il quoziente elettorale nazionale di maggioranza;
nell'effettuare tale divisione non tiene conto della parte frazionaria del quoziente;

b) procede ad un nuovo riparto ai sensi della lettera f) del comma 1, relativamente ai
soli seggi spettanti alle coalizioni e alle liste singole di cui ai numeri 1) e 2) della
lettera e) del comma 1 diverse da quella cui è stato attribuito il premio di
governabilità, pari alla differenza tra il numero 314 e il numero di seggi calcolato ai
sensi della lettera a). A questo fine divide il totale delle loro cifre elettorali
nazionali per il numero di seggi determinato ai sensi del primo periodo, ottenendo il
quoziente elettorale nazionale di minoranza. Nell'effettuare tale divisione non tiene
conto dell'eventuale parte frazionaria del quoziente. Divide poi la cifra elettorale di
ciascuna coalizione di liste o singola lista per tale quoziente. La parte intera del
quoziente così ottenuto rappresenta il numero dei seggi da assegnare a ciascuna
coalizione di liste o singola lista. I seggi che rimangono ancora da attribuire sono
rispettivamente assegnati alle coalizioni di liste o alle liste singole per le quali
queste ultime divisioni abbiano dato i maggiori resti e, in caso di parità di resti, a
quelle che abbiano conseguito la maggiore cifra elettorale nazionale; a parità di
quest'ultima, si procede a sorteggio;

c) procede ai sensi della lettera g) del comma 1, ripartendo tra le liste tra loro
coalizzate i seggi spettanti a ciascuna coalizione ai sensi delle lettere a) e b) del
presente comma;

d) procede quindi alla distribuzione nelle singole circoscrizioni dei seggi assegnati
alle coalizioni e liste singole ai sensi delle lettere a), b) e c). A tale fine, per la
coalizione o lista singola cui è stato attribuito il premio di governabilità, divide la
rispettiva cifra elettorale circoscrizionale per il quoziente elettorale nazionale di
maggioranza di cui alla lettera a), ottenendo così l'indice relativo ai seggi da
attribuire nella circoscrizione alla coalizione o alla lista singola cui è stato
attribuito il premio di governabilità, il cui valore è troncato alla sesta cifra
decimale. Analogamente, per le altre coalizioni e liste singole, divide le rispettive
cifre elettorali circoscrizionali per il quoziente elettorale di minoranza di cui alla
lettera b), secondo periodo, ottenendo così l'indice relativo ai seggi da attribuire
nella circoscrizione a tali coalizioni e liste singole. Quindi, moltiplica ciascuno
degli indici suddetti per il numero dei seggi assegnati alla circoscrizione, di cui
all'articolo 3, comma 2, lettera a), e divide il prodotto per la somma di tutti gli
indici, ottenendo così il quoziente di attribuzione circoscrizionale. La parte intera
del quoziente di attribuzione rappresenta il numero dei seggi da assegnare a ciascuna
coalizione e lista singola. I seggi che rimangono ancora da attribuire sono
rispettivamente assegnati alle coalizioni e alle liste singole per le quali queste
ultime divisioni hanno dato le maggiori parti decimali e, in caso di parità, alle
coalizioni e alle liste singole che hanno conseguito la maggiore cifra elettorale
nazionale; a parità di quest'ultima si procede a sorteggio. Esclude dall'attribuzione di
cui al periodo precedente le coalizioni e le liste singole alle quali è stato già
attribuito il numero di seggi a esse assegnato a seguito delle operazioni di cui alle
lettere a), b) e c). Successivamente l'Ufficio accerta se il numero dei seggi assegnati
in tutte le circoscrizioni a ciascuna delle coalizioni e delle liste singole corrisponda
al numero di seggi determinato ai sensi delle lettere a), b) e c). In caso negativo, con
riferimento alle coalizioni e alle liste singole eccedentarie e alle coalizioni e alle
liste singole deficitarie, procede ai sensi del comma 1, lettera h), periodi nono e
seguenti;

e) successivamente, per ciascuna coalizione di liste, procede a ripartire i seggi a
questa assegnati nelle circoscrizioni ai sensi della lettera d) tra le liste ammesse,
procedendo come disposto dal comma 1, lettera i).

1-quater. L’ufficio centrale nazionale procede ai sensi del comma 1-bis anche qualora si
siano svolte le elezioni per il rinnovo della sola Camera dei deputati.

1-quinquies. Qualora, per qualsiasi motivo, la coalizione o lista singola cui è
attribuito il premio di governabilità ai sensi del comma 1, lettera e-sexies), non
disponga del prescritto numero di candidati all’interno delle liste circoscrizionali
presentate ai fini dell’attribuzione del premio di governabilità, nelle circoscrizioni
in cui tali candidati difettino, e limitatamente al numero di candidati mancanti,
l’Ufficio centrale nazionale assegna i seggi corrispondenti alla coalizione o alla lista
singola cui è attribuito il premio di governabilità secondo le disposizioni di cui
all’articolo 86, comma 1, periodi secondo e seguenti. In assenza di candidati nei
collegi plurinominali della circoscrizione, l’Ufficio centrale nazionale procede alle
operazioni di cui all’articolo 86, comma 1, periodi secondo e seguenti, nella
circoscrizione in cui la coalizione o la lista singola cui è attribuito il premio di
governabilità abbia la maggiore parte decimale del quoziente non utilizzata, procedendo
secondo l'ordine decrescente, o, in mancanza di parti decimali non utilizzate, sulla
base delle parti decimali già utilizzate, secondo l'ordine decrescente.

1-sexies. I voti espressi nelle circoscrizioni Valle d'Aosta/Vallée d'Aoste e
Trentino-Alto Adige/Südtirol sono computati nella determinazione delle cifre elettorali
nazionali delle liste e delle coalizioni di liste, nonché del totale dei voti validi
nazionali, ai fini del raggiungimento delle soglie di accesso alla ripartizione dei
seggi di cui al comma 1, lettera e), dell'individuazione della lista singola o
coalizione di liste che ha ottenuto la maggiore cifra elettorale nazionale di cui al
comma 1, lettera e-ter), nonché ai fini del raggiungimento della soglia minima prevista
per l'attribuzione del premio di governabilità di cui al comma 1, lettera e-quater).
Tali voti non sono considerati ai fini della ripartizione dei seggi assegnati nella
restante parte del territorio nazionale di cui ai commi 1, lettere f) e g), 1-bis e
1-ter.

2. L’Ufficio centrale nazionale provvede a comunicare ai singoli Uffici centrali
circoscrizionali il numero dei seggi assegnati a ciascuna lista e la singola lista o
coalizione di liste eventualmente assegnataria del premio di governabilità.

[...]

### Art. 83-bis

1. L'Ufficio centrale circoscrizionale, ricevute dall'Ufficio elettorale centrale
nazionale le comunicazioni di cui all'articolo 83, comma 2, procede:

a) alla proclamazione dei candidati della lista circoscrizionale presentata dalla lista
singola o dalla coalizione di liste cui è stato attribuito il premio di governabilità;

b) all'attribuzione nei singoli collegi plurinominali dei seggi spettanti alle liste, in
tutti i casi diversi da quello di cui all’articolo 83, comma 1-ter. A tale fine
l'Ufficio:

1) determina il quoziente elettorale di collegio dividendo la somma delle cifre
elettorali di collegio delle liste cui sono attribuiti seggi nella circoscrizione per il
numero dei seggi da attribuire nel collegio stesso. Nell'effettuare tale divisione non
tiene conto dell'eventuale parte frazionaria del quoziente;

2) divide quindi la cifra elettorale di collegio di ciascuna lista per tale quoziente di
collegio. La parte intera del quoziente così ottenuto rappresenta il numero dei seggi da
assegnare a ciascuna lista. I seggi che rimangono ancora da attribuire sono assegnati
alle liste seguendo la graduatoria decrescente delle parti decimali dei quozienti così
ottenuti; in caso di parità, sono assegnati alle liste con la maggiore cifra elettorale
circoscrizionale; a parità di quest'ultima, si procede a sorteggio. L'Ufficio esclude
dall'attribuzione di cui al periodo precedente le liste alle quali è stato attribuito il
numero di seggi a esse assegnato nella circoscrizione secondo la comunicazione di cui
all'articolo 83, comma 2;

3) accerta se il numero dei seggi assegnati in tutti i collegi a ciascuna lista
corrisponda al numero di seggi ad essa attribuito nella circoscrizione dall'Ufficio
elettorale centrale nazionale. In caso negativo, i seggi attribuiti in eccesso sono
sottratti alla lista nei collegi in cui li ha ottenuti con la minore parte decimale dei
quozienti di attribuzione; alla lista cui sono stati attribuiti seggi in numero minore
di quelli spettanti, i seggi sono assegnati nel collegio in cui essa ha la maggiore
parte decimale del quoziente di attribuzione non utilizzata.

1-bis. L'Ufficio centrale circoscrizionale, qualora i seggi siano stati assegnati alle
liste ai sensi dell’articolo 83, comma 1-ter, procede come segue:

a) determina, ai fini della ripartizione, il quoziente elettorale circoscrizionale della
lista singola ovvero delle liste della coalizione cui è stato attribuito il premio di
governabilità, limitatamente alle liste cui sono attribuiti seggi nella circoscrizione,
e il quoziente elettorale circoscrizionale delle altre liste cui sono attribuiti seggi
nella circoscrizione di seguito denominate, rispettivamente, “gruppo di liste di
maggioranza” e “gruppo di liste di minoranza”. Per determinare ciascuno dei quozienti,
divide il totale delle cifre elettorali circoscrizionali di ciascun gruppo di liste per
il totale dei seggi rispettivamente loro assegnati nella circoscrizione e trascura la
parte frazionaria del risultato;

b) divide poi, per ciascun collegio plurinominale, la cifra elettorale del gruppo di
liste di maggioranza per il rispettivo quoziente elettorale determinato ai sensi della
lettera a), ottenendo così l’indice di ripartizione di collegio plurinominale del gruppo
di liste di maggioranza, il cui valore è troncato alla sesta cifra decimale.
Analogamente, per le altre liste cui spettano seggi nella circoscrizione, divide il
totale delle cifre elettorali di collegio per il quoziente elettorale di minoranza
determinato ai sensi della medesima lettera a), ottenendo così l’indice di ripartizione
di collegio plurinominale del gruppo di liste di minoranza, il cui valore è troncato
alla sesta cifra decimale. Quindi, moltiplica ciascuno degli indici suddetti per il
numero dei seggi assegnati al collegio e divide il prodotto per la somma di tutti gli
indici. La parte intera dei quozienti di attribuzione così ottenuti rappresenta il
numero dei seggi da attribuire nel collegio a ciascun gruppo di liste. I seggi che
rimangono ancora da attribuire sono rispettivamente assegnati al gruppo di liste per il
quale le parti decimali dei quozienti di attribuzione siano maggiori e, in caso di
parità, al gruppo di liste che abbia conseguito la maggiore cifra elettorale
circoscrizionale; a parità di quest’ultima, si procede a sorteggio;

c) successivamente accerta se il numero dei seggi assegnati in tutti i collegi a ciascun
gruppo di liste corrisponda al numero dei seggi complessivamente determinato
dall’Ufficio centrale nazionale. In caso negativo, al gruppo di liste che abbia seggi
eccedenti sottrae i seggi nei collegi nei quali i seggi stessi sono stati ottenuti con
le parti decimali dei quozienti di attribuzione, secondo il loro ordine crescente, e li
assegna, nei medesimi collegi, al gruppo di liste deficitario;

d) procede quindi all’attribuzione nei singoli collegi dei seggi spettanti alle liste di
ciascun gruppo di liste. A tale fine, per ciascun gruppo di liste, procede ai sensi del
comma 1, lettera b), numeri 1), 2) e 3).

### Art. 84

1. Al termine delle operazioni di cui agli articoli precedenti, l'Ufficio centrale
circoscrizionale proclama eletti in ciascun collegio plurinominale, nei limiti dei seggi
ai quali ciascuna lista ha diritto, i candidati compresi nella lista medesima, a partire
dal candidato capolista e successivamente seguendo la graduatoria di cui all'articolo
77, comma 1, lettera d).

2. Qualora una lista abbia esaurito il numero dei candidati presentati in un collegio
plurinominale e non sia quindi possibile attribuire tutti i seggi a essa spettanti in
quel collegio, l'Ufficio centrale circoscrizionale assegna i seggi alla lista negli
altri collegi plurinominali della stessa circoscrizione in cui la lista medesima abbia
la maggiore parte decimale del quoziente non utilizzata, procedendo secondo l'ordine
decrescente. Qualora al termine di detta operazione residuino ancora seggi da assegnare
alla lista, questi le sono attribuiti negli altri collegi plurinominali della stessa
circoscrizione in cui la lista medesima abbia la maggiore parte decimale del quoziente
già utilizzata, procedendo secondo l'ordine decrescente.

4. Qualora al termine delle operazioni di cui al comma 2 residuino ancora seggi da
assegnare alla lista, l'Ufficio centrale nazionale, previa apposita comunicazione
dell'Ufficio centrale circoscrizionale, individua la circoscrizione in cui la lista
abbia la maggiore parte decimale del quoziente non utilizzata e procede a sua volta ad
apposita comunicazione all'Ufficio centrale circoscrizionale competente. L'Ufficio
centrale circoscrizionale provvede all'assegnazione dei seggi ai sensi del comma 2.
Qualora al termine delle operazioni di cui ai precedenti periodi residuino ancora seggi
da assegnare alla lista, questi le sono attribuiti nelle altre circoscrizioni in cui la
stessa lista abbia la maggiore parte decimale del quoziente già utilizzata, procedendo
secondo l'ordine decrescente.

5. Qualora al termine delle operazioni di cui al comma 4 residuino ancora seggi da
assegnare ad una lista in un collegio plurinominale, questi sono attribuiti, nell'ambito
del collegio plurinominale originario, alla lista facente parte della medesima
coalizione della lista deficitaria che abbia la maggiore parte decimale del quoziente
non utilizzata, procedendo secondo l'ordine decrescente; esaurite le liste con la parte
decimale del quoziente non utilizzata, si procede con le liste facenti parte della
medesima coalizione, sulla base delle parti decimali del quoziente già utilizzate,
secondo l'ordine decrescente. Qualora al termine delle operazioni di cui al primo
periodo residuino ancora seggi da assegnare alla lista, questi sono attribuiti alle
liste facenti parte della medesima coalizione negli altri collegi plurinominali della
circoscrizione, partendo da quello in cui la coalizione abbia la maggiore parte decimale
del quoziente non utilizzata e procedendo secondo quanto previsto dal primo periodo; si
procede successivamente nei collegi plurinominali in cui la coalizione abbia la maggiore
parte decimale del quoziente già utilizzata, secondo l'ordine decrescente.

7. Qualora al termine delle operazioni di cui al comma 5 residuino ancora seggi da
assegnare ad una lista, questi sono attribuiti alle liste facenti parte della medesima
coalizione della lista deficitaria nelle altre circoscrizioni. A tale fine si procede
con le modalità previste dai commi 4 e 5.

8. Nell'effettuare le operazioni di cui ai precedenti commi, in caso di parità della
parte decimale del quoziente, si procede mediante sorteggio.

[...]

### Art. 85

1. Il deputato eletto in più collegi plurinominali in posizione di capolista e in altra
posizione è proclamato nel collegio nel quale è eletto in posizione di capolista. Se
eletto in più collegi plurinominali in posizione di capolista, è proclamato nel collegio
nel quale la lista cui appartiene ha ottenuto la minore cifra elettorale percentuale di
collegio plurinominale, determinata ai sensi dell'articolo 77, comma 1, lettera e). Se
eletto in più collegi plurinominali in posizione diversa da quella di capolista, è
proclamato nel collegio nel quale ha ottenuto la maggiore cifra elettorale individuale
percentuale di collegio plurinominale, determinata ai sensi dell'articolo 77, comma 1,
lettera e-bis).

1-bis. Il deputato eletto nella lista di candidati presentata ai fini dell'attribuzione
del premio di governabilità e in uno o più collegi plurinominali si intende eletto nella
lista di candidati presentata ai fini dell'attribuzione del premio di governabilità.

### Art. 86

1. Il seggio che rimanga vacante per qualsiasi causa, anche sopravvenuta, in un collegio
plurinominale è attribuito, nell'ambito del medesimo collegio plurinominale, al
candidato primo dei non eletti, seguendo la graduatoria di cui all'articolo 77, comma 1,
lettera d). Il seggio che rimanga vacante per qualsiasi causa, anche sopravvenuta, tra
quelli assegnati ai sensi dell'articolo 83, comma 1, lettera e-sexies), alle liste
circoscrizionali presentate ai fini dell'attribuzione del premio di governabilità è
assegnato, nell'ambito della medesima circoscrizione, al candidato primo dei non eletti,
a partire dal candidato capolista e successivamente seguendo la graduatoria di cui
all'articolo 77, comma 1, lettera d), nel caso in cui la lista circoscrizionale fosse
collegata ad una lista singola, ovvero, nel caso in cui la lista circoscrizionale fosse
collegata ad una coalizione di liste, al candidato primo dei non eletti, a partire dal
candidato capolista e successivamente seguendo la graduatoria di cui all'articolo 77,
comma 1, lettera d), della lista coalizzata che abbia la maggiore parte decimale del
quoziente non utilizzata, procedendo secondo l'ordine decrescente; esaurite le liste con
la parte decimale del quoziente non utilizzata, si procede con le liste facenti parte
della medesima coalizione, sulla base delle parti decimali del quoziente già utilizzate,
secondo l'ordine decrescente. Nel caso di circoscrizione suddivisa in più collegi
plurinominali, il seggio è assegnato alla lista individuata ai sensi del secondo periodo
nel collegio plurinominale ove essa abbia ottenuto la maggiore cifra elettorale
percentuale di collegio.

2. Nel caso in cui una lista abbia già esaurito i propri candidati si procede con le
modalità di cui all'articolo 84, commi 2, 4 e 5.

[...]

## Camera dei deputati - Disposizioni speciali per le circoscrizioni Valle d'Aosta/Vallée d'Aoste e Trentino Alto Adige/Südtirol

### Art. 92

[...]

1) alla "Valle d'Aosta" spetta un solo deputato;

[...]

2-ter) i candidati nel collegio uninominale, all’atto della presentazione della
candidatura, si collegano con una lista singola o con liste tra loro collegate che
abbiano depositato il proprio contrassegno ai sensi dell’articolo 14, nel rispetto delle
dichiarazioni di collegamento in coalizione rese ai sensi dell’articolo 14-bis e con le
modalità stabilite dall’articolo 93-quinquies, commi 2 e 3.

[...]

L'elettore esprime il voto tracciando con la matita sulla scheda un segno, comunque
apposto, sul rettangolo contenente il contrassegno della lista. Il voto è valido a
favore della lista e ai fini dell'elezione del candidato nel collegio uninominale.

Nei casi in cui il segno sia tracciato solo sul nome del candidato nel collegio
uninominale, i voti sono validi a favore della lista e ai fini dell'elezione del
candidato nel collegio uninominale. Nel caso di più liste collegate in coalizione, i
voti sono ripartiti tra le liste della coalizione in proporzione ai voti ottenuti da
ciascuna nel collegio uninominale.

[...]

### Art. 93

[...]

2. È proclamato eletto il candidato che ha ottenuto il maggior numero di voti validi
anche se non collegato ad una lista ammessa ai sensi dell’articolo 83, comma 1, lettera
e).

3. In caso di parità è proclamato eletto il candidato più giovane di età.

3-bis. L'Ufficio centrale elettorale, inoltre:

a) determina la cifra elettorale di collegio uninominale di ciascuna lista. Tale cifra è
data dalla somma dei voti validi conseguiti dalla lista stessa nelle singole sezioni
elettorali del collegio uninominale e dei voti espressi a favore dei soli candidati nel
collegio uninominale collegati a più liste in coalizione, di cui all'articolo 92, terzo
comma, attribuiti alla lista a seguito delle seguenti operazioni: l'Ufficio divide il
totale dei voti validi conseguiti da tutte le liste della coalizione nel collegio
uninominale per il numero dei voti espressi a favore dei soli candidati nel collegio
uninominale, ottenendo il quoziente di ripartizione. Divide poi il totale dei voti
validi conseguiti da ciascuna lista per tale quoziente. La parte intera del quoziente
così ottenuto rappresenta il numero dei voti da assegnare a ciascuna lista; i voti che
rimangono ancora da attribuire sono rispettivamente assegnati alle liste per le quali
queste ultime divisioni abbiano dato i maggiori resti, secondo l'ordine decrescente dei
resti medesimi;

b) determina il totale dei voti validi della circoscrizione. Tale totale è dato dalla
somma delle cifre elettorali circoscrizionali di tutte le liste;

c) comunica all'Ufficio centrale nazionale, a mezzo di estratto del verbale, la cifra
elettorale circoscrizionale di ciascuna lista e il totale dei voti validi della
circoscrizione ai fini di quanto disposto dall'articolo 83, comma 1-sexies, nonché il
nominativo del candidato nel collegio uninominale proclamato eletto e la lista o le
liste ad esso collegate ai fini di quanto disposto dall'articolo 83, comma 1, lettera
f-bis).

### Art. 93-quinquies

1. Le elezioni nella circoscrizione Trentino-Alto Adige/Südtirol sono disciplinate dalle
disposizioni del presente testo unico, in quanto applicabili, con le modificazioni e
integrazioni di cui al presente articolo e agli articoli 93-sexies e 93-septies. I
candidati concorrenti nei collegi uninominali sono eletti con metodo maggioritario; i
seggi da assegnare con metodo proporzionale sono attribuiti con le modalità di cui
all'articolo 93-septies, comma 2.

2. I candidati nei collegi uninominali sono collegati con una lista singola o con liste
tra loro collegate che abbiano depositato il proprio contrassegno ai sensi dell’articolo
14, nel rispetto delle dichiarazioni di collegamento in coalizione rese ai sensi
dell’articolo 14-bis.

[...]

4. Nel complesso delle candidature presentate da ogni lista o coalizione di liste nei
collegi uninominali, nessuno dei due generi può essere rappresentato in misura superiore
al 60 per cento, con arrotondamento all'unità più prossima.

[...]

6. Per la presentazione delle liste di candidati per l'assegnazione dei seggi nel
collegio plurinominale che concorrono all'assegnazione dei seggi in ragione
proporzionale si applicano, in quanto compatibili e fatto salvo quanto previsto dalle
disposizioni del presente titolo, le disposizioni degli articoli 18-bis, 19 e 20.

[...]

### Art. 93-sexies

[...]

3. L'elettore esprime il voto tracciando con la matita sulla scheda un segno, comunque
apposto, sul rettangolo contenente il contrassegno della lista e i nomi e i cognomi dei
candidati di lista e può esprimere fino a tre preferenze in favore dei candidati nel
collegio plurinominale appartenenti alla medesima lista, escluso il capolista. Il voto
di preferenza si esprime tracciando un segno nel quadrato posto a fianco del nome e
cognome del candidato prescelto, riportato nell'apposita colonna a fianco del
contrassegno di lista. Nel caso di più preferenze espresse, queste devono riguardare
candidati di sesso diverso, pena l'annullamento della seconda e della terza preferenza
nell'ordine di lista. Il voto è valido a favore della lista e ai fini dell'elezione del
candidato nel collegio uninominale.

4. Nei casi in cui il segno sia tracciato solo sul nome del candidato nel collegio
uninominale, i voti sono validi a favore della lista e ai fini dell'elezione del
candidato nel collegio uninominale. Nel caso di più liste collegate in coalizione, i
voti sono ripartiti tra le liste della coalizione in proporzione ai voti ottenuti da
ciascuna nel collegio uninominale.

[...]

### Art. 93-septies

1. L'Ufficio centrale circoscrizionale, compiute le operazioni di cui all'articolo 76:

a) determina la cifra elettorale individuale di ciascun candidato nel collegio
uninominale; tale cifra è data dalla somma dei voti validi conseguiti dal candidato
nelle singole sezioni elettorali del collegio uninominale;

b) proclama eletto in ciascun collegio uninominale il candidato che ha ottenuto il
maggior numero di voti validi anche se non collegato ad una lista ammessa ai sensi
dell’articolo 83, comma 1, lettera e); in caso di parità, è eletto il candidato più
giovane di età;

c) determina la cifra elettorale di collegio uninominale di ciascuna lista. Tale cifra è
data dalla somma dei voti validi conseguiti dalla lista stessa nelle singole sezioni
elettorali del collegio uninominale e dei voti espressi a favore dei soli candidati nei
collegi uninominali collegati a più liste in coalizione di cui all'articolo 93-sexies,
comma 4, ultimo periodo, che sono attribuiti alla lista a seguito delle seguenti
operazioni: l'Ufficio divide il totale dei voti validi conseguiti da tutte le liste
della coalizione nel collegio uninominale per il numero dei voti espressi a favore dei
soli candidati nei collegi uninominali, ottenendo il quoziente di ripartizione. Divide
poi il totale dei voti validi conseguiti da ciascuna lista per tale quoziente. La parte
intera del quoziente così ottenuto rappresenta il numero dei voti da assegnare a
ciascuna lista; i voti che rimangono ancora da attribuire sono rispettivamente assegnati
alle liste per le quali queste ultime divisioni abbiano dato i maggiori resti, secondo
l'ordine decrescente dei resti medesimi;

d) determina la cifra elettorale circoscrizionale di ciascuna lista. Tale cifra è data
dalla somma delle cifre elettorali di collegio uninominale di ciascuna lista;

e) per ciascuna lista determina la cifra elettorale individuale circoscrizionale di
ciascun candidato, costituita dal numero di voti di preferenza validi conseguiti nelle
sezioni elettorali del collegio plurinominale. Determina quindi la graduatoria dei
candidati di ciascuna lista, escluso il capolista, sulla base delle rispettive cifre
individuali. A parità di cifre individuali, prevale l'ordine di presentazione nella
lista;

f) determina la cifra elettorale percentuale circoscrizionale di ciascuna lista. Tale
cifra è data dal quoziente risultante dalla divisione della cifra elettorale
circoscrizionale di ciascuna lista per il totale dei voti validi della circoscrizione,
moltiplicato per cento;

g) determina la cifra elettorale individuale percentuale circoscrizionale di ciascun
candidato. Tale cifra è data dal quoziente risultante dalla divisione della cifra
elettorale individuale circoscrizionale di ciascun candidato per il totale dei voti
validi della circoscrizione, moltiplicato per cento;

h) determina il totale dei voti validi della circoscrizione. Tale totale è dato dalla
somma delle cifre elettorali circoscrizionali di tutte le liste;

i) comunica all’Ufficio centrale nazionale, a mezzo di estratto del verbale, i
nominativi dei candidati nei collegi uninominali proclamati eletti e la lista o le liste
ad essi collegati, la cifra elettorale circoscrizionale di ciascuna lista nonché il
totale dei voti validi della circoscrizione, ai fini di quanto disposto dall’articolo
83, comma 1-sexies.

2. Ricevuta la comunicazione di cui all’articolo 83, comma 1, lettera e-bis), l’Ufficio
centrale circoscrizionale procede all’assegnazione dei seggi in ragione proporzionale
tra le liste e le coalizioni ammesse, come individuate nella medesima comunicazione. Ai
fini delle operazioni di cui al presente comma, sono considerate liste singole anche le
liste rappresentative di minoranze linguistiche riconosciute collegate in coalizioni che
non abbiano raggiunto la percentuale di cui all'articolo 83, comma 1, lettera e) numero
1), ove esse abbiano conseguito almeno il 20 per cento dei voti validi espressi nella
circoscrizione ai sensi dell'articolo 83, comma 1, lettera e), numero 2-bis). L’Ufficio:

a) divide la somma delle cifre elettorali circoscrizionali delle liste singole e delle
coalizioni ammesse per il numero dei seggi da assegnare nella circoscrizione, ottenendo
così il quoziente elettorale circoscrizionale. Divide poi la cifra elettorale
circoscrizionale di ciascuna coalizione di liste o singola lista per tale quoziente. La
parte intera del quoziente così ottenuto rappresenta il numero dei seggi da assegnare a
ciascuna coalizione di liste o singola lista. I seggi che rimangono ancora da assegnare
sono rispettivamente assegnati alle coalizioni di liste o singole liste per le quali
queste ultime divisioni abbiano dato i maggiori resti, secondo l'ordine decrescente dei
resti medesimi, e, in caso di parità di resti, a quelle che abbiano conseguito la
maggiore cifra elettorale circoscrizionale; a parità di quest'ultima si procede a
sorteggio;

b) ripartisce i seggi assegnati a ciascuna coalizione di liste tra le liste coalizzate.
A tal fine, per ciascuna coalizione calcola il quoziente elettorale di coalizione dato
dalla somma delle cifre elettorali circoscrizionali delle liste coalizzate, per il
numero di seggi da distribuire. Divide poi la cifra elettorale circoscrizionale di
ciascuna lista per tale quoziente. La parte intera del quoziente così ottenuto
rappresenta il numero dei seggi da assegnare a ciascuna lista. I seggi che rimangono
ancora da assegnare sono rispettivamente assegnati alle liste per le quali queste ultime
divisioni abbiano dato i maggiori resti, secondo l'ordine decrescente dei resti
medesimi, e, in caso di parità di resti, a quelle che abbiano conseguito la maggiore
cifra elettorale circoscrizionale; a parità di quest'ultima si procede a sorteggio;

c) proclama eletti, nei limiti dei seggi ai quali ciascuna lista ha diritto, i candidati
compresi nella lista della circoscrizione, a partire dal candidato capolista e
successivamente seguendo la graduatoria di cui al comma 1, lettera e).

3. L’Ufficio centrale circoscrizionale comunica all’Ufficio centrale nazionale, a mezzo
di estratto del verbale, i nominativi dei candidati nei collegi uninominali proclamati
eletti e la lista o le liste ad essi collegati, nonché il numero di seggi attribuiti a
ciascuna lista, ai fini di quanto disposto dall’articolo 83, comma 1, lettera f-bis).

### Art. 93-octies

1. Nelle circoscrizioni di cui al presente titolo nessuno può essere candidato in più di
un collegio uninominale, a pena di nullità.

2. Il candidato in un collegio uninominale di una delle circoscrizioni di cui al
presente titolo o nelle liste presentate per l'assegnazione dei seggi nel collegio
plurinominale della circoscrizione Trentino-Alto Adige/Südtirol non può essere candidato
nelle liste presentate, nelle altre circoscrizioni, ai fini dell'attribuzione del premio
di governabilità.

3. Il candidato in un collegio uninominale delle circoscrizioni di cui al presente
titolo può essere candidato, con il medesimo contrassegno, in collegi plurinominali,
fino al numero massimo di cinque nel rispetto di quanto previsto dall'articolo 19, comma
2, e dell'articolo 85, comma 1. Il candidato che risulta eletto in un collegio
uninominale e in uno o più collegi plurinominali si intende eletto nel collegio
uninominale.

[...]

## Senato

D.Lgs. 20 dicembre 1993, n. 533

### Art. 1

1. Il Senato della Repubblica è eletto su base regionale. Salvo i seggi assegnati alla
circoscrizione Estero, i seggi sono ripartiti tra le regioni a norma dell'articolo 57
della Costituzione sulla base dei risultati riportati nell’ultimo decreto del Presidente
della Repubblica che fissa, a scadenza quinquennale e ai soli fini elettorali, i dati
del censimento permanente della popolazione a livello comunale, con decreto del
Presidente della Repubblica, da emanare, su proposta del Ministro dell'interno, previa
deliberazione del Consiglio dei ministri, contemporaneamente al decreto di convocazione
dei comizi.

2. Fatti salvi i collegi uninominali delle Regioni Valle d’Aosta/Vallée d’Aoste e
Trentino-Alto Adige/Südtirol, per l’assegnazione degli altri seggi ciascuna
circoscrizione regionale è ripartita nei collegi plurinominali di cui alla tabella B 2
allegata al decreto legislativo 23 dicembre 2020, n. 177. L’assegnazione dei seggi alle
liste e alle coalizioni di liste nei collegi plurinominali è effettuata con metodo
proporzionale, con l’eventuale attribuzione di un premio di governabilità da assegnare,
alle condizioni previste dall’articolo 16-bis, alla lista singola o alla coalizione di
liste che abbia conseguito il maggior numero di voti validi a livello nazionale in
entrambe le Camere e che abbia conseguito almeno il 42 per cento di voti validi in
ciascuna di esse. Il premio di governabilità è assegnato, nelle diverse circoscrizioni
regionali, a liste circoscrizionali appositamente presentate, ai sensi dell’articolo 9,
comma 2, per un totale di trentacinque seggi complessivi. Alla lista singola o alla
coalizione di liste assegnataria del premio di governabilità non possono comunque essere
attribuiti più di centotredici seggi complessivi, ad esclusione dei seggi conseguiti
nella circoscrizione Estero.

2-ter. Con il medesimo decreto del Presidente della Repubblica di cui al comma 1, sulla
base dei risultati riportati nell’ultimo decreto del Presidente della Repubblica che
fissa, a scadenza quinquennale e ai soli fini elettorali, i dati del censimento
permanente della popolazione a livello comunale, e con esclusione delle regioni Valle
d’Aosta/Vallée d’Aoste e Trentino-Alto Adige/Südtirol, sono determinate:

a) la distribuzione tra le regioni di un numero complessivo di trentacinque seggi, da
assegnare quale premio di governabilità, alle condizioni previste dall’articolo 16-bis;

b) per il caso di attribuzione del premio di governabilità, la distribuzione tra i
collegi plurinominali di ciascuna circoscrizione regionale, dei seggi a questa spettanti
ai sensi della distribuzione di cui al comma 1, detratti quelli di cui alla lettera a)
del presente comma;

c) per il caso di mancata attribuzione del premio di governabilità, la distribuzione tra
i collegi plurinominali di ciascuna circoscrizione regionale, di tutti i seggi a questa
spettanti ai sensi della distribuzione di cui al comma 1.

3. La regione Valle d’Aosta/Vallée d’Aoste è costituita in un unico collegio
uninominale.

4. La regione Trentino-Alto Adige/Südtirol è costituita in sei collegi uninominali
definiti ai sensi della legge 30 dicembre 1991, n. 422.

4-bis. I voti espressi nelle regioni di cui ai commi 3 e 4 sono computati nella
determinazione delle cifre elettorali nazionali delle liste e delle coalizioni di liste
nonché del totale dei voti validi nazionali esclusivamente ai fini indicati
dall’articolo 16-bis, comma 1-sexies. I seggi attribuiti nelle medesime regioni sono
computati ai fini della verifica di cui all’articolo 16-bis, comma 1, lettera f-bis).

4-ter. Con riferimento alle regioni di cui ai commi 3 e 4, con il termine “lista” si
intende il partito o gruppo politico organizzato che abbia depositato il contrassegno ai
sensi dell’articolo 14 del decreto del Presidente della Repubblica 30 marzo 1957, n.
361, cui i candidati nei collegi uninominali delle medesime regioni sono tenuti a
collegarsi ai fini di cui all’articolo 16-bis, comma 1-sexies, anche nel caso in cui
tale partito o gruppo politico organizzato non presenti candidature in altre regioni.

### Art. 2

1. Il Senato della Repubblica è eletto a suffragio universale, favorendo l'equilibrio
della rappresentanza tra donne e uomini con voto diretto, libero e segreto, sulla base
dei voti espressi nelle circoscrizioni regionali, in favore di liste, anche coalizzate
tra loro, di candidati, nei confronti dei quali, ad eccezione del capolista, l'elettore
può esprimere fino a tre preferenze.

[...]

### Art. 8

[...]

1-bis. Nel medesimo atto del deposito del contrassegno, i partiti o i gruppi politici
organizzati, che si presentano singolarmente o collegati tra loro ai sensi del comma 1,
se intendono concorrere all’attribuzione del premio di governabilità di cui all’articolo
1, comma 2, dichiarano altresì il collegamento con partiti o gruppi politici organizzati
che si presentano, singolarmente o collegati tra loro, per l’elezione della Camera dei
deputati, ai sensi degli articoli 14 e 14-bis del testo unico delle leggi recanti norme
per la elezione della Camera dei deputati, di cui al decreto del Presidente della
Repubblica 30 marzo 1957, n. 361.

### Art. 9

[...]

2. La presentazione delle liste di candidati per l'attribuzione dei seggi nei collegi
plurinominali, con l'indicazione dei candidati della lista circoscrizionale presentata
ai fini dell’attribuzione del premio di governabilità, è disciplinata dalle disposizioni
contenute nell'articolo 18-bis del testo unico delle leggi recanti norme per la elezione
della Camera dei deputati, di cui al decreto del Presidente della Repubblica 30 marzo
1957, n. 361. È ammessa la presentazione di liste anche in una sola regione.

[...]

2-bis. Nel caso di elezioni per il rinnovo del solo Senato della Repubblica, i partiti o
gruppi politici organizzati presentano esclusivamente liste di candidati per
l’attribuzione dei seggi nei collegi plurinominali ai sensi dell’articolo 18-bis, comma
1, del testo unico delle leggi recanti norme per la elezione della Camera dei deputati,
di cui al decreto del Presidente della Repubblica 30 marzo 1957, n. 361, e non
presentano liste di candidati circoscrizionali ai fini dell’attribuzione del premio di
governabilità ai sensi dell’articolo 18-bis, comma 2-bis, del medesimo testo unico.

4. In ogni collegio plurinominale ciascuna lista, all'atto della presentazione, è
composta da un elenco di candidati presentati secondo un ordine numerico. Il numero dei
candidati è, in ogni caso, pari a sette, compreso il capolista. A pena di
inammissibilità, i candidati, compreso il capolista, sono collocati secondo un ordine
alternato di genere.

4-bis. Nel complesso delle candidature, a livello nazionale, presentate nelle
circoscrizioni regionali da ogni lista o coalizione di liste ai fini dell’attribuzione
del premio di governabilità, nessuno dei due generi può essere rappresentato in misura
superiore al 60 per cento, con arrotondamento all'unità più prossima.

[...]

5. [...] con l’osservanza delle norme di cui agli articoli 18-bis, 19, 20 e 21 del testo
unico delle leggi recanti norme per la elezione della Camera dei deputati, di cui al
decreto del Presidente della Repubblica 30 marzo 1957, n. 361.

### Art. 11

[...]

3-bis. Nel caso in cui una lista singola non abbia presentato candidature in tutti i
collegi plurinominali della regione, nei collegi plurinominali in cui non sono state
presentate candidature la scheda di votazione e il manifesto non recano nemmeno la lista
circoscrizionale da tale lista presentata ai fini dell’attribuzione del premio di
governabilità. In caso di liste coalizzate, la disposizione di cui al periodo precedente
si applica ai collegi plurinominali nei quali nessuna delle liste coalizzate abbia
presentato candidature.

[...]

### Art. 13

1. All’elezione dei senatori partecipano gli elettori che hanno compiuto la maggiore
età.

[...]

### Art. 14

1. L'elettore, senza che sia avvicinato da alcuno, esprime il voto tracciando con la
matita sulla scheda un segno, comunque apposto, sul rettangolo contenente il
contrassegno della lista. Il voto è valido a favore della lista. Ogni elettore, inoltre,
può esprimere fino a tre preferenze in favore di candidati nel collegio plurinominale
della lista votata che non sono capolista. Il voto di preferenza si esprime tracciando
un segno nel quadrato posto a fianco del nome e cognome del candidato prescelto,
riportato nell'apposita colonna a fianco del contrassegno di lista. Nel caso di più
preferenze espresse, queste devono riguardare candidati di sesso diverso, pena
l'annullamento della seconda e della terza preferenza nell'ordine di lista.

2. Nei casi in cui il segno sia tracciato solo nel rettangolo contenente i candidati
delle liste circoscrizionali presentate ai fini dell’attribuzione del premio di
governabilità, i voti sono validi a favore della lista. Nel caso di più liste collegate
in coalizione, i voti sono ripartiti tra le liste della coalizione in proporzione ai
voti ottenuti da ciascuna nel collegio plurinominale.

### Art. 16

1. L'Ufficio elettorale regionale, compiute le operazioni previste dall'articolo 76 del
testo unico delle leggi recanti norme per la elezione della Camera dei deputati, di cui
al decreto del Presidente della Repubblica 30 marzo 1957, n. 361, facendosi assistere,
ove lo ritenga opportuno, da uno o più esperti scelti dal presidente:

c) determina la cifra elettorale di collegio plurinominale di ciascuna lista. Tale cifra
è data dalla somma dei voti validi conseguiti dalla lista stessa nelle singole sezioni
elettorali del collegio plurinominale e dei voti espressi nel solo rettangolo contenente
i candidati delle liste circoscrizionali presentate ai fini dell’attribuzione del premio
di governabilità collegati a più liste in coalizione, che sono attribuiti alla lista a
seguito delle seguenti operazioni: l'Ufficio divide il totale dei voti validi conseguiti
da tutte le liste della coalizione nel collegio per il numero dei voti espressi
tracciando un segno nel solo rettangolo contenente i candidati delle liste
circoscrizionali presentate ai fini dell’attribuzione del premio di governabilità
collegati a più liste in coalizione, ottenendo il quoziente di ripartizione. Divide poi
il totale dei voti validi conseguiti da ciascuna lista per tale quoziente. La parte
intera del quoziente così ottenuto rappresenta il numero dei voti da assegnare a
ciascuna lista; i voti che rimangono ancora da attribuire sono rispettivamente assegnati
alle liste per le quali queste ultime divisioni abbiano dato i maggiori resti, secondo
l'ordine decrescente dei resti medesimi;

d) per ciascuna lista determina la cifra elettorale individuale di collegio
plurinominale di ciascun candidato, costituita dal numero di voti di preferenza validi
conseguiti nelle sezioni elettorali del collegio plurinominale. Determina quindi la
graduatoria dei candidati di ciascuna lista, escluso il capolista, sulla base delle
rispettive cifre individuali. A parità di cifre individuali, prevale l'ordine di
presentazione nella lista;

e) determina la cifra elettorale percentuale di collegio plurinominale di ciascuna
lista. Tale cifra è data dal quoziente risultante dalla divisione della cifra elettorale
di collegio plurinominale di ciascuna lista per il totale dei voti validi del rispettivo
collegio plurinominale, moltiplicato per cento;

e-bis) determina la cifra elettorale individuale percentuale di collegio plurinominale
di ciascun candidato. Tale cifra è data dal quoziente risultante dalla divisione della
cifra elettorale individuale di collegio plurinominale di ciascun candidato per il
totale dei voti validi del rispettivo collegio plurinominale, moltiplicato per cento;

f) determina la cifra elettorale regionale di ciascuna lista. Tale cifra è data dalla
somma delle cifre elettorali di collegio plurinominale della lista stessa;

i) determina il totale dei voti validi della regione. Tale totale è dato dalla somma
delle cifre elettorali regionali di tutte le liste;

l) comunica all'Ufficio elettorale centrale nazionale di cui all'articolo 12 del testo
unico di cui al decreto del Presidente della Repubblica 30 marzo 1957, n. 361, a mezzo
di estratto del verbale, la cifra elettorale regionale di ciascuna lista nonché il
totale dei voti validi della regione.

### Art. 16-bis

1. L'Ufficio elettorale centrale nazionale, ricevuti gli estratti dei verbali da tutti
gli Uffici elettorali regionali e fermo restando quanto previsto dal comma 1-sexies,
facendosi assistere, ove lo ritenga opportuno, da uno o più esperti scelti dal
presidente:

a) determina la cifra elettorale nazionale di ciascuna lista. Tale cifra è data dalla
somma delle cifre elettorali regionali conseguite nelle singole regioni dalle liste
aventi il medesimo contrassegno;

b) determina il totale nazionale dei voti validi. Esso è dato dalla somma delle cifre
elettorali regionali di tutte le liste;

c) determina la cifra elettorale nazionale di ciascuna coalizione di liste. Tale cifra è
data dalla somma delle cifre elettorali nazionali delle liste collegate tra loro in
coalizione;

d) determina la cifra elettorale regionale di ciascuna coalizione di liste. Tale cifra è
data dalla somma delle cifre elettorali regionali delle liste collegate tra loro in
coalizione;

e) individua quindi le coalizioni di liste e le liste ammesse alla ripartizione dei
seggi, identificandole nelle seguenti:

1) le coalizioni di liste che abbiano conseguito sul piano nazionale almeno il 10 per
cento dei voti validi espressi e che comprendano almeno una lista collegata che abbia
conseguito sul piano nazionale almeno il 3 per cento dei voti validi espressi; ovvero
una lista collegata che abbia conseguito almeno il 20 per cento dei voti validi espressi
almeno in una regione;

2) le singole liste non collegate, o collegate in coalizioni che non abbiano raggiunto
la percentuale di cui al numero 1), che abbiano conseguito sul piano nazionale almeno il
3 per cento dei voti validi espressi, e le singole liste non collegate, o collegate in
coalizioni che non abbiano raggiunto la percentuale di cui al numero 1), che abbiano
conseguito almeno il 20 per cento dei voti validi espressi almeno in una regione;

2-bis) le liste collegate con altre nelle coalizioni di cui al numero 1), che abbiano
conseguito sul piano nazionale almeno il 3 per cento dei voti validi espressi, nonché le
liste collegate nelle coalizioni di cui al medesimo numero 1) che abbiano conseguito
almeno il 20 per cento dei voti validi espressi almeno in una regione;

2-ter) per ciascuna coalizione di liste di cui al numero 1), la lista che abbia
conseguito la maggiore cifra elettorale nazionale tra quelle non ammesse alla
ripartizione dei seggi ai sensi del numero 2-bis);

e-bis) individua quindi tra le liste singole e le coalizioni di liste di cui alla
lettera e), numeri 1) e 2), quella che ha ottenuto la maggiore cifra elettorale
nazionale;

e-ter) verifica se la cifra elettorale nazionale della lista o della coalizione che ha
ottenuto la maggiore cifra elettorale nazionale, individuata ai sensi della lettera
e-bis), corrisponda almeno al 42 per cento del totale nazionale dei voti validi;

e-quater) qualora la verifica di cui alla lettera e-ter) abbia dato esito positivo
verifica se nell’elezione della Camera dei deputati la lista singola o coalizione di
liste che ha ottenuto la maggiore cifra elettorale nazionale, individuata ai sensi
dell’articolo 83, comma 1, lettera e-ter), del testo unico delle leggi recanti norme per
la elezione della Camera dei deputati di cui al decreto del Presidente della Repubblica
30 marzo 1957, n. 361, sia quella collegata, ai sensi dell’articolo 8, comma 1-bis, del
presente testo unico, alla lista singola o alla coalizione di liste che ha ottenuto la
maggiore cifra elettorale nazionale individuata ai sensi della lettera e-bis) del
presente comma e verifica altresì se essa abbia conseguito almeno il 42 per cento del
totale nazionale dei voti validi espressi nell’elezione della Camera dei deputati;

e-quinquies) qualora le verifiche di cui alla lettera e-quater) abbiano dato
congiuntamente esito positivo, attribuisce alla lista singola o alla coalizione che ha
ottenuto la maggiore cifra elettorale nazionale individuata ai sensi della lettera
e-bis), il premio di governabilità di cui all'articolo 1, comma 2, pari a trentacinque
seggi e procede ai sensi della lettera f); e-sexies) qualora la verifica di cui alla
lettera e-ter) abbia dato esito negativo, ovvero una o entrambe le verifiche di cui alla
lettera e-quater) abbiano dato esito negativo, l’Ufficio procede ai sensi del comma
1-bis;

f) procede, per ciascuna regione, a una prima distribuzione provvisoria dei seggi di cui
all'articolo 1, comma 2-ter, lettera b), tra le coalizioni e le singole liste
individuate ai sensi della lettera e), numeri 1) e 2), in base alla cifra elettorale
regionale di ciascuna di esse. A tale fine divide il totale delle cifre elettorali
regionali di ciascuna coalizione e lista singola per il numero dei seggi da assegnare
nella regione, ottenendo così il quoziente elettorale regionale. Nell'effettuare tale
divisione non tiene conto dell'eventuale parte frazionaria del quoziente. Divide poi la
cifra elettorale regionale di ciascuna coalizione e lista singola per il quoziente
elettorale regionale. La parte intera del quoziente così ottenuto rappresenta il numero
dei seggi da assegnare a ciascuna coalizione e lista singola. I seggi che rimangono
ancora da assegnare sono rispettivamente assegnati alle coalizioni o liste singole per
le quali queste ultime divisioni hanno dato i maggiori resti e, in caso di parità di
resti, a quelle che abbiano conseguito la maggiore cifra elettorale regionale; a parità
di quest'ultima si procede a sorteggio. Determina poi il totale nazionale dei seggi
assegnati in base a tale attribuzione provvisoria a ciascuna coalizione di liste e lista
singola. Tale totale è dato, per ciascuna coalizione e lista singola, dalla somma dei
seggi ad essa assegnati in ciascuna regione. Infine, ordina all'interno di un'unica
graduatoria nazionale crescente i resti che in ciascuna regione hanno dato luogo
all'attribuzione di seggi alla coalizione o lista singola di cui alla lettera e-bis);

f-bis) verifica se il totale dei seggi assegnati alla coalizione o alla lista singola
cui è stato attribuito il premio di governabilità, ivi compresi quelli assegnati ai
sensi della lettera f), sia superiore a centotredici seggi complessivi; in questo totale
sono compresi i seggi assegnati a tale coalizione o lista singola nelle regioni Valle
d’Aosta/Vallée d’Aoste e Trentino-Alto Adige/Südtirol, come risultanti dalle
comunicazioni di cui all’articolo 21, comma 2-bis, lettera c);

f-ter) qualora la verifica di cui alla lettera f-bis) abbia dato esito negativo,
conferma il riparto dei seggi effettuato ai sensi della lettera f) e comunica il
risultato delle operazioni agli Uffici elettorali regionali ai sensi del comma
1-septies;

f-quater) qualora la verifica di cui alla lettera f-bis) abbia dato esito positivo,
prosegue ai sensi del comma 1-ter.

1-bis. Nei casi di cui al comma 1, lettera e-sexies), l’Ufficio procede, per ciascuna
regione, alla distribuzione dei seggi di cui all’articolo 1 comma 2-ter, lettera c), tra
le coalizioni e le singole liste individuate ai sensi dei numeri 1) e 2) della lettera
e) del comma 1 del presente articolo in base alla cifra elettorale regionale di ciascuna
di queste, secondo quanto disposto dal comma 1, lettera f), periodi secondo e seguenti.
Procede poi alle comunicazioni agli Uffici elettorali regionali di cui al comma
1-septies.

1-ter. Nel caso di cui al comma 1, lettera f-quater), l’Ufficio procede come segue:

a) ferma restando l’assegnazione dei trentacinque seggi del premio di governabilità ai
sensi del comma 1, lettera e-quinquies), sottrae alla coalizione o lista singola cui è
stato attribuito il premio di governabilità un numero di seggi pari alla differenza tra
il numero totale dei seggi ad essa attribuiti ai sensi del comma 1, lettera f),
comprensivo dei seggi ad essa assegnati nelle regioni Valle d’Aosta/Vallée d’Aoste e
Trentino-Alto Adige/Südtirol, come risultanti dalle comunicazioni di cui all’articolo
21, comma 2-bis, lettera c) e il numero 78. I seggi sono sottratti nelle regioni sulla
base della graduatoria nazionale crescente dei resti di cui al comma 1, lettera f),
ultimo periodo, in numero pari a quanto determinato al periodo precedente. Nel caso in
cui, terminata la graduatoria nazionale dei resti di cui al comma 1, lettera f), ultimo
periodo, risultino ancora seggi da sottrarre, l'Ufficio prosegue sottraendo gli
ulteriori seggi nelle regioni in cui la coalizione o la lista singola cui è stato
attribuito il premio di governabilità ha conseguito la minore cifra elettorale regionale
percentuale, calcolata come rapporto tra la cifra elettorale regionale e il totale dei
voti della regione, moltiplicato per cento. I seggi sono sottratti, uno per regione,
fino alla concorrenza del numero determinato al primo periodo. Sono escluse dalle
operazioni di cui al terzo periodo, le regioni in cui è già stato sottratto un seggio ai
sensi del secondo periodo e quelle in cui alla coalizione o alla lista singola cui è
stato attribuito il premio di governabilità, a seguito delle operazioni di cui al comma
1, lettera f), è stato attribuito un solo seggio. Nel caso in cui, al termine delle
operazioni di cui ai periodi precedenti, risultino ancora seggi da sottrarre, l'Ufficio
ripete le operazioni di cui ai periodi terzo e quarto fino alla concorrenza del numero
di seggi determinato al primo periodo, escludendo dalle operazioni le regioni in cui
alla coalizione o alla lista singola cui è stato attribuito il premio di governabilità,
anche a seguito delle operazioni di cui ai periodi precedenti, risulti attribuito un
solo seggio;

b) in ciascuna regione in cui è stato sottratto un seggio ai sensi della lettera a), è
corrispondentemente assegnato un seggio alla coalizione o alla lista singola diversa da
quella cui è stato attribuito il premio di governabilità che, a seguito della
ripartizione regionale di cui al comma 1, lettera f), presenta il maggiore resto non
utilizzato, ovvero, nel caso in cui non vi siano resti non utilizzati, a quella che
presenta la maggiore cifra elettorale regionale. 

1-quater. L'Ufficio elettorale centrale
nazionale procede ai sensi del comma 1-bis anche qualora si siano svolte le elezioni per
il rinnovo del solo Senato della Repubblica. 

1-quinquies. Qualora, per qualsiasi motivo,
la coalizione o lista singola cui è attribuito il premio di governabilità ai sensi del
comma 1, lettera e-quinquies), non disponga del prescritto numero di candidati
all’interno delle liste circoscrizionali presentate ai fini dell’attribuzione del premio
di governabilità, nelle circoscrizioni in cui tali candidati difettino, e limitatamente
al numero di candidati mancanti, l’Ufficio centrale nazionale assegna i seggi
corrispondenti alla coalizione o lista singola cui è attribuito il premio di
governabilità secondo le disposizioni di cui all’articolo 83, comma 1-quinquies, del
testo unico delle leggi recanti norme per la elezione della Camera dei deputati, di cui
al decreto del Presidente della Repubblica 30 marzo 1957, n. 361, in quanto applicabili.

1-sexies. I voti espressi nelle circoscrizioni Valle d’Aosta/Vallée d’Aoste e
Trentino-Alto Adige/Südtirol sono computati nella determinazione delle cifre elettorali
nazionali delle liste e delle coalizioni di liste, nonché del totale dei voti validi
nazionali, ai fini del raggiungimento delle soglie di accesso alla ripartizione dei
seggi di cui al comma 1, lettera e), dell’individuazione della lista singola o
coalizione di liste che ha ottenuto la maggiore cifra elettorale nazionale di cui al
comma 1, lettera e-bis), nonché ai fini del raggiungimento della soglia minima prevista
per l’attribuzione del premio di governabilità di cui al comma 1, lettera e-ter).

1-septies. Al termine delle operazioni l'Ufficio elettorale centrale nazionale, a mezzo
di estratto del verbale, comunica agli Uffici elettorali regionali: l'elenco delle liste
e delle coalizioni di liste individuate ai sensi del comma 1, lettera e); la coalizione
o lista singola eventualmente assegnataria del premio di governabilità ai sensi del
comma 1, lettera e-quinquies); l’assegnazione dei seggi alle coalizioni di liste e alle
liste singole nella rispettiva regione come determinata ai sensi del comma 1 lettere f)
e f-ter), ovvero nei casi di cui ai commi 1-bis e 1-ter.

### Art. 17

1. Sulla base delle comunicazioni di cui al comma 1-septies dell'articolo 16-bis
l'Ufficio elettorale regionale procede:

a) alla proclamazione dei candidati della lista circoscrizionale presentata dalla lista
singola o dalla coalizione di liste cui è stato attribuito il premio di governabilità;

b) per ciascuna coalizione di liste, al riparto dei seggi fra le liste collegate ammesse
al riparto di cui all'articolo 16-bis, comma 1, lettera e), numeri 2-bis) e 2-ter). A
tal fine, per ciascuna coalizione di liste, divide la somma delle cifre elettorali
regionali delle liste ammesse al riparto per il numero di seggi già determinato
dall'Ufficio elettorale centrale nazionale, ottenendo così il relativo quoziente
elettorale di coalizione. Nell'effettuare tale divisione non tiene conto dell'eventuale
parte frazionaria del quoziente. Divide poi la cifra elettorale regionale di ciascuna
lista ammessa al riparto per il quoziente elettorale di coalizione. La parte intera del
quoziente così ottenuta rappresenta il numero di seggi da assegnare a ciascuna lista. I
seggi che rimangono ancora da assegnare sono rispettivamente assegnati alle liste per le
quali queste ultime divisioni hanno dato i maggiori resti e, in caso di parità di resti,
a quelle che hanno conseguito la maggiore cifra elettorale regionale; a parità di
quest'ultima si procede a sorteggio.

2. I seggi spettanti alle liste in ciascuna regione sono determinati dalle attribuzioni
effettuate dall'Ufficio centrale nazionale per le coalizioni di liste e per le liste
singole e comunicate ai sensi dell'articolo 16-bis, comma 1-septies, nonché dalle
assegnazioni di seggi effettuate per le liste coalizzate ai sensi del comma 1 del
presente articolo.

3. Nel caso in cui la regione sia ripartita in più collegi plurinominali, l'Ufficio
elettorale regionale procede ad assegnare nei collegi plurinominali i seggi assegnati a
ciascuna lista in sede regionale. Il numero di seggi da assegnare alle liste nei collegi
plurinominali è determinato ai sensi:

a) dell'articolo 1, comma 2-ter, lettera b), nel caso in cui i seggi siano stati
assegnati alle coalizioni e alle liste singole a seguito dell'attribuzione del premio di
governabilità, ai sensi dell'articolo 16-bis, commi 1 e 1-ter;

b) dell'articolo 1, comma 2-ter, lettera c), nel caso in cui i seggi siano stati
assegnati alle coalizioni e alle liste singole senza l'attribuzione del premio di
governabilità, ai sensi dell'articolo 16-bis, commi 1-bis e 1-quater.

4. In tutti i casi diversi da quello di cui all'articolo 16-bis, comma 1-ter, l'Ufficio
procede alla distribuzione dei seggi assegnati al collegio tra le liste con le seguenti
modalità:

a) determina il quoziente elettorale di collegio dividendo la somma delle cifre
elettorali di collegio delle liste cui sono attribuiti seggi nella circoscrizione
regionale per il numero dei seggi da attribuire nel collegio stesso. Nell'effettuare
tale divisione non tiene conto dell'eventuale parte frazionaria del quoziente;

b) divide quindi la cifra elettorale di collegio di ciascuna lista per il quoziente
elettorale di collegio di cui alla lettera a). La parte intera del quoziente così
ottenuto rappresenta il numero dei seggi da assegnare a ciascuna lista. I seggi che
rimangono ancora da attribuire sono assegnati alle liste seguendo la graduatoria
decrescente delle parti decimali dei quozienti così ottenuti; in caso di parità, sono
assegnati alle liste con la maggiore cifra elettorale di collegio; a parità di
quest'ultima, si procede a sorteggio. L'Ufficio esclude dall'assegnazione di cui al
periodo precedente le liste alle quali è stato attribuito il numero di seggi a esse
assegnato nella circoscrizione regionale, ai sensi del comma 2;

c) successivamente, l'Ufficio accerta se il numero dei seggi assegnati in tutti i
collegi a ciascuna lista corrisponda al numero di seggi ad essa attribuito nella
circoscrizione regionale ai sensi del comma 2. In caso negativo, i seggi attribuiti in
eccesso sono sottratti alla lista nei collegi in cui li ha ottenuti con la minore parte
decimale dei quozienti di attribuzione; alla lista cui sono stati attribuiti seggi in
numero minore di quelli spettanti, i seggi sono assegnati nel collegio in cui essa ha la
maggiore parte decimale del quoziente di attribuzione non utilizzata.

5. Nel caso di cui all'articolo 16-bis, comma 1-ter, l'Ufficio procede con le seguenti
modalità:

a) determina il quoziente elettorale regionale della lista singola ovvero delle liste
della coalizione cui è stato attribuito il premio di governabilità, limitatamente alle
liste cui sono assegnati seggi nella regione, e il quoziente elettorale regionale delle
altre liste cui sono assegnati seggi nella regione, di seguito denominati,
rispettivamente, “gruppo di liste di maggioranza” e “gruppo di liste di minoranza”. Per
determinare ciascuno dei quozienti, divide il totale delle cifre elettorali regionali
delle liste di ciascun gruppo per il totale dei seggi loro assegnati nella regione e
trascura la parte frazionaria del risultato;

b) divide poi, per ciascun collegio plurinominale, la cifra elettorale del gruppo di
liste di maggioranza per il rispettivo quoziente elettorale determinato ai sensi della
lettera a), ottenendo così l'indice di ripartizione di collegio plurinominale del gruppo
di liste di maggioranza, il cui valore è troncato alla sesta cifra decimale.
Analogamente, per le altre liste cui spettano seggi nella circoscrizione, divide il
totale delle cifre elettorali di collegio per il quoziente elettorale di minoranza
determinato ai sensi della lettera a), ottenendo così l'indice di ripartizione di
collegio plurinominale del gruppo di liste di minoranza, il cui valore è troncato alla
sesta cifra decimale. Quindi, moltiplica ciascuno degli indici suddetti per il numero
dei seggi assegnati al collegio, come determinato ai sensi dell'articolo 1, comma 2-ter,
lettera b), e divide il prodotto per la somma di tutti gli indici. La parte intera dei
quozienti di attribuzione così ottenuti rappresenta il numero dei seggi da assegnare nel
collegio a ciascun gruppo di liste. I seggi che rimangono ancora da attribuire sono
rispettivamente assegnati al gruppo di liste per il quale le parti decimali dei
quozienti di assegnazione siano maggiori e, in caso di parità, al gruppo di liste che
abbia conseguito la maggiore cifra elettorale di collegio; a parità di quest'ultima, si
procede a sorteggio;

c) successivamente, accerta se il numero dei seggi assegnati in tutti i collegi a
ciascun gruppo di liste corrisponda al numero dei seggi ad esso assegnato nella
circoscrizione regionale ai sensi del comma 2. In caso negativo, al gruppo di liste che
abbia seggi eccedenti sottrae i seggi nei collegi nei quali i seggi stessi sono stati
ottenuti con le parti decimali dei quozienti di attribuzione, secondo il loro ordine
crescente, e li assegna, nei medesimi collegi, al gruppo di liste deficitario; d)
procede quindi all'assegnazione nei singoli collegi dei seggi spettanti alle liste di
ciascun gruppo di liste. A tale fine, per ciascun gruppo di liste, procede ai sensi del
comma 4, lettere a), b) e c).

### Art. 17-bis

1. Al termine delle operazioni di cui agli articoli precedenti, l'Ufficio elettorale
regionale proclama eletti in ciascun collegio plurinominale, nei limiti dei seggi ai
quali ciascuna lista ha diritto, i candidati compresi nella lista medesima, a partire
dal candidato capolista e successivamente seguendo la graduatoria di cui all'articolo
16, comma 1, lettera d).

2. Qualora una lista abbia esaurito il numero dei candidati presentati in un collegio
plurinominale e non sia quindi possibile attribuire tutti i seggi a essa spettanti in
quel collegio, si applica l'articolo 84 del testo unico delle leggi recanti norme per la
elezione della Camera dei deputati, di cui al decreto del Presidente della Repubblica 30
marzo 1957, n. 361, ad eccezione di quanto previsto dai commi 4 e 7.

3. Nel caso di elezione in più collegi si applica quanto previsto dall'articolo 85 del
testo unico delle leggi recanti norme per la elezione della Camera dei deputati, di cui
al decreto del Presidente della Repubblica 30 marzo 1957, n. 361.

[...]

### Art. 19

2. Nel caso in cui rimanga vacante per qualsiasi causa, anche sopravvenuta, un seggio in
un collegio plurinominale si applica l'articolo 86 del testo unico delle leggi recanti
norme per la elezione della Camera dei deputati, di cui al decreto del Presidente della
Repubblica 30 marzo 1957, n. 361.

## Senato - Disposizioni speciali per le regioni Valle d'Aosta/Vallée d'Aoste e Trentino-Alto Adige/Südtirol

### Art. 20

1. L'elezione uninominale nel collegio uninominale della regione Valle d'Aosta/Vallée
d'Aoste e nei collegi uninominali della regione Trentino-Alto Adige/Südtirol è regolata
dalle disposizioni dei precedenti articoli, in quanto applicabili, e dalle norme
seguenti:

[...]

a-bis) i candidati nel collegio uninominale, all’atto della presentazione della
candidatura, si collegano con una lista singola o con liste tra loro collegate che
abbiano depositato il proprio contrassegno ai sensi dell’articolo 8, nel rispetto delle
dichiarazioni di collegamento in coalizione rese ai sensi dell’articolo 14-bis del testo
unico delle leggi recanti norme per la elezione della Camera dei deputati, di cui al
decreto del Presidente della Repubblica 30 marzo 1957, n. 361;

a-ter) nel complesso delle candidature presentate da ogni lista o coalizione di liste
nei collegi uninominali, nessuno dei due generi può essere rappresentato in misura
superiore al 60 per cento, con arrotondamento all'unità più prossima;

[...]

c-bis) l’elettore esprime il voto tracciando con la matita sulla scheda un segno,
comunque apposto, sul rettangolo contenente il contrassegno della lista. Il voto è
valido a favore della lista e ai fini dell’elezione del candidato nel collegio
uninominale;

c-ter) nei casi in cui il segno sia tracciato solo sul nome del candidato nel collegio
uninominale, i voti sono validi a favore della lista e ai fini dell’elezione del
candidato nel collegio uninominale. Nel caso di più liste collegate in coalizione, i
voti sono ripartiti tra le liste della coalizione in proporzione ai voti ottenuti da
ciascuna nel collegio uninominale;

[...]

### Art. 20-bis

01. La presentazione delle candidature nei collegi uninominali è regolata dalle
disposizioni del titolo VI del testo unico delle leggi recanti norme per la elezione
della Camera dei deputati, di cui al decreto del Presidente della Repubblica 30 marzo
1957, n. 361, in quanto applicabili.

1. A pena di nullità dell'elezione, nessun candidato può accettare la candidatura in più
di un collegio uninominale.

### Art. 21

1. L'ufficio elettorale regionale procede, con l'assistenza del cancelliere, alle
seguenti operazioni:

a) effettua lo spoglio delle schede eventualmente inviate dalle sezioni;

b) somma i voti ottenuti da ciascun candidato nelle singole sezioni, come risultano dai
verbali.

2. Il presidente dell'ufficio elettorale regionale, in conformità ai risultati
accertati, proclama eletto per ciascun collegio il candidato che ha ottenuto il maggior
numero di voti validi, anche se non collegato ad una lista ammessa ai sensi
dell’articolo 16-bis, comma 1, lettera e). In caso di parità di voti, è proclamato
eletto il candidato più anziano di età.

2-bis. L’ufficio elettorale regionale, inoltre:

a) determina la cifra elettorale di collegio uninominale di ciascuna lista. Tale cifra è
data dalla somma dei voti validi conseguiti dalla lista stessa nelle singole sezioni
elettorali del collegio uninominale e dei voti espressi a favore dei soli candidati nei
collegi uninominali collegati a più liste in coalizione di cui all’articolo 20, comma 1,
lettera a-bis), attribuiti alla lista a seguito delle seguenti operazioni: l’Ufficio
divide il totale dei voti validi conseguiti da tutte le liste della coalizione nel
collegio uninominale per il numero dei voti espressi a favore dei soli candidati nei
collegi uninominali, ottenendo il quoziente di ripartizione. Divide poi il totale dei
voti validi conseguiti da ciascuna lista per tale quoziente. La parte intera del
quoziente così ottenuto rappresenta il numero dei voti da assegnare a ciascuna lista; i
voti che rimangono ancora da attribuire sono rispettivamente assegnati alle liste per le
quali queste ultime divisioni abbiano dato i maggiori resti, secondo l’ordine
decrescente dei resti medesimi;

b) determina il totale dei voti validi della regione. Tale totale è dato dalla somma
delle cifre elettorali regionali di tutte le liste;

c) comunica all’Ufficio centrale nazionale, a mezzo di estratto del verbale, la cifra
elettorale regionale di ciascuna lista e il totale dei voti validi della regione ai fini
di quanto disposto dall’articolo 16-bis, comma 1-sexies, nonché i nominativi dei
candidati nei collegi uninominali proclamati eletti e la lista o le liste ad essi
collegati, ai fini di quanto disposto dall’articolo 16-bis, comma 1, lettera f-bis).

## Delimitazione dei collegi senatoriali della provincia di Bolzano

D.Lgs. 23 dicembre 2020, n. 177, tabella B.1, sezioni U04, U05 e U06, nel testo
sostituito dall’allegato 5 della legge 9 ottobre 2026, n. 180. Le corrispondenti sezioni
4, 5 e 6 della tabella allegata alla legge 30 dicembre 1991, n. 422 sono definite
dall’allegato 4 della stessa legge. La tabella riporta i comuni, uniti nelle righe
originarie spezzate per impaginazione.

| Collegio | Comune |
| --- | --- |
| U04 | Bolzano/ Bozen |
| U04 | Laives/ Leifers |
| U05 | Aldino/ Aldein |
| U05 | Andriano/ Andrian |
| U05 | Anterivo/ Altrei |
| U05 | Appiano sulla strada del vino/ Eppan an der Weinstraße |
| U05 | Avelengo/ Hafling |
| U05 | Bronzolo/ Branzoll |
| U05 | Caines/ Kuens |
| U05 | Caldaro sulla strada del vino/ Kaltern an der Weinstraße |
| U05 | Castel- bello-Ciardes/ Kastelbell-Tschars |
| U05 | Cermes/ Tscherms |
| U05 | Cortaccia sulla strada del vino/ Kurtatsch an der Weinstraße |
| U05 | Cortina sulla strada del vino/ Kurtinig an der Weinstraße |
| U05 | Curon Venosta/ Graun im Vinschgau |
| U05 | Egna/ Neumarkt |
| U05 | Gargazzone/ Gargazon |
| U05 | Glorenza/ Glurns |
| U05 | Laces/ Latsch |
| U05 | Lagundo/ Algund |
| U05 | Lana/ Lana |
| U05 | Lasa/ Laas |
| U05 | Lauregno/ Laurein |
| U05 | Magre' sulla strada del vino/ Margreid an der Weinstraße |
| U05 | Malles Venosta/ Mals |
| U05 | Marlengo/ Marling |
| U05 | Martello/ Martell |
| U05 | Meltina/ Mölten |
| U05 | Merano/ Meran |
| U05 | Montagna sulla strada del vino/ Montan an der Weinstraße |
| U05 | Moso in Passiria/ Moos in Passeier |
| U05 | Nalles/ Nals |
| U05 | Naturno/ Naturns |
| U05 | Ora/ Auer |
| U05 | Parcines/ Partschins |
| U05 | Plaus/ Plaus |
| U05 | Postal/ Burgstall |
| U05 | Prato allo Stelvio/ Prad am Stilfserjoch |
| U05 | Proves/ Proveis |
| U05 | Rifiano/ Riffian |
| U05 | Salorno sulla strada del vino/ Salurn an der Weinstraße |
| U05 | San Genesio Atesino/ Jenesien |
| U05 | San Leonardo in Passiria/ St. Leonhard in Passeier |
| U05 | San Martino in Passiria/ St. Martin in Passeier |
| U05 | San Pancrazio/ St. Pankraz |
| U05 | Sarentino/ Sarntal |
| U05 | Scena/ Schenna |
| U05 | Senales/ Schnals |
| U05 | Senale-San Felice/ Unsere Liebe Frau im Walde- St. Felix |
| U05 | Silandro/ Schlanders |
| U05 | Sluderno/ Schluderns |
| U05 | Stelvio/ Stilfs |
| U05 | Terlano/ Terlan |
| U05 | Termeno sulla strada del vino/ Tramin an der Weinstraße |
| U05 | Tesimo/ Tisens |
| U05 | Tirolo/ Tirol |
| U05 | Trodena nel parco naturale/ Truden im Naturpark |
| U05 | Tubre/ Taufers im Münstertal |
| U05 | Ultimo/ Ulten |
| U05 | Vadena/ Pfatten |
| U05 | Verano/ Vöran |
| U06 | Badia/ Abtei |
| U06 | Barbiano/ Barbian |
| U06 | Braies/ Prags |
| U06 | Brennero/ Brenner |
| U06 | Bressanone/ Brixen |
| U06 | Brunico/ Bruneck |
| U06 | Campo di Trens/ Freienfeld |
| U06 | Campo Tures/ Sand in Taufers |
| U06 | Castel- rotto/ Kastelruth |
| U06 | Chienes/ Kiens |
| U06 | Chiusa/ Klausen |
| U06 | Cornedo all'Isarco/ Karneid |
| U06 | Corvara in Badia/ Corvara |
| U06 | Dobbiaco/ Toblach |
| U06 | Falzes/ Pfalzen |
| U06 | Fie' allo Sciliar/ Völs am Schlern |
| U06 | Fortezza/ Franzen- sfeste |
| U06 | Funes/ Villnöß |
| U06 | Gais/ Gais |
| U06 | Laion/ Lajen |
| U06 | La Valle/ Wengen |
| U06 | Luson/ Lüsen |
| U06 | Marebbe/ Enneberg |
| U06 | Monguelfo-Tesido/ Welsberg-Taisten |
| U06 | Naz-Sciaves/ Natz-Schabs |
| U06 | Nova Levante/ Welschnofen |
| U06 | Nova Ponente/ Deut- schnofen |
| U06 | Ortisei/ St. Ulrich |
| U06 | Perca/ Percha |
| U06 | Ponte Gardena/ Waidbruck |
| U06 | Predoi/ Prettau |
| U06 | Racines/ Ratschings |
| U06 | Rasun-Anterselva/ Rasen-Antholz |
| U06 | Renon/ Ritten |
| U06 | Rio di Pusteria/ Mühlbach |
| U06 | Rodengo/ Rodeneck |
| U06 | San Candido/ Innichen |
| U06 | San Lorenzo di Sebato/ St. Lorenzen |
| U06 | San Martino in Badia/ St. Martin in Thurn |
| U06 | Santa Cristina Valgardena/ St. Christina in Gröden |
| U06 | Selva dei Molini/ Mühlwald |
| U06 | Selva di Val Gardena/ Wolkenstein in Gröden |
| U06 | Sesto/ Sexten |
| U06 | Terento/ Terenten |
| U06 | Tires/ Tiers |
| U06 | Valdaora/ Olang |
| U06 | Val di Vizze/ Pfitsch |
| U06 | Valle Aurina/ Ahrntal |
| U06 | Valle di Casies/ Gsies |
| U06 | Vandoies/ Vintl |
| U06 | Varna/ Vahrn |
| U06 | Velturno/ Feldthurns |
| U06 | Villabassa/ Niederdorf |
| U06 | Villandro/ Villanders |
| U06 | Vipiteno/ Sterzing |
