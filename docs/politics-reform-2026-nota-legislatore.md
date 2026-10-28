# Lacune e ambiguità della riforma elettorale del 2026

Elenco di seguito alcune situazioni nelle quali l'applicazione letterale delle norme 
riformate dalla Legge 9 ottobre 2026, n. 180 porta all'impossibilità di concludere
lo scrutinio.

Si tratta di osservazioni emerse durante lo studio della legge elettorale e la 
realizzazione di una serie di simulazioni di scrutinio. Non ha la pretesa di essere un 
elenco esaustivo, ed è opportuno che ogni lacuna individuata sia sottoposta a revisione
da parte di altre persone.

Per brevità, indicheremo con

- C il DPR 361/1957, così come modificato dalla L. 180/2026;
- S il D.Lgs. 533/1993, così come modificato dalla L. 180/2026

L'articolo verrà indicato di seguito a C o S; comma, lettera e numero verranno indicati tra parentesi di seguito.

Ad esempio C83-bis(1)(b)(3) significherà DPR 361/1957, art. 83-bis, comma 1, lettera b, numero 3.

I riferimenti sono verificati sulla ripubblicazione annotata della legge nella
Gazzetta Ufficiale del 23 ottobre 2026: [disposizioni per la Camera][fonte-camera]
e [disposizioni per il Senato][fonte-senato].

Salvo diversa indicazione nel singolo esempio, gli esempi numerati provengono da
[`r26-risultati (2).json.gz`](../data/derived/r26-manual-tests/r26-risultati%20(2).json.gz),
esportato il 27 ottobre 2026 (00:33, ora italiana), con seme `r26-default-v1`. Il numero della
simulazione è quello della colonna `Simulation` nell'archivio. Le dotazioni
territoriali sono quelle provvisorie del simulatore, ricostruite dal censimento,
non quelle di un decreto di assegnazione. L'archivio contiene anche soluzioni
interpretative provvisorie: il completamento di una simulazione non implica
che la situazione descritta sia risolta dal testo della legge.

## 1. Esaurimento dei seggi ottenuti con parti decimali

### a. Riparto tra le circoscrizioni

Nel riparto dei seggi tra le circoscrizioni, dopo aver distribuito, in
ciascuna circoscrizione, i seggi della circoscrizione tra le coalizioni e le liste
singole in base ai voti nella circoscrizione, C83(1)(h) stabilisce che, se il totale
nazionale di ciascuna coalizione o lista singola non corrisponde al numero che il
riparto nazionale le ha assegnato,

> sottrae i seggi
> eccedenti alla coalizione di liste o singola lista nelle circoscrizioni nelle quali essa
> li ha ottenuti con le parti decimali dei quozienti di attribuzione, secondo il loro
> ordine crescente, e nelle quali inoltre le coalizioni di liste o singole liste, che non
> abbiano ottenuto il numero di seggi spettante, abbiano parti decimali dei quozienti non
> utilizzate. Conseguentemente, assegna i seggi a tali coalizioni di liste o singole
> liste. Qualora nella medesima circoscrizione due o più coalizioni di liste o singole
> liste abbiano parti decimali dei quozienti non utilizzate, il seggio è attribuito alla
> coalizione di liste o alla singola lista con la più alta parte decimale del quoziente
> non utilizzata o, in caso di parità, a quella con la maggiore cifra elettorale
> nazionale. Nel caso in cui non sia possibile attribuire il seggio eccedentario nella
> medesima circoscrizione, in quanto non vi siano coalizioni di liste o singole liste
> deficitarie con parti decimali di quozienti non utilizzate, l’Ufficio prosegue, per la
> stessa coalizione di liste o singola lista eccedentaria, nell’ordine dei decimali
> crescenti, a individuare un’altra circoscrizione, fino a quando non sia possibile
> sottrarre il seggio eccedentario e attribuirlo ad una coalizione di liste o singola
> lista deficitaria nella medesima circoscrizione. Nel caso in cui non sia possibile fare
> riferimento alla medesima circoscrizione ai fini del completamento delle operazioni
> precedenti, fino a concorrenza dei seggi ancora da cedere, alla coalizione di liste o
> singola lista eccedentaria vengono sottratti i seggi nelle circoscrizioni nelle quali li
> ha ottenuti con le minori parti decimali del quoziente di attribuzione e alla coalizione
> di liste o singola lista deficitaria sono conseguentemente attribuiti seggi nelle altre
> circoscrizioni nelle quali abbia le maggiori parti decimali del quoziente di
> attribuzione non utilizzate

Può però verificarsi il caso in cui i seggi da rimuovere siano superiori al
numero di seggi ottenuti con parti decimali. La legge non indica cosa fare
in questo caso.

Lo stesso procedimento è richiamato da C83(1)(i) nel riparto tra le circoscrizioni 
dei seggi delle singole liste interne alle coalizioni, anche in questo caso può 
verificarsi il caso in cui i seggi da rimuovere siano superiori al numero di seggi ottenuti
con parti decimali.

**Esempio:**

<!-- esempio non trovato: nella nuova serie scn-autonoma-20261028 non risultano
esaurimenti dei donatori decimali nel riparto tra circoscrizioni. Negli archivi
precedenti esaminati i casi individuati coinvolgono tutti Sud chiama Nord. -->

### b. Riparto tra i collegi di una circoscrizione alla Camera

Nel riparto dei seggi tra i collegi di una circoscrizione,
dopo aver distribuito, in ciascun collegio, i seggi del collegio tra le liste
in base ai voti nel collegio, C83-bis(1)(b)(3) stabilisce che, se il totale 
circoscrizionale di ciascuna lista non corrisponde al numero che il riparto
nazionale ha assegnato a quella lista nella circoscrizione,

> i seggi attribuiti in eccesso sono
> sottratti alla lista nei collegi in cui li ha ottenuti con la minore parte decimale dei
> quozienti di attribuzione

Può però verificarsi il caso in cui i seggi da rimuovere siano superiori al
numero di seggi ottenuti con parti decimali. La legge non indica cosa fare
in questo caso.

**Esempio:**

Simulazione **2, Camera, Umbria, senza premio di governabilità**. Le correzioni
tra circoscrizioni e tra liste hanno fissato per l'Umbria i seggi indicati
nell'ultima colonna. L'unico collegio, **100001**, deve distribuirne sei:

| Lista | Voti nel collegio | Seggi interi | Seggi dopo il riparto iniziale | Seggi spettanti nella circoscrizione |
| --- | ---: | ---: | ---: | ---: |
| Azione | 38.016 | 0 | 1 | 1 |
| Partito Democratico | 102.230 | 2 | 2 | 2 |
| Fratelli d'Italia | 122.209 | 2 | 2 | 1 |
| Lega | 31.836 | 0 | 1 | 1 |
| Italia Viva | 10.962 | 0 | 0 | 1 |

Il quoziente è $\lfloor 305\,253/6 \rfloor = 50\,875$. Le parti intere
assegnano quattro seggi; i due residui vanno ad Azione e Lega. Restano quindi
due seggi a Fratelli d'Italia, entrambi interi, e nessuno a Italia Viva.
Per rispettare i seggi comunicati dal riparto nazionale occorre togliere un
seggio a Fratelli d'Italia e aggiungerne uno a Italia Viva. Nella lettura
adottata dalla nota, manca un seggio decimale da sottrarre.


### c. Riparto tra i collegi di una circoscrizione al Senato

La stessa situazione può verificarsi nel riparto del Senato, secondo S17(4)(c).

**Esempio:**

Simulazione **28, Senato, Sicilia, con premio ma senza correzione del tetto
al Senato**. Il riparto regionale e quello interno alle coalizioni assegnano
al Movimento 5 Stelle **3 seggi ordinari**. Nei due collegi siciliani, ciascuno
con sei seggi ordinari, si ottiene invece:

| Collegio | Voti M5S | Quoziente di collegio | Seggi interi M5S | Seggi decimali M5S |
| --- | ---: | ---: | ---: | ---: |
| 190001 | 319.083 | 141.531 | 2 | 0 |
| 190002 | 298.735 | 145.575 | 2 | 0 |

Il M5S ha dunque quattro seggi interi anziché tre. Il riparto iniziale dà
anche un seggio in più a Fratelli d'Italia e ad AVS, mentre Futuro Nazionale,
PD e Noi Moderati hanno un seggio in meno ciascuno. Le prime due correzioni
tolgono i seggi decimali di Fratelli d'Italia e AVS nel collegio 190001,
assegnando rispettivamente un seggio a Futuro Nazionale e al PD nel 190002.

Resta da trasferire un seggio dal M5S a Noi Moderati, ma entrambi i collegi
hanno assegnato al M5S soltanto seggi interi.


### d. Spostamento di seggi dal gruppo di maggioranza al gruppo di minoranza nella circoscrizione alla Camera

Situazione simile si può verificare nello spostamento di seggi dal gruppo di maggioranza
al gruppo di minoranza per rispettare il tetto massimo di seggi: dopo aver distribuito,
in ciascun collegio, i seggi del collegio tra i gruppi di minoranza e maggioranza
in base ai voti nel collegio, C83-bis(1-bis)(c) stabilisce che, se il totale dei seggi
di ciascun gruppo nella circoscrizione non corrisponde a quanto stabilito dal riparto nazionale

> al gruppo di liste che abbia seggi
> eccedenti sottrae i seggi nei collegi nei quali i seggi stessi sono stati ottenuti con
> le parti decimali dei quozienti di attribuzione, secondo il loro ordine crescente

Può però verificarsi il caso in cui i seggi da rimuovere siano superiori al
numero di seggi ottenuti con parti decimali. La legge non indica cosa fare
in questo caso.

**Esempio:**

<!-- esempio non trovato: nessuna occorrenza nei gruppi alla Camera, né nella
nuova serie scn-autonoma-20261028 né negli archivi precedenti esaminati. -->

### e. Spostamento di seggi dal gruppo di maggioranza al gruppo di minoranza nella circoscrizione al Senato

Situazione analoga al Senato, secondo S17(5)(c).

**Esempio:**

<!-- esempio non trovato: nessuna occorrenza nei gruppi al Senato, né nella
nuova serie scn-autonoma-20261028 né negli archivi precedenti esaminati. -->

## 2. Esaurimento delle parti decimali non utilizzate

### a. Riparto tra i collegi di una circoscrizione alla Camera

Simmetricamente al punto 1, C83-bis(1)(b)(3) stabilisce che, se il totale 
circoscrizionale di ciascuna lista non corrisponde al numero che il riparto
nazionale ha assegnato a quella lista nella circoscrizione,

> alla lista cui sono stati attribuiti seggi in numero minore
> di quelli spettanti, i seggi sono assegnati nel collegio in cui essa ha la maggiore
> parte decimale del quoziente di attribuzione non utilizzata

Può però verificarsi il caso in cui i seggi da aggiungere siano superiori al
numero di parti decimali non utilizzate. La legge non indica cosa fare
in questo caso.

**Esempio:**

Simulazione **354, Camera, Sicilia 2, senza premio di governabilità**.
Il riparto precedente assegna a Fratelli d'Italia **7 seggi** nella
circoscrizione. Nei tre collegi il suo riparto iniziale è il seguente:

| Collegio | Voti FdI | Quoziente di collegio | Seggi interi FdI | Seggi decimali FdI |
| --- | ---: | ---: | ---: | ---: |
| 190201 | 58.194 | 38.692 | 1 | 1 |
| 190202 | 102.381 | 53.528 | 1 | 1 |
| 190203 | 97.590 | 54.636 | 1 | 1 |

FdI riceve tre seggi interi e tre decimali: **6 anziché 7**, avendo già
utilizzato tutti e tre i propri decimali. Le altre differenze sono PD
3 anziché 1, Forza Italia 3 anziché 2, Futuro Nazionale e Italia Viva
0 anziché 1 ciascuna; M5S ha già i cinque seggi spettanti.

La correzione rimuove prima un seggio decimale di Forza Italia nel 190203 e
due del PD nei collegi 190202 e 190201. Assegna poi i seggi mancanti a Futuro
Nazionale e Italia Viva, entrambi nel 190202. Resta da aggiungere il settimo
seggio a FdI, ma non esiste un suo decimale non utilizzato in alcun collegio.

### b. Riparto tra i collegi di una circoscrizione al Senato

La stessa cosa può capitare al Senato, secondo S17(4)(c).

**Esempio:**

Simulazione **312, Senato, Sicilia, con premio ma senza correzione del tetto
al Senato**. Il riparto regionale e interno alle coalizioni assegna a
Fratelli d'Italia **5 seggi ordinari**. Il riparto nei collegi produce:

| Collegio | Voti FdI | Quoziente di collegio | Seggi interi FdI | Seggi decimali FdI |
| --- | ---: | ---: | ---: | ---: |
| 190001 | 286.161 | 156.547 | 1 | 1 |
| 190002 | 287.170 | 151.452 | 1 | 1 |

FdI ha quattro seggi e ha usato entrambi i decimali: anche assegnandoli
entrambi una volta sola, i due seggi interi più i due decimali non possono
raggiungere il totale regionale di cinque.

Il riparto iniziale dà inoltre due seggi al PD anziché uno, quattro al M5S
anziché tre e nessuno ad AVS anziché uno. La prima correzione sottrae il
seggio decimale del PD nel 190002 e lo attribuisce ad AVS nel 190001.
La successiva dovrebbe togliere il seggio eccedentario del M5S e assegnarlo
a FdI, ma questa lista non ha più decimali non utilizzati.

## 3. Spareggio necessario ma nessun metodo di spareggio indicato

In diversi punti dello scrutinio possono essere richiesti spareggi senza che la norma
indichi una modalità di spareggio: 

| Passaggio | Riferimenti | Parità residua non disciplinata |
| --- | --- | --- |
| Ripartizione dei voti espressi soltanto per i candidati collegati a una coalizione | C77(1)(c), C93(3-bis)(a), C93-septies(1)(c); S16(1)(c), S21(2-bis)(a) | Uguali resti nell'attribuzione dei voti residui. |
| Ammissione della migliore lista coalizzata altrimenti esclusa | C83(1)(e)(2-ter); S16-bis(1)(e)(2-ter) | Uguale cifra elettorale nazionale tra liste escluse. |
| Individuazione del vincitore del premio | C83(1)(e-ter); S16-bis(1)(e-bis) | Uguale cifra nazionale tra soggetti primi classificati. |
| Proclamazione negli uninominali speciali | C93(3), C93-septies(1)(b); S21(2) | Uguali voti e uguale data di nascita, dopo il criterio dell'età. |
| Correzioni tra circoscrizioni | C83(1)(h), richiamato da C83(1)(i), (1-bis), (1-ter)(e) | Uguali eccedenze e cifre nazionali; uguali decimali tra territori; uguali decimali e cifre nazionali tra destinatari nella stessa circoscrizione. |
| Correzioni tra collegi, anche nelle liste interne ai gruppi | C83-bis(1)(b)(3), (1-bis)(c)-(d); S17(4)(c), (5)(c)-(d) | Uguali decimali nella scelta dei collegi di sottrazione o assegnazione. |
| Correzione del tetto al Senato | S16-bis(1-ter)(a)-(b) | Uguali resti o percentuali regionali nelle sottrazioni; uguali resti destinatari o cifre regionali nel ripiego. |
| Elezioni multiple | C85(1), richiamato da S17-bis(3) | Uguali percentuali di lista tra elezioni da capolista, o individuali tra altre elezioni. |
| Sostituzioni e recuperi dei seggi del premio | C86(1), C83(1-quinquies); S16-bis(1-quinquies), S19(2) | Uguali decimali tra liste o circoscrizioni; uguali percentuali tra collegi; più seggi del premio destinati alla stessa persona. |

I riferimenti relativi al Trentino-Alto Adige e alle elezioni multiple sono
riscontrabili anche nel [testo a fronte parlamentare, pp. 82, 94-95 e
129-130](https://documenti.camera.it/leg19/dossier/Pdf/AC0469g.pdf#page=84).
Qui si considera la parità rimasta **dopo** tutti i criteri espressi: i
sorteggi già previsti per i riparti iniziali e da C84(8) per i recuperi
ordinari sono casi distinti.

**Esempio:**

Simulazione **332, Senato, Valle d'Aosta, collegio uninominale 020001**,
nell'archivio precedente
[`r26-risultati (1).json.gz`](../data/derived/r26-manual-tests/r26-risultati%20(1).json.gz).
La somma dei voti attribuiti ai candidati nel collegio dà questa graduatoria
nelle prime posizioni:

| Candidato sintetico collegato a | Voti | Data di nascita sintetica |
| --- | ---: | --- |
| Vallée d'Aoste – Autonomie Progrès Fédéralisme | 10.930 | 1 gennaio 1977 |
| Pour l'Autonomie – Per l'Autonomia | 10.930 | 1 gennaio 1977 |
| Valle d'Aosta Aperta | 9.401 | 1 gennaio 1977 |

Occorre proclamare un solo senatore. I primi due candidati hanno il medesimo
numero di voti, superiore a quello di tutti gli altri candidati. S21(2)
impone quindi di preferire il più anziano, ma anche la data di nascita
coincide: nessuno dei due prevale applicando i criteri espressamente previsti.
La disposizione non indica un criterio ulteriore per risolvere questa parità.
Le date di nascita sono sintetiche, come le candidature dello scenario;
i voti dei candidati in parità sono entrambi positivi.

## 4. La differenza tra i seggi spettanti ad una circoscrizione e quelli assegnati dal riparto nazionale rende impossibile il rispetto del tetto massimo

Nella Camera, è possibile che il riparto nazionale assegni ad una circoscrizione seggi in più o in meno rispetto
a quanto previsto dalle tabelle di cui all'art. 3 di C. Se però è necessario rispettare il tetto massimo
togliendo seggi alle liste eccedentarie e assegnandoli a quelle deficitarie in una circoscrizione,
e nella stessa circoscrizione il riparto nazionale aveva assegnato un seggio in più o uno in meno,
l'operazione risulterà impossibile: C83-bis(1-bis)(c) stabilisce che, se il numero di seggi attribuito nei collegi
non corrisponde al numero di seggi da riparto nazionale,

> al gruppo di liste che abbia seggi
> eccedenti sottrae i seggi nei collegi nei quali i seggi stessi sono stati ottenuti con
> le parti decimali dei quozienti di attribuzione, secondo il loro ordine crescente, e li
> assegna, **nei medesimi collegi**, al gruppo di liste deficitario;

Questa operazione però lascia invariato il numero di seggi totali del collegio e della circoscrizione: di conseguenza non si potrà mai
arrivare a soddisfare il riparto nazionale effettuando solo questa operazione.

**Esempio:**

<!-- esempio non trovato: nessun conflitto tra dotazione dei collegi e totale
circoscrizionale nella nuova serie scn-autonoma-20261028. Nei sette casi
individuati nell'archivio precedente r26-risultati (2).json.gz, il trasferimento
tra circoscrizioni che produce il conflitto coinvolge sempre Sud chiama Nord. -->

## 5. Esaurimento dei candidati presenti in una regione per il Senato

### a. Proclamazione degli eletti ordinari al Senato

Se, nella proclamazione degli eletti al Senato, è necessario proclamare eletto
un candidato in un collegio dove la lista ha già esaurito il numero di candidati
presentati, S17-bis(2) rimanda a C84 escludendo C84(4) e C84(7), cioè:

1. C84(2): ripesca i candidati della stessa lista negli altri collegi della regione;
2. se questi sono esauriti passa a C84(5): ripesca i candidati di altre liste della
  stessa coalizione nello stesso collegio o nella stessa regione.

E' però possibile che la lista abbia esaurito tutti i candidati nella regione, e
che non ci siano altre liste di coalizione, o abbiano anch'esse esaurito i candidati.

**Esempio:**

La situazione si è realmente verificata nel 2019 nel caso dell'elezione della senatrice
Emma Pavanelli (decisione della Giunta delle elezioni e delle immunità parlamentari del Senato
del 31 luglio 2019), in cui il Movimento 5 Stelle in Sicilia aveva ottenuto un numero di seggi
superiore al totale dei candidati.

Il singolo caso è stato risolto con la decisione del Senato, ma il vuoto normativo rimane.

## Frequenza con cui si verificano le situazioni problematiche

Sono state svolte 1000 simulazioni con un set di liste verosimile, generando voti sulla base dei sondaggi e delle ultime elezioni.

La tabella seguente si riferisce alla serie originale di
[`r26-risultati (2).json.gz`](../data/derived/r26-manual-tests/r26-risultati%20(2).json.gz),
non alla nuova serie con Sud chiama Nord come lista singola. I conteggi originari
comprendono anche le situazioni con Sud chiama Nord descritte nella precedente
versione della nota; non sono stati ricalcolati escludendole.

Nella tabella è presente il numero di simulazioni nelle quali ciascuno di questi problemi si è verificato:

| Problema | Simulazioni interessate |
| --- | --- |
| 1. Esaurimento dei seggi ottenuti con parti decimali | 333/1000 |
| 2. Esaurimento delle parti decimali non utilizzate | 16/1000 |
| 3. Spareggio necessario ma nessun metodo di spareggio indicato | 8/1000 |
| 4. La differenza tra i seggi spettanti ad una circoscrizione e quelli assegnati dal riparto nazionale rende impossibile il rispetto del tetto massimo | 7/1000 |
| 5. Esaurimento dei candidati presenti in una regione per il Senato | 5/1000 |

[fonte-camera]: https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticoloDefault/originario?atto.codiceRedazionale=26A05611&atto.dataPubblicazioneGazzetta=2026-10-23&atto.tipoProvvedimento=LEGGE
[fonte-senato]: https://www.gazzettaufficiale.it/atto/serie_generale/caricaArticolo?art.codiceRedazionale=26A05611&art.dataPubblicazioneGazzetta=2026-10-23&art.flagTipoArticolo=0&art.idArticolo=2&art.idGruppo=0&art.idSottoArticolo=1&art.idSottoArticolo1=10&art.progressivo=0&art.versione=1
