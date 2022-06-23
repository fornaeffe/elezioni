

# $camera_voti_candidato_per_comune
# CIRCOSCRIZIONE
# COLLEGIOPLURINOMINALE
# COLLEGIOUNINOMINALE
# COMUNE
# CANDIDATO
# VOTI_CANDIDATO

# Art. 77.
# ((1. L'Ufficio centrale circoscrizionale, compiute le operazioni di
# cui all'articolo 76, facendosi assistere, ove lo ritenga opportuno,
#   da uno o piu' esperti scelti dal presidente:
# a) determina la cifra elettorale individuale di ciascun candidato
# nel collegio uninominale; tale cifra e' data dalla somma dei voti
#   validi conseguiti dal candidato nelle singole sezioni elettorali del
#   collegio uninominale;

cifre_ind <- aggregate(
  VOTI_CANDIDATO ~ 
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    COALIZIONE +
    CANDIDATO, 
  data = dati$camera_voti_candidato_per_comune, 
  sum
)

cifre_ind <- merge(cifre_ind, dati$camera_candidati_uni)

# b) proclama eletto in ciascun collegio uninominale il candidato
# che ha ottenuto il maggior numero di voti validi; in caso di parita',
# e' eletto il candidato piu' giovane di eta';

cifre_ind <- cifre_ind[
  order(
    cifre_ind$CIRCOSCRIZIONE, 
    cifre_ind$COLLEGIOPLURINOMINALE, 
    cifre_ind$COLLEGIOUNINOMINALE, 
    cifre_ind$VOTI_CANDIDATO,
    cifre_ind$DATA_NASCITA,
    decreasing = c("FALSE", "FALSE", "FALSE", "TRUE", "TRUE"), 
    method = "radix"
  ),
]
cifre_ind$ELETTO <- !duplicated(
  cifre_ind[
    ,
    c(
      "CIRCOSCRIZIONE", 
      "COLLEGIOPLURINOMINALE", 
      "COLLEGIOUNINOMINALE"
    )
  ]
)

# c) determina la cifra elettorale di collegio uninominale di
# ciascuna lista. Tale cifra e' data dalla somma dei voti validi
# conseguiti dalla lista stessa nelle singole sezioni elettorali del
# collegio uninominale e dei voti espressi a favore dei soli candidati
# nei collegi uninominali collegati a piu' liste in coalizione di cui
# all'articolo 58, terzo comma, ultimo periodo, attribuiti alla lista a
# seguito delle seguenti operazioni: l'Ufficio divide il totale dei
# voti validi conseguiti da tutte le liste della coalizione nel
# collegio uninominale per il numero dei voti espressi a favore dei
# soli candidati nei collegi uninominali, ottenendo il quoziente di
# ripartizione. Divide poi il totale dei voti validi conseguiti da
# ciascuna lista per tale quoziente. La parte intera del quoziente
# cosi' ottenuto rappresenta il numero dei voti da assegnare a ciascuna
# lista; i voti che rimangono ancora da attribuire sono rispettivamente
# assegnati alle liste per le quali queste ultime divisioni abbiano
# dato i maggiori resti, secondo l'ordine decrescente dei resti
# medesimi. Nella ripartizione dei voti espressi in favore dei soli
# candidati nei collegi uninominali collegati a piu' liste in
# coalizione, l'Ufficio esclude dal computo i voti espressi in favore
# della lista rappresentativa di minoranze linguistiche riconosciute
# nei collegi uninominali dove questa ha presentato proprie candidature
# ai sensi dell'articolo 18-bis, comma 1-bis;

voti_lista_per_candidato <- aggregate(
  VOTI_LISTA ~ 
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    COALIZIONE +
    CANDIDATO
  , 
  data = dati$camera_voti_lista_per_comune,
  sum,
  na.action = na.pass
)
voti_lista_per_candidato <- merge(voti_lista_per_candidato, cifre_ind)
voti_lista_per_candidato$VOTI_SOLO_CANDIDATO <-
  voti_lista_per_candidato$VOTI_CANDIDATO - voti_lista_per_candidato$VOTI_LISTA

voti_lista_per_candidato$QUOZIENTE_RIPARTIZIONE <-
  voti_lista_per_candidato$VOTI_LISTA / voti_lista_per_candidato$VOTI_SOLO_CANDIDATO

cifre_uni <- aggregate(
  VOTI_LISTA ~
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    COALIZIONE +
    CANDIDATO +
    LISTA
  ,
  data = dati$camera_voti_lista_per_comune,
  sum,
  na.action = na.pass
)

cifre_uni <- merge(
  cifre_uni, 
  voti_lista_per_candidato[
    ,
    c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "QUOZIENTE_RIPARTIZIONE"
    )
  ], 
  by = c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "CANDIDATO"
  )
)

cifre_uni$PARTE_INTERA <- 
  cifre_uni$VOTI_LISTA %/% cifre_uni$QUOZIENTE_RIPARTIZIONE
cifre_uni$RESTO <- 
  cifre_uni$VOTI_LISTA %% cifre_uni$QUOZIENTE_RIPARTIZIONE

cifre_uni$PARTE_INTERA[cifre_uni$PARTE_INTERA < 0] <- 0

voti_assegnati <- aggregate(
  PARTE_INTERA ~
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    COALIZIONE +
    CANDIDATO
  ,
  data = cifre_uni,
  sum,
  na.action = na.pass
)

voti_lista_per_candidato <- merge(voti_lista_per_candidato, voti_assegnati)
voti_lista_per_candidato$DA_ASSEGNARE <-
  voti_lista_per_candidato$VOTI_SOLO_CANDIDATO - voti_lista_per_candidato$PARTE_INTERA

cifre_uni <- merge(
  cifre_uni, 
  voti_lista_per_candidato[
    ,
    c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "DA_ASSEGNARE"
    )
  ], 
  by = c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "CANDIDATO"
  )
)

cifre_uni <- cifre_uni[
  order(
    cifre_uni$CIRCOSCRIZIONE, 
    cifre_uni$COLLEGIOPLURINOMINALE, 
    cifre_uni$COLLEGIOUNINOMINALE,
    cifre_uni$COALIZIONE, 
    cifre_uni$CANDIDATO,  
    cifre_uni$RESTO,
    decreasing = c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE"), 
    method = "radix"
  ),
]

cifre_uni$ORDINE <- ave(
  seq_along(cifre_uni$CIRCOSCRIZIONE),
  paste(
    cifre_uni$CIRCOSCRIZIONE, 
    cifre_uni$COLLEGIOPLURINOMINALE, 
    cifre_uni$COLLEGIOUNINOMINALE, 
    cifre_uni$CANDIDATO
  ),
  FUN = seq_along
)

cifre_uni$VOTI_DA_RESTI <- cifre_uni$ORDINE <= cifre_uni$DA_ASSEGNARE

cifre_uni$PARTE_INTERA[
  is.na(cifre_uni$PARTE_INTERA) | is.nan(cifre_uni$PARTE_INTERA)
] <- 0

cifre_uni$VOTI_DA_RESTI[
  is.na(cifre_uni$VOTI_DA_RESTI) | is.nan(cifre_uni$VOTI_DA_RESTI)
] <- 0

cifre_uni$CIFRA <- 
  cifre_uni$VOTI_LISTA + 
  cifre_uni$PARTE_INTERA + 
  cifre_uni$VOTI_DA_RESTI

# d) determina la cifra elettorale di collegio plurinominale di
# ciascuna lista. Tale cifra e' data dalla somma delle cifre elettorali
# di collegio uninominale di ciascuna lista;

cifre_pluri <- aggregate(
  CIFRA ~
    CIRCOSCRIZIONE +
    COLLEGIOPLURINOMINALE +
    COALIZIONE +
    LISTA,
  data = cifre_uni,
  sum,
  na.action = na.pass
)

# e) determina la cifra elettorale percentuale di collegio
# plurinominale di ciascuna lista. Tale cifra e' data dal quoziente
# risultante dalla divisione della cifra elettorale di collegio
# plurinominale di ciascuna lista per il totale dei voti validi del
# rispettivo collegio plurinominale, moltiplicato per cento;

totali_pluri <- aggregate(
  CIFRA ~
    CIRCOSCRIZIONE +
    COLLEGIOPLURINOMINALE,
  data = cifre_pluri,
  sum,
  na.action = na.pass
)

cifre_pluri <- merge(
  cifre_pluri,
  totali_pluri,
  by = c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE"
  ),
  suffixes = c("", "_TOT")
)

cifre_pluri$CIFRA_PERCENTUALE <- cifre_pluri$CIFRA / cifre_pluri$CIFRA_TOT * 100

# f) determina la cifra elettorale circoscrizionale di ciascuna
# lista. Tale cifra e' data dalla somma delle cifre elettorali di
# collegio plurinominale della lista stessa;

cifre_circ <- aggregate(
  CIFRA ~
    CIRCOSCRIZIONE +
    COALIZIONE +
    LISTA,
  data = cifre_pluri,
  sum,
  na.action = na.pass
)

# g) determina la cifra elettorale percentuale di ciascun candidato
# nel collegio uninominale. Tale cifra e' data dal quoziente risultante
# dalla divisione della cifra elettorale individuale di ciascun
# candidato per il totale dei voti validi del rispettivo collegio
# uninominale, moltiplicato per cento;

totali_uni <- aggregate(
  VOTI_CANDIDATO ~
    CIRCOSCRIZIONE +
    COLLEGIOPLURINOMINALE +
    COLLEGIOUNINOMINALE,
  data = cifre_ind,
  sum,
  na.action = na.pass
)

cifre_ind <- merge(
  cifre_ind,
  totali_uni,
  by = c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE"
  ),
  suffixes = c("", "_TOT")
)

cifre_ind$CIFRA_PERCENTUALE <- 
  cifre_ind$VOTI_CANDIDATO / cifre_ind$VOTI_CANDIDATO_TOT * 100

# h) determina, per ciascuna lista, la graduatoria dei candidati
# nei collegi uninominali della circoscrizione non proclamati eletti,
# disponendoli nell'ordine delle rispettive cifre elettorali
# individuali percentuali. A parita' di cifre individuali percentuali,
# prevale il piu' giovane di eta'. In caso di collegamento dei
# candidati con piu' liste, i candidati entrano a far parte della
# graduatoria relativa a ciascuna delle liste con cui e' stato
# dichiarato il collegamento;

candidati_uni_non_eletti <- cifre_uni[
  !(cifre_uni$CANDIDATO %in% cifre_ind$CANDIDATO[cifre_ind$ELETTO])
  ,
  c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "CANDIDATO",
    "COALIZIONE",
    "LISTA"
  )
]

candidati_uni_non_eletti <- merge(
  candidati_uni_non_eletti,
  cifre_ind[
    ,
    c(
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "DATA_NASCITA",
      "CIFRA_PERCENTUALE"
    )
  ]
)

candidati_uni_non_eletti <- candidati_uni_non_eletti[
  order(
    candidati_uni_non_eletti$CIRCOSCRIZIONE,
    candidati_uni_non_eletti$LISTA, 
    candidati_uni_non_eletti$CIFRA_PERCENTUALE,  
    candidati_uni_non_eletti$DATA_NASCITA,
    decreasing = c("FALSE", "FALSE", "TRUE", "TRUE"), 
    method = "radix"
  ),
]

# i) determina il totale dei voti validi della circoscrizione. Tale
# totale e' dato dalla somma delle cifre elettorali circoscrizionali di
# tutte le liste;

totali_circ <- aggregate(
  CIFRA ~ CIRCOSCRIZIONE,
  data = cifre_circ,
  sum,
  na.action = na.pass
)

# l) comunica all'Ufficio centrale nazionale, a mezzo di estratto
# del verbale, la cifra elettorale circoscrizionale di ciascuna lista
# nonche' il totale dei voti validi della circoscrizione)).

# Art. 83.
# 1. L'Ufficio centrale nazionale, ricevuti gli estratti dei verbali
# da tutti gli Uffici centrali circoscrizionali, facendosi assistere,
# ove lo ritenga opportuno, da uno o piu' esperti scelti dal
# presidente:
#   a) determina la cifra elettorale nazionale di ciascuna lista.
# Tale cifra e' data dalla somma delle cifre elettorali
# circoscrizionali conseguite nelle singole circoscrizioni dalle liste
# aventi il medesimo contrassegno;

cifre_naz <- aggregate(
  CIFRA ~ COALIZIONE + LISTA,
  data = cifre_circ,
  sum
)

# b) determina il totale nazionale dei voti validi. Esso e' dato
# dalla somma delle cifre elettorali circoscrizionali di tutte le
# liste;
totale_naz <- sum(cifre_naz$CIFRA)

# c) determina la cifra elettorale nazionale di ciascuna coalizione
# di liste. Tale cifra e' data dalla somma delle cifre elettorali
# nazionali delle liste collegate in coalizione. Non concorrono alla
# determinazione della cifra elettorale nazionale di coalizione i voti
# espressi a favore delle liste collegate che abbiano conseguito sul
# piano nazionale un numero di voti validi inferiore all'1 per cento
# del totale, fatto salvo, per le liste rappresentative di minoranze
# linguistiche riconosciute, quanto previsto alla lettera e);


# d) determina la cifra elettorale circoscrizionale di ciascuna
# coalizione di liste. Tale cifra e' data dalla somma delle cifre
# elettorali circoscrizionali delle liste collegate tra loro in
# coalizione, individuate ai sensi dell'ultimo periodo della lettera
# c);
# e) individua quindi:
#   1) le coalizioni di liste che abbiano conseguito sul piano
# nazionale almeno il 10 per cento dei voti validi espressi e che
# comprendano almeno una lista collegata che abbia conseguito sul piano
# nazionale almeno il 3 per cento dei voti validi espressi ovvero una
# lista collegata rappresentativa di minoranze linguistiche
# riconosciute, presentata esclusivamente in una regione ad autonomia
# speciale il cui statuto o le relative norme di attuazione prevedano
# una particolare tutela di tali minoranze linguistiche, che abbia
# conseguito almeno il 20 per cento dei voti validi espressi nella
# regione medesima o i cui candidati siano stati proclamati eletti in
# almeno ((un quarto dei collegi uninominali della circoscrizione ai
#          sensi dell'articolo 77, con arrotondamento all'unita' superiore));
# 2) le singole liste non collegate, o collegate in coalizioni
# che non abbiano raggiunto la percentuale di cui al numero 1), che
# abbiano conseguito sul piano nazionale almeno il 3 per cento dei voti
# validi espressi, nonche' le singole liste non collegate e le liste
#          collegate in coalizioni che non abbiano raggiunto la percentuale di
#          cui al numero 1), rappresentative di minoranze linguistiche
#         riconosciute, presentate esclusivamente in una regione ad autonomia
#         speciale il cui statuto o le relative norme di attuazione prevedano
#         una particolare tutela di tali minoranze linguistiche, che abbiano
#         conseguito almeno il 20 per cento dei voti validi espressi nella
#         regione medesima o i cui candidati siano stati proclamati eletti in
#         almeno ((un quarto dei collegi uninominali della circoscrizione ai
#                  sensi dell'articolo 77, con arrotondamento all'unita' superiore));
# f) procede al riparto ((dei seggi assegnati nelle circoscrizioni
# del territorio nazionale, con esclusione del seggio assegnato alla
# circoscrizione Valle d'Aosta)); a tale fine, detrae i ((...)) seggi
#         gia' attribuiti ai candidati proclamati eletti nei collegi
# uninominali ai sensi dell'articolo 77, comma 1, lettera b), e procede
# al riparto dei restanti seggi tra le coalizioni di liste e le singole
# liste di cui alla lettera e) del presente comma in base alla cifra
# elettorale nazionale di ciascuna di esse, fatto salvo quanto previsto
# all'articolo 92, primo comma. A tale fine divide il totale delle
# cifre elettorali nazionali delle coalizioni di liste e delle singole
# liste di cui alla lettera e) del presente comma per il numero dei
# seggi da attribuire, ottenendo cosi' il quoziente elettorale
# nazionale. Nell'effettuare tale divisione non tiene conto
# dell'eventuale parte frazionaria del quoziente. Divide poi la cifra
# elettorale nazionale di ciascuna coalizione di liste o singola lista
# per tale quoziente. La parte intera del quoziente cosi' ottenuto
# rappresenta il numero dei seggi da assegnare a ciascuna coalizione di
# liste o singola lista. I seggi che rimangono ancora da attribuire
# sono rispettivamente assegnati alle coalizioni di liste o singole
# liste per le quali queste ultime divisioni abbiano dato i maggiori
# resti, secondo l'ordine decrescente dei resti medesimi, e, in caso di
# parita' di resti, a quelle che abbiano conseguito la maggiore cifra
# elettorale nazionale; a parita' di quest'ultima si procede a
# sorteggio;
# g) procede, per ciascuna coalizione di liste, al riparto dei
# seggi fra le liste collegate che abbiano conseguito sul piano
# nazionale almeno il 3 per cento dei voti validi espressi nonche' fra
# le liste collegate rappresentative di minoranze linguistiche
# riconosciute, presentate esclusivamente in una regione ad autonomia
# speciale il cui statuto o le relative norme di attuazione prevedano
# una particolare tutela di tali minoranze linguistiche, che abbiano
# conseguito almeno il 20 per cento dei voti validi espressi nella
# regione medesima o i cui candidati siano stati proclamati eletti in
# almeno ((un quarto dei collegi uninominali della circoscrizione ai
#          sensi dell'articolo 77, con arrotondamento all'unita' superiore)). A
# tale fine, divide la somma delle cifre elettorali delle liste ammesse
# al riparto per il numero di seggi gia' individuato ai sensi della
#          lettera f) del presente comma. Nell'effettuare tale divisione non
# tiene conto dell'eventuale parte frazionaria del quoziente cosi'
# ottenuto. Divide poi la cifra elettorale nazionale di ciascuna lista
# ammessa al riparto per tale quoziente. La parte intera del quoziente
# cosi' ottenuto rappresenta il numero dei seggi da assegnare a
#         ciascuna lista. I seggi che rimangono ancora da attribuire sono
#         rispettivamente assegnati alle liste per le quali queste ultime
#         divisioni abbiano dato i maggiori resti e, in caso di parita' di
# resti, alle liste che abbiano conseguito la maggiore cifra elettorale
# nazionale; a parita' di quest'ultima si procede a sorteggio;
# h) procede quindi alla distribuzione nelle singole circoscrizioni
# dei seggi assegnati alle coalizioni di liste o singole liste di cui
# alla lettera e). A tale fine determina il numero di seggi da
# attribuire in ciascuna circoscrizione sottraendo dal numero dei seggi
# spettanti alla circoscrizione stessa ai sensi dell'articolo 3, comma
#         1, il numero dei collegi uninominali costituiti nella circoscrizione.
#         Divide quindi la somma delle cifre elettorali circoscrizionali delle
#         coalizioni di liste e delle singole liste ammesse al riparto per il
#         numero di seggi da attribuire nella circoscrizione, ottenendo cosi'
# il quoziente elettorale circoscrizionale. Nell'effettuare tale
#         divisione non tiene conto dell'eventuale parte frazionaria del
# quoziente cosi' ottenuto. Divide poi la cifra elettorale
#         circoscrizionale di ciascuna coalizione di liste o singola lista per
#         il quoziente elettorale circoscrizionale, ottenendo cosi' il
# quoziente di attribuzione. La parte intera del quoziente di
# attribuzione rappresenta il numero dei seggi da assegnare a ciascuna
# coalizione di liste o singola lista. I seggi che rimangono ancora da
# attribuire sono rispettivamente assegnati alle coalizioni di liste o
# singole liste per le quali queste ultime divisioni hanno dato le
# maggiori parti decimali e, in caso di parita', alle coalizioni di
#         liste o singole liste che hanno conseguito la maggiore cifra
#         elettorale nazionale; a parita' di quest'ultima si procede a
#         sorteggio. Esclude dall'attribuzione di cui al periodo precedente le
# coalizioni di liste o singole liste alle quali e' stato gia'
# attribuito il numero di seggi ad esse assegnato a seguito delle
# operazioni di cui alla lettera f). Successivamente l'Ufficio accerta
#         se il numero dei seggi assegnati in tutte le circoscrizioni a
#         ciascuna coalizione di liste o singola lista corrisponda al numero di
#         seggi determinato ai sensi della lettera f). In caso negativo,
# procede alle seguenti operazioni, iniziando dalla coalizione di liste
# o singola lista che abbia il maggior numero di seggi eccedenti e, in
# caso di parita' di seggi eccedenti da parte di piu' coalizioni di
# liste o singole liste, da quella che abbia ottenuto la maggiore cifra
# elettorale nazionale, proseguendo poi con le altre coalizioni di
# liste o singole liste in ordine decrescente di seggi eccedenti:
#   sottrae i seggi eccedenti alla coalizione di liste o singola lista
# nelle circoscrizioni nelle quali essa li ha ottenuti con le parti
# decimali dei quozienti di attribuzione, secondo il loro ordine
# crescente, e nelle quali inoltre le coalizioni di liste o singole
# liste, che non abbiano ottenuto il numero di seggi spettante, abbiano
# parti decimali dei quozienti non utilizzate. Conseguentemente,
# assegna i seggi a tali coalizioni di liste o singole liste. Qualora
# nella medesima circoscrizione due o piu' coalizioni di liste o
# singole liste abbiano parti decimali dei quozienti non utilizzate, il
# seggio e' attribuito alla coalizione di liste o alla singola lista
# con la piu' alta parte decimale del quoziente non utilizzata o, in
# caso di parita', a quella con la maggiore cifra elettorale nazionale.
# Nel caso in cui non sia possibile attribuire il seggio eccedentario
# nella medesima circoscrizione, in quanto non vi siano coalizioni di
# liste o singole liste deficitarie con parti decimali di quozienti non
# utilizzate, l'Ufficio prosegue, per la stessa coalizione di liste o
# singola lista eccedentaria, nell'ordine dei decimali crescenti, a
# individuare un'altra circoscrizione, fino a quando non sia possibile
# sottrarre il seggio eccedentario e attribuirlo ad una coalizione di
# liste o singola lista deficitaria nella medesima circoscrizione. Nel
# caso in cui non sia possibile fare riferimento alla medesima
# circoscrizione ai fini del completamento delle operazioni precedenti,
# fino a concorrenza dei seggi ancora da cedere, alla coalizione di
# liste o singola lista eccedentaria vengono sottratti i seggi nelle
# circoscrizioni nelle quali li ha ottenuti con le minori parti
# decimali del quoziente di attribuzione e alla coalizione di liste o
# singola lista deficitaria sono conseguentemente attribuiti seggi
# nelle altre circoscrizioni nelle quali abbia le maggiori parti
# decimali del quoziente di attribuzione non utilizzate;
# i) procede quindi all'attribuzione nelle singole circoscrizioni
# dei seggi spettanti alle liste di ciascuna coalizione. A tale fine,
# determina il quoziente circoscrizionale di ciascuna coalizione di
# liste dividendo il totale delle cifre elettorali circoscrizionali
# delle liste ammesse alla ripartizione ai sensi della lettera g),
# primo periodo, per il numero dei seggi assegnati alla coalizione
# nella circoscrizione ai sensi della lettera h). Nell'effettuare la
# divisione di cui al periodo precedente non tiene conto dell'eventuale
# parte frazionaria del quoziente. Divide quindi la cifra elettorale
# circoscrizionale di ciascuna lista della coalizione per tale
# quoziente circoscrizionale. La parte intera del quoziente cosi'
# ottenuto rappresenta il numero dei seggi da assegnare a ciascuna
# lista. I seggi che rimangono ancora da attribuire sono assegnati alle
# liste seguendo la graduatoria decrescente delle parti decimali dei
# quozienti cosi' ottenuti; in caso di parita', sono attribuiti alle
# liste con la maggiore cifra elettorale circoscrizionale; a parita' di
# quest'ultima, si procede a sorteggio. Esclude dall'attribuzione di
# cui al periodo precedente le liste alle quali e' stato attribuito il
# numero di seggi ad esse assegnato a seguito delle operazioni di cui
# alla lettera g). Successivamente l'ufficio accerta se il numero dei
# seggi assegnati in tutte le circoscrizioni a ciascuna lista
# corrisponda al numero dei seggi ad essa attribuito ai sensi della
# lettera g). In caso negativo, procede alle seguenti operazioni,
# iniziando dalla lista che abbia il maggior numero di seggi eccedenti
# e, in caso di parita' di seggi eccedenti da parte di piu' liste, da
# quella che abbia ottenuto la maggiore cifra elettorale nazionale,
# proseguendo poi con le altre liste, in ordine decrescente di seggi
# eccedenti: sottrae i seggi eccedenti alla lista nelle circoscrizioni
# nelle quali essa li ha ottenuti con le parti decimali dei quozienti,
# secondo il loro ordine crescente, e nelle quali inoltre le liste, che
# non abbiano ottenuto il numero di seggi spettante, abbiano parti
# decimali dei quozienti non utilizzate. Conseguentemente, assegna i
# seggi a tali liste. Qualora nella medesima circoscrizione due o piu'
# liste abbiano parti decimali dei quozienti non utilizzate, il seggio
# e' attribuito alla lista con la piu' alta parte decimale del
# quoziente non utilizzata o, in caso di parita', a quella con la
# maggiore cifra elettorale nazionale. Nel caso in cui non sia
# possibile attribuire il seggio eccedentario nella medesima
# circoscrizione, in quanto non vi siano liste deficitarie con parti
# decimali di quozienti non utilizzate, l'Ufficio prosegue, per la
# stessa lista eccedentaria, nell'ordine dei decimali crescenti, a
# individuare un'altra circoscrizione, fino a quando non sia possibile
# sottrarre il seggio eccedentario e attribuirlo ad una lista
# deficitaria nella medesima circoscrizione. Nel caso in cui non sia
# possibile fare riferimento alla medesima circoscrizione ai fini del
# completamento delle operazioni precedenti, fino a concorrenza dei
# seggi ancora da cedere, alla lista eccedentaria vengono sottratti i
# seggi nelle circoscrizioni nelle quali li ha ottenuti con le minori
# parti decimali del quoziente di attribuzione e alle liste deficitarie
# sono conseguentemente attribuiti seggi nelle altre circoscrizioni
# nelle quali abbiano le maggiori parti decimali del quoziente di
# attribuzione non utilizzate.
# 2. L'Ufficio centrale nazionale provvede a comunicare ai singoli
# Uffici centrali circoscrizionali il numero dei seggi assegnati a
# ciascuna lista.
# 3. Di tutte le operazioni dell'Ufficio centrale nazionale viene
# redatto, in duplice esemplare, un apposito verbale: un esemplare e'
# rimesso alla Segreteria generale della Camera dei deputati, la quale
# ne rilascia ricevuta; un altro esemplare e' depositato presso la
# cancelleria della Corte di cassazione.