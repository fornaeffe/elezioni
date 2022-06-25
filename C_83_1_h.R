##### Art. 83 comma 1 lettera h - riparto circoscrizionale ####

# h) procede quindi alla distribuzione nelle singole circoscrizioni
# dei seggi assegnati alle coalizioni di liste o singole liste di cui
# alla lettera e). A tale fine determina il numero di seggi da
# attribuire in ciascuna circoscrizione sottraendo dal numero dei seggi
# spettanti alla circoscrizione stessa ai sensi dell'articolo 3, comma
#         1, il numero dei collegi uninominali costituiti nella circoscrizione.
totali_circ <- merge(
  totali_circ,
  aggregate(
    SEGGI ~ CIRCOSCRIZIONE,
    data = camera_pluri,
    sum
  )
)

#         Divide quindi la somma delle cifre elettorali circoscrizionali delle
#         coalizioni di liste e delle singole liste ammesse al riparto per il
#         numero di seggi da attribuire nella circoscrizione, ottenendo cosi'
# il quoziente elettorale circoscrizionale. Nell'effettuare tale
#         divisione non tiene conto dell'eventuale parte frazionaria del
# quoziente cosi' ottenuto.
cifre_circ <- merge(
  cifre_circ,
  cifre_naz[
    ,
    c(
      "LISTA",
      "SOGGETTO_RIPARTO",
      "SOGLIA3M"
    )
  ]
)

riparto_circ <- aggregate(
  CIFRA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
  data = cifre_circ,
  sum,
  subset = SOGLIA1M
)

totali_circ <- merge(
  totali_circ,
  aggregate(
    CIFRA ~ CIRCOSCRIZIONE,
    data = riparto_circ,
    sum
  ),
  by = "CIRCOSCRIZIONE",
  suffixes = c("","_AMMESSE_AL_RIPARTO")
)

totali_circ$QUOZIENTE <- 
  totali_circ$CIFRA_AMMESSE_AL_RIPARTO %/% totali_circ$SEGGI

# Divide poi la cifra elettorale
#         circoscrizionale di ciascuna coalizione di liste o singola lista per
#         il quoziente elettorale circoscrizionale, ottenendo cosi' il
# quoziente di attribuzione. La parte intera del quoziente di
# attribuzione rappresenta il numero dei seggi da assegnare a ciascuna
# coalizione di liste o singola lista.

riparto_circ <- merge(
  riparto_circ,
  totali_circ[,c("CIRCOSCRIZIONE", "QUOZIENTE")]
)

riparto_circ$PARTE_INTERA <- riparto_circ$CIFRA %/% riparto_circ$QUOZIENTE
riparto_circ$RESTO <- riparto_circ$CIFRA %% riparto_circ$QUOZIENTE

# I seggi che rimangono ancora da
# attribuire sono rispettivamente assegnati alle coalizioni di liste o
# singole liste per le quali queste ultime divisioni hanno dato le
# maggiori parti decimali e, in caso di parita', alle coalizioni di
#         liste o singole liste che hanno conseguito la maggiore cifra
#         elettorale nazionale; a parita' di quest'ultima si procede a
#         sorteggio. Esclude dall'attribuzione di cui al periodo precedente le
# coalizioni di liste o singole liste alle quali e' stato gia'
# attribuito il numero di seggi ad esse assegnato a seguito delle
# operazioni di cui alla lettera f).

riparto_naz <- merge(
  riparto_naz,
  aggregate(
    PARTE_INTERA ~ SOGGETTO_RIPARTO,
    data = riparto_circ,
    sum
  ),
  by = "SOGGETTO_RIPARTO",
  suffixes = c("","_CIRC")
)

riparto_naz$ESCLUSE <- riparto_naz$PARTE_INTERA_CIRC >= riparto_naz$SEGGI

riparto_circ <- merge(
  riparto_circ,
  riparto_naz[,c("SOGGETTO_RIPARTO", "ESCLUSE")]
)

totali_circ <- merge(
  totali_circ,
  aggregate(
    PARTE_INTERA ~ CIRCOSCRIZIONE,
    data = riparto_circ,
    sum
  )
)

totali_circ$DA_ASSEGNARE <- totali_circ$SEGGI - totali_circ$PARTE_INTERA

riparto_circ <- merge(
  riparto_circ,
  totali_circ[,c("CIRCOSCRIZIONE", "DA_ASSEGNARE")]
)

riparto_circ <- riparto_circ[
  order(
    riparto_circ$CIRCOSCRIZIONE,
    riparto_circ$ESCLUSE,
    riparto_circ$RESTO,
    decreasing = c(FALSE, FALSE, TRUE),
    method = "radix"
  ),
]

riparto_circ$ORDINE[!riparto_circ$ESCLUSE] <- ave(
  seq_along(riparto_circ$CIRCOSCRIZIONE[!riparto_circ$ESCLUSE]),
  riparto_circ$CIRCOSCRIZIONE[!riparto_circ$ESCLUSE],
  FUN = seq_along
)

riparto_circ$SEGGIO_DA_RESTI <- riparto_circ$ORDINE <= riparto_circ$DA_ASSEGNARE
riparto_circ$SEGGIO_DA_RESTI[is.na(riparto_circ$SEGGIO_DA_RESTI)] <- FALSE

riparto_circ$SEGGI <- riparto_circ$PARTE_INTERA + riparto_circ$SEGGIO_DA_RESTI

# Successivamente l'Ufficio accerta
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