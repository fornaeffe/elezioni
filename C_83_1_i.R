##### Art. 83 comma 1 lettera i - riparto circoscrizionale interno alle coalizioni ####

# i) procede quindi all'attribuzione nelle singole circoscrizioni
# dei seggi spettanti alle liste di ciascuna coalizione. A tale fine,
# determina il quoziente circoscrizionale di ciascuna coalizione di
# liste dividendo il totale delle cifre elettorali circoscrizionali
# delle liste ammesse alla ripartizione ai sensi della lettera g),
# primo periodo, per il numero dei seggi assegnati alla coalizione
# nella circoscrizione ai sensi della lettera h). Nell'effettuare la
# divisione di cui al periodo precedente non tiene conto dell'eventuale
# parte frazionaria del quoziente.

ammesse_circ <- liste_circ[
  liste_circ$SOGLIA3M,
  c(
    "CIRCOSCRIZIONE",
    "SOGGETTO_RIPARTO",
    "LISTA",
    "CIFRA"
  )
]

riparto_circ <- merge(
  riparto_circ,
  aggregate(
    CIFRA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
    data = ammesse_circ,
    sum
  ),
  by = c("CIRCOSCRIZIONE", "SOGGETTO_RIPARTO"),
  suffixes = c("","_AMMESSE_AL_RIPARTO")
)

riparto_circ$QUOZIENTE_COAL <- 
  riparto_circ$CIFRA_AMMESSE_AL_RIPARTO %/% riparto_circ$SEGGI_FLIPPER

# Divide quindi la cifra elettorale
# circoscrizionale di ciascuna lista della coalizione per tale
# quoziente circoscrizionale. La parte intera del quoziente cosi'
# ottenuto rappresenta il numero dei seggi da assegnare a ciascuna
# lista.

ammesse_circ <- merge(
  ammesse_circ,
  riparto_circ[, c("CIRCOSCRIZIONE", "SOGGETTO_RIPARTO", "QUOZIENTE_COAL")]
)

ammesse_circ$PARTE_INTERA <- ammesse_circ$CIFRA %/% ammesse_circ$QUOZIENTE_COAL
ammesse_circ$RESTO <- ( ammesse_circ$CIFRA / ammesse_circ$QUOZIENTE_COAL ) %% 1

# I seggi che rimangono ancora da attribuire sono assegnati alle
# liste seguendo la graduatoria decrescente delle parti decimali dei
# quozienti cosi' ottenuti; in caso di parita', sono attribuiti alle
# liste con la maggiore cifra elettorale circoscrizionale; a parita' di
# quest'ultima, si procede a sorteggio. Esclude dall'attribuzione di
# cui al periodo precedente le liste alle quali e' stato attribuito il
# numero di seggi ad esse assegnato a seguito delle operazioni di cui
# alla lettera g).

ammesse_naz <- merge(
  ammesse_naz,
  aggregate(
    PARTE_INTERA ~ LISTA,
    ammesse_circ,
    sum
  ),
  by = "LISTA",
  suffixes = c("", "_CIRC")
)

ammesse_naz$ESCLUSE <- ammesse_naz$PARTE_INTERA_CIRC >= ammesse_naz$SEGGI

ammesse_circ <- merge(
  ammesse_circ,
  ammesse_naz[,c("LISTA", "ESCLUSE", "CIFRA")],
  by = "LISTA",
  suffixes = c("", "_NAZ")
)

riparto_circ <- merge(
  riparto_circ,
  aggregate(
    PARTE_INTERA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
    ammesse_circ,
    sum
  ),
  by = c("CIRCOSCRIZIONE", "SOGGETTO_RIPARTO"),
  suffixes = c("", "_TOT")
)

riparto_circ$DA_ASSEGNARE_COAL <- 
  riparto_circ$SEGGI_FLIPPER - riparto_circ$PARTE_INTERA_TOT

ammesse_circ <- merge(
  ammesse_circ,
  riparto_circ[,c(
    "CIRCOSCRIZIONE", 
    "SOGGETTO_RIPARTO",
    "DA_ASSEGNARE_COAL"
  )]
)

ammesse_circ <- ammesse_circ[
  order(
    ammesse_circ$CIRCOSCRIZIONE,
    ammesse_circ$SOGGETTO_RIPARTO,
    ammesse_circ$ESCLUSE,
    ammesse_circ$RESTO,
    ammesse_circ$CIFRA,
    decreasing = c(FALSE, FALSE, FALSE, TRUE, TRUE)
  ),
]

ammesse_circ$ORDINE[!ammesse_circ$ESCLUSE] <- ave(
  seq_along(ammesse_circ$SOGGETTO_RIPARTO[!ammesse_circ$ESCLUSE]),
  paste(
    ammesse_circ$CIRCOSCRIZIONE[!ammesse_circ$ESCLUSE],
    ammesse_circ$SOGGETTO_RIPARTO[!ammesse_circ$ESCLUSE]
  ),
  FUN = seq_along
)

ammesse_circ$SEGGIO_DA_RESTO <- 
  ammesse_circ$ORDINE <= ammesse_circ$DA_ASSEGNARE_COAL
ammesse_circ$SEGGIO_DA_RESTO[is.na(ammesse_circ$SEGGIO_DA_RESTO)] <- FALSE

ammesse_circ$SEGGI <- ammesse_circ$PARTE_INTERA + ammesse_circ$SEGGIO_DA_RESTO

# Successivamente l'ufficio accerta se il numero dei
# seggi assegnati in tutte le circoscrizioni a ciascuna lista
# corrisponda al numero dei seggi ad essa attribuito ai sensi della
# lettera g).

ammesse_naz <- merge(
  ammesse_naz,
  aggregate(
    SEGGI ~ LISTA,
    ammesse_circ,
    sum
  ),
  by = "LISTA",
  suffixes = c("", "_CIRC")
)

ammesse_naz$SEGGI_ECCEDENTI <- ammesse_naz$SEGGI_CIRC - ammesse_naz$SEGGI

# In caso negativo, procede alle seguenti operazioni,
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

ammesse_naz <- ammesse_naz[
  order(
    ammesse_naz$SEGGI_ECCEDENTI,
    ammesse_naz$CIFRA,
    decreasing = TRUE
  ),
]

ammesse_naz$SEGGI_ECCEDENTI_CONTATORE <- ammesse_naz$SEGGI_ECCEDENTI

ammesse_circ$FLIPPER <- 0

for (i in seq_along(ammesse_naz$LISTA)) {
  if (ammesse_naz$SEGGI_ECCEDENTI[i] < 1) break
  
  s <- ammesse_naz$SOGGETTO_RIPARTO[i]
  l <- ammesse_naz$LISTA[i]
  for (j in 1:ammesse_naz$SEGGI_ECCEDENTI[i]) {
    ammesse_circ$DEFICIT <- 
      ammesse_circ$SOGGETTO_RIPARTO == s &
      ammesse_circ$LISTA %in% ammesse_naz$LISTA[
        ammesse_naz$SEGGI_ECCEDENTI_CONTATORE < 0
      ] &
      !ammesse_circ$SEGGIO_DA_RESTO &
      ammesse_circ$FLIPPER == 0
    
    ac <- ammesse_circ[
      ammesse_circ$LISTA == l &
        ammesse_circ$SEGGIO_DA_RESTO &
        ammesse_circ$FLIPPER == 0,
    ]
    
    if (dim(ac)[1] < 1) stop(
      "Devo togliere un seggio a ", l, " ma non ci sono circoscrizioni dove ",
      "questo sia stato ottenuto con i resti."
    )
    
    ac$DEFICIT_PRESENTE <- ac$CIRCOSCRIZIONE %in% 
      ammesse_circ$CIRCOSCRIZIONE[ammesse_circ$DEFICIT]
    
    ac <- ac[
      order(
        ac$DEFICIT_PRESENTE,
        ac$RESTO,
        decreasing = c(TRUE, FALSE)
      ),
    ]
    
    c <- ac$CIRCOSCRIZIONE[1]
    
    if (ac$DEFICIT_PRESENTE[1]) {
      ac2 <- ammesse_circ[
        ammesse_circ$CIRCOSCRIZIONE == c &
          ammesse_circ$SOGGETTO_RIPARTO == s &
          ammesse_circ$DEFICIT,
      ]
    } else {
      ac2 <- ammesse_circ[
        ammesse_circ$SOGGETTO_RIPARTO == s &
          ammesse_circ$DEFICIT,
      ]
    }
    
    if (dim(ac2)[1] < 1) stop(
      "Devo togliere un seggio a ", l, " ma non ho a chi darlo."
    )
    
    ac2 <- ac2[order(
      ac2$RESTO,
      ac2$CIFRA_NAZ,
      decreasing = TRUE
    ),]
    
    l2 <- ac2$LISTA[1]
    c2 <- ac2$CIRCOSCRIZIONE[1]
    
    ammesse_circ$FLIPPER[
      ammesse_circ$LISTA == l &
        ammesse_circ$CIRCOSCRIZIONE == c
    ] <- -1
    
    ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l] <-
      ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l] - 1
    
    ammesse_circ$FLIPPER[
      ammesse_circ$LISTA == l2 &
        ammesse_circ$CIRCOSCRIZIONE == c2
    ] <- 1
    
    ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l2] <-
      ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l2] - 1
    
    cat(
      "Tolgo un seggio a", levels(l)[l], "in", levels(c)[c],
      "per darlo a", levels(l2)[l2], "in", levels(c2)[c2], "\n"
    )
  }
}

ammesse_circ$SEGGI_FLIPPER <- ammesse_circ$SEGGI + ammesse_circ$FLIPPER
