##### Art. 83 bis - riparto plurinominali ####

# Art. 83-bis.
# ((1. L'Ufficio centrale circoscrizionale, ricevute da parte
# dell'Ufficio elettorale centrale nazionale le comunicazioni di cui
#   all'articolo 83, comma 2, procede all'attribuzione nei singoli
#   collegi plurinominali dei seggi spettanti alle liste. A tale fine
#   l'ufficio determina il quoziente elettorale di collegio dividendo la
# somma delle cifre elettorali di collegio di tutte le liste per il
# numero dei seggi da attribuire nel collegio stesso. Nell'effettuare
#   tale divisione non tiene conto dell'eventuale parte frazionaria del
# quoziente.

liste_pluri <- merge(
  liste_pluri,
  liste_naz[
    ,
    c("LISTA", "SOGLIA3M")
  ]
)

ammesse_pluri <- liste_pluri[
  liste_pluri$SOGLIA3M,
  c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "LISTA",
    "CIFRA",
    "CIFRA_PERCENTUALE"
  )
]

totali_pluri <- merge(
  totali_pluri,
  aggregate(
    CIFRA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
    ammesse_pluri,
    sum
  ),
  by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE"),
  suffixes = c("", "_AMMESSE_AL_RIPARTO")
)

totali_pluri <- merge(
  totali_pluri,
  dati$camera_pluri
)

totali_pluri$QUOZIENTE <-
  totali_pluri$CIFRA_AMMESSE_AL_RIPARTO %/% totali_pluri$SEGGI

# Divide quindi la cifra elettorale di collegio di ciascuna
# lista per tale quoziente di collegio. La parte intera del quoziente
# cosi' ottenuto rappresenta il numero dei seggi da assegnare a
#   ciascuna lista.

ammesse_pluri <- merge(
  ammesse_pluri,
  totali_pluri[, c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "QUOZIENTE")]
)

ammesse_pluri$PARTE_INTERA <- ammesse_pluri$CIFRA %/% ammesse_pluri$QUOZIENTE
ammesse_pluri$DECIMALI <- ( ammesse_pluri$CIFRA / ammesse_pluri$QUOZIENTE ) %% 1


# I seggi che rimangono ancora da attribuire sono
#   assegnati alle liste seguendo la graduatoria decrescente delle parti
#   decimali dei quozienti cosi' ottenuti; in caso di parita', sono
#   attribuiti alle liste con la maggiore cifra elettorale
#   circoscrizionale; a parita' di quest'ultima, si procede a sorteggio.
#   L'Ufficio esclude dall'attribuzione di cui al periodo precedente le
#   liste alle quali e' stato attribuito il numero di seggi ad esse
# assegnato nella circoscrizione secondo la comunicazione di cui
# all'articolo 83, comma 2.

ammesse_circ <- merge(
  ammesse_circ,
  aggregate(
    PARTE_INTERA ~ CIRCOSCRIZIONE + LISTA,
    ammesse_pluri,
    sum
  ),
  by = c("CIRCOSCRIZIONE", "LISTA"),
  suffixes = c("", "_PLURI")
)

ammesse_circ$ESCLUSE_PLURI <- 
  ammesse_circ$PARTE_INTERA_PLURI >= ammesse_circ$SEGGI_FLIPPER

ammesse_pluri <- merge(
  ammesse_pluri,
  ammesse_circ[,c("CIRCOSCRIZIONE", "LISTA", "ESCLUSE_PLURI", "CIFRA")],
  by = c("CIRCOSCRIZIONE", "LISTA"),
  suffixes = c("", "_CIRC")
)

totali_pluri <- merge(
  totali_pluri,
  aggregate(
    PARTE_INTERA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
    ammesse_pluri,
    sum
  )
)

totali_pluri$DA_ASSEGNARE <- totali_pluri$SEGGI - totali_pluri$PARTE_INTERA

ammesse_pluri <- merge(
  ammesse_pluri,
  totali_pluri[,c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "DA_ASSEGNARE")]
)

ammesse_pluri <- ammesse_pluri[
  order(
    ammesse_pluri$CIRCOSCRIZIONE,
    ammesse_pluri$COLLEGIOPLURINOMINALE,
    ammesse_pluri$ESCLUSE_PLURI,
    ammesse_pluri$DECIMALI,
    ammesse_pluri$CIFRA_CIRC,
    decreasing = c(FALSE, FALSE, FALSE, TRUE, TRUE)
  ),
]

ammesse_pluri$ORDINE[!ammesse_pluri$ESCLUSE_PLURI] <- ave(
  seq_along(ammesse_pluri$COLLEGIOPLURINOMINALE[!ammesse_pluri$ESCLUSE_PLURI]),
  paste(
    ammesse_pluri$CIRCOSCRIZIONE[!ammesse_pluri$ESCLUSE_PLURI],
    ammesse_pluri$COLLEGIOPLURINOMINALE[!ammesse_pluri$ESCLUSE_PLURI]
  ),
  FUN = seq_along
)

ammesse_pluri$SEGGIO_DA_DECIMALI <- 
  ammesse_pluri$ORDINE <= ammesse_pluri$DA_ASSEGNARE
ammesse_pluri$SEGGIO_DA_DECIMALI[is.na(ammesse_pluri$SEGGIO_DA_DECIMALI)] <- FALSE

ammesse_pluri$SEGGI <- 
  ammesse_pluri$PARTE_INTERA + ammesse_pluri$SEGGIO_DA_DECIMALI

# Successivamente l'ufficio accerta se il
# numero dei seggi assegnati in tutti i collegi a ciascuna lista
# corrisponda al numero di seggi ad essa attribuito nella
# circoscrizione dall'Ufficio elettorale centrale nazionale.

ammesse_circ <- merge(
  ammesse_circ,
  aggregate(
    SEGGI ~ CIRCOSCRIZIONE + LISTA,
    ammesse_pluri,
    sum
  ),
  by = c("CIRCOSCRIZIONE", "LISTA"),
  suffixes = c("", "_PLURI")
)

ammesse_circ$SEGGI_ECCEDENTI <- 
  ammesse_circ$SEGGI_PLURI - ammesse_circ$SEGGI_FLIPPER

# In caso
#   negativo, determina la lista che ha il maggior numero di seggi
#   eccedentari e, a parita' di essi, la lista che tra queste ha ottenuto
# il seggio eccedentario con la minore parte decimale del quoziente;
# sottrae quindi il seggio a tale lista nel collegio in cui e' stato
#   ottenuto con la minore parte decimale dei quozienti di attribuzione e
#   lo assegna alla lista deficitaria che ha il maggior numero di seggi
#   deficitari e, a parita' di essi, alla lista che tra queste ha la
# maggiore parte decimale del quoziente che non ha dato luogo
# all'assegnazione di seggio; il seggio e' assegnato alla lista
# deficitaria nel collegio plurinominale in cui essa ha la maggiore
# parte decimale del quoziente di attribuzione non utilizzata; ripete
# quindi, in successione, tali operazioni sino all'assegnazione di
#   tutti i seggi eccedentari alle liste deficitarie)).

ammesse_pluri <- merge(
  ammesse_pluri,
  ammesse_circ[
    ,
    c("CIRCOSCRIZIONE", "LISTA", "SEGGI_ECCEDENTI")
  ]
)

ammesse_pluri$CEDE <- 
  ammesse_pluri$SEGGI_ECCEDENTI > 0 & ammesse_pluri$SEGGIO_DA_DECIMALI

ammesse_pluri$RICEVE <-
  ammesse_pluri$SEGGI_ECCEDENTI < 0 & !ammesse_pluri$SEGGIO_DA_DECIMALI

ammesse_pluri <- ammesse_pluri[order(
  ammesse_pluri$CIRCOSCRIZIONE,
  ammesse_pluri$SEGGIO_DA_DECIMALI,
  ammesse_pluri$SEGGI_ECCEDENTI,
  ammesse_pluri$DECIMALI,
  decreasing = c(FALSE, TRUE, TRUE, FALSE)
),]

ammesse_pluri$ORDINE_CEDE[ammesse_pluri$CEDE] <- ave(
  seq_along(ammesse_pluri$LISTA[ammesse_pluri$CEDE]),
  paste(
    ammesse_pluri$CIRCOSCRIZIONE,
    ammesse_pluri$LISTA
  )[ammesse_pluri$CEDE],
  FUN = seq_along
)

ammesse_pluri$CEDUTO <- 
  ammesse_pluri$ORDINE_CEDE <= ammesse_pluri$SEGGI_ECCEDENTI

ammesse_pluri$CEDUTO[is.na(ammesse_pluri$CEDUTO)] <- FALSE

ammesse_pluri <- ammesse_pluri[order(
  ammesse_pluri$CIRCOSCRIZIONE,
  ammesse_pluri$SEGGIO_DA_DECIMALI,
  ammesse_pluri$SEGGI_ECCEDENTI,
  ammesse_pluri$DECIMALI,
  decreasing = c(FALSE, FALSE, FALSE, TRUE)
),]

ammesse_pluri$ORDINE_RICEVE[ammesse_pluri$RICEVE] <- ave(
  seq_along(ammesse_pluri$LISTA[ammesse_pluri$RICEVE]),
  paste(
    ammesse_pluri$CIRCOSCRIZIONE,
    ammesse_pluri$LISTA
  )[ammesse_pluri$RICEVE],
  FUN = seq_along
)

ammesse_pluri$RICEVUTO <- 
  ammesse_pluri$ORDINE_RICEVE <= - ammesse_pluri$SEGGI_ECCEDENTI

ammesse_pluri$RICEVUTO[is.na(ammesse_pluri$RICEVUTO)] <- FALSE

ammesse_pluri$SEGGI_FLIPPER <- 
  ammesse_pluri$SEGGI - ammesse_pluri$CEDUTO + ammesse_pluri$RICEVUTO

# 2. Di tutte le operazioni dell'Ufficio centrale circoscrizionale
# viene redatto, in duplice esemplare, apposito verbale: un esemplare
# e' rimesso alla Segreteria generale della Camera dei deputati, la
# quale ne rilascia ricevuta; un altro esemplare e' depositato presso
# la cancelleria della Corte di cassazione.
