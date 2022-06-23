

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

