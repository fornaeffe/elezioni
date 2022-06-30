##### Art. 77 comma 1 lettera c - liste cifra uninominale ####

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
    CANDIDATO
  , 
  data = dati$camera_voti_lista_per_comune,
  sum
)
voti_lista_per_candidato <- merge(voti_lista_per_candidato, candidati_uni)
voti_lista_per_candidato$VOTI_SOLO_CANDIDATO <-
  voti_lista_per_candidato$VOTI_CANDIDATO - voti_lista_per_candidato$VOTI_LISTA

voti_lista_per_candidato$QUOZIENTE_RIPARTIZIONE <-
  voti_lista_per_candidato$VOTI_LISTA / voti_lista_per_candidato$VOTI_SOLO_CANDIDATO

cifre_uni <- aggregate(
  VOTI_LISTA ~
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    CANDIDATO +
    LISTA
  ,
  data = dati$camera_voti_lista_per_comune,
  sum
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
  ]
)

cifre_uni$PARTE_INTERA <- 
  cifre_uni$VOTI_LISTA %/% cifre_uni$QUOZIENTE_RIPARTIZIONE
cifre_uni$RESTO <- 
  cifre_uni$VOTI_LISTA %% cifre_uni$QUOZIENTE_RIPARTIZIONE

cifre_uni$PARTE_INTERA[
  cifre_uni$PARTE_INTERA < 0 |
    is.na(cifre_uni$PARTE_INTERA) | 
    is.nan(cifre_uni$PARTE_INTERA)
] <- 0

voti_assegnati <- aggregate(
  PARTE_INTERA ~
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    CANDIDATO
  ,
  data = cifre_uni,
  sum
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
  ]
)

cifre_uni <- cifre_uni[
  order(
    cifre_uni$CIRCOSCRIZIONE, 
    cifre_uni$COLLEGIOPLURINOMINALE, 
    cifre_uni$COLLEGIOUNINOMINALE,
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

cifre_uni$VOTI_DA_RESTI[
  is.na(cifre_uni$VOTI_DA_RESTI) | is.nan(cifre_uni$VOTI_DA_RESTI)
] <- 0

cifre_uni$CIFRA <- 
  cifre_uni$VOTI_LISTA + 
  cifre_uni$PARTE_INTERA + 
  cifre_uni$VOTI_DA_RESTI
