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

candidati_uni <- merge(
  candidati_uni,
  aggregate(
    VOTI_LISTA ~ COLLEGIOUNINOMINALE + CANDIDATO, 
    liste_comune,
    sum
  ),
)

candidati_uni$VOTI_SOLO_CANDIDATO <-
  candidati_uni$VOTI_CANDIDATO - candidati_uni$VOTI_LISTA

candidati_uni$QUOZIENTE <-
  candidati_uni$VOTI_LISTA / candidati_uni$VOTI_SOLO_CANDIDATO



liste_uni <- aggregate(
  VOTI_LISTA ~
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    LISTA
  ,
  liste_comune,
  sum
)

liste_uni <- merge(
  liste_uni,
  unique(liste_comune[, c("COLLEGIOUNINOMINALE", "CANDIDATO", "LISTA")])
)


liste_uni <- merge(
  liste_uni, 
  candidati_uni[, c(
    "COLLEGIOUNINOMINALE",
    "CANDIDATO",
    "QUOZIENTE"
  )],
  all.x = TRUE
)

liste_uni$PARTE_INTERA <- 
  liste_uni$VOTI_LISTA %/% liste_uni$QUOZIENTE
liste_uni$RESTO <- 
  liste_uni$VOTI_LISTA %% liste_uni$QUOZIENTE

liste_uni$PARTE_INTERA[
  liste_uni$PARTE_INTERA < 0 |
    is.na(liste_uni$PARTE_INTERA) | 
    is.nan(liste_uni$PARTE_INTERA)
] <- 0

candidati_uni <- merge(
  candidati_uni,
  aggregate(
    PARTE_INTERA ~ COLLEGIOUNINOMINALE + CANDIDATO,
    liste_uni,
    sum
  )
)
candidati_uni$DA_ASSEGNARE <-
  candidati_uni$VOTI_SOLO_CANDIDATO - candidati_uni$PARTE_INTERA

liste_uni <- merge(
  liste_uni, 
  candidati_uni[, c("COLLEGIOUNINOMINALE", "CANDIDATO", "DA_ASSEGNARE")],
  all.x = TRUE
)

liste_uni <- liste_uni[order(
  liste_uni$CIRCOSCRIZIONE, 
  liste_uni$COLLEGIOPLURINOMINALE, 
  liste_uni$COLLEGIOUNINOMINALE,
  liste_uni$CANDIDATO,  
  liste_uni$RESTO,
  decreasing = c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE")
), ]

liste_uni$ORDINE <- ave(
  seq_along(liste_uni$CIRCOSCRIZIONE),
  paste(liste_uni$COLLEGIOUNINOMINALE, liste_uni$CANDIDATO),
  FUN = seq_along
)

liste_uni$VOTO_DA_RESTO <- liste_uni$ORDINE <= liste_uni$DA_ASSEGNARE

liste_uni$VOTO_DA_RESTO[
  is.na(liste_uni$VOTO_DA_RESTO) | is.nan(liste_uni$VOTO_DA_RESTO)
] <- 0

liste_uni$CIFRA <- 
  liste_uni$VOTI_LISTA + 
  liste_uni$PARTE_INTERA + 
  liste_uni$VOTO_DA_RESTO
