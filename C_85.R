# Art. 85.
# ((1. Il deputato eletto in piu' collegi plurinominali e' proclamato
#   nel collegio nel quale la lista cui appartiene ha ottenuto la minore
#   cifra elettorale percentuale di collegio plurinominale, determinata
#   ai sensi dell'articolo 77, comma 1, lettera e) )).
# ((1-bis. Il deputato eletto in un collegio uninominale e in uno o
# piu' collegi plurinominali si intende eletto nel collegio
#   uninominale)).

for (i in 1:100) {
  if (sum(ammesse_pluri$SEGGI != ammesse_pluri$ELETTI) > 0) stop(
    "In alcuni collegi il numero di eletti non corrisponde al numero di seggi
  al termine dei subentri"
  )
  
  candidati_pluri$ELETTI <- NULL
  
  candidati_pluri <- merge(
    candidati_pluri,
    ammesse_pluri[,
                  c(
                    "CIRCOSCRIZIONE",
                    "COLLEGIOPLURINOMINALE",
                    "LISTA",
                    "ELETTI",
                    "CIFRA_PERCENTUALE"
                  )
    ]
  )
  
  candidati_pluri <- candidati_pluri[order(
    candidati_pluri$CIRCOSCRIZIONE,
    candidati_pluri$COLLEGIOPLURINOMINALE,
    candidati_pluri$LISTA,
    candidati_pluri$NUMERO
  ),]
  
  candidati_pluri$ORDINE <- ave(
    seq_along(candidati_pluri$CANDIDATO),
    paste(
      candidati_pluri$CIRCOSCRIZIONE,
      candidati_pluri$COLLEGIOPLURINOMINALE,
      candidati_pluri$LISTA
    ),
    FUN = seq_along
  )
  
  candidati_pluri$ELETTO <- candidati_pluri$ORDINE <= candidati_pluri$ELETTI
  
  candidati_pluri <- candidati_pluri[order(
    candidati_pluri$CANDIDATO,
    candidati_pluri$ELETTO,
    candidati_pluri$CIFRA_PERCENTUALE,
    decreasing = c(FALSE, TRUE, FALSE)
  ),]
  
  if (sum(candidati_pluri$ELETTO & duplicated(candidati_pluri$CANDIDATO)) == 0)
    break
  
  candidati_pluri <- candidati_pluri[
    -which(candidati_pluri$ELETTO & duplicated(candidati_pluri$CANDIDATO)),
  ]
  
  ammesse_pluri$CANDIDATI <- NULL
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    aggregate(
      CANDIDATO ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
      candidati_pluri,
      length
    ),
    all.x = TRUE
  )
  
  names(ammesse_pluri)[names(ammesse_pluri) == "CANDIDATO"] <- "CANDIDATI"
  ammesse_pluri$CANDIDATI[is.na(ammesse_pluri$CANDIDATI)] <- 0
  
  ammesse_pluri$ELETTI <- 
    pmin(ammesse_pluri$SEGGI, ammesse_pluri$CANDIDATI)
  
  # 1. Il seggio che rimanga vacante per qualsiasi causa, anche
  # sopravvenuta, ((in un collegio plurinominale)) e' attribuito,
  # nell'ambito del medesimo collegio plurinominale, al candidato ((primo
  # dei non eletti, secondo l'ordine di presentazione)). (42)
  # 2. Nel caso in cui una lista abbia gia' esaurito i propri candidati
  # si procede con le modalita' di cui all'articolo 84, commi 2, 3 ((, 4 e 5)).
  
  subentro()
  subentro(livello = "pluri", uni = TRUE)
  subentro(livello = "circ", uni = TRUE)
  subentro(livello = "naz")
  subentro(livello = "pluri", coal = TRUE)
  subentro(livello = "circ", coal = TRUE)
}

