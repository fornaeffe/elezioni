##### Art. 84 commi 1 e 2 - nomine pluri ####

# Art. 84.
# ((1. Al termine delle operazioni di cui agli articoli precedenti,
#   l'Ufficio centrale circoscrizionale proclama eletti in ciascun
# collegio plurinominale, nei limiti dei seggi ai quali ciascuna lista
# ha diritto, i candidati compresi nella lista del collegio, secondo
# l'ordine di presentazione.



#   2. Qualora una lista abbia esaurito il numero dei candidati
#   presentati in un collegio plurinominale e non sia quindi possibile
#   attribuire tutti i seggi a essa spettanti in quel collegio, l'Ufficio
# centrale circoscrizionale assegna i seggi alla lista negli altri
# collegi plurinominali della stessa circoscrizione in cui la lista
# medesima abbia la maggiore parte decimale del quoziente non
# utilizzata, procedendo secondo l'ordine decrescente. Qualora al
#   termine di detta operazione residuino ancora seggi da assegnare alla
#   lista, questi le sono attribuiti negli altri collegi plurinominali
#   della stessa circoscrizione in cui la lista medesima abbia la
#   maggiore parte decimale del quoziente gia' utilizzata, procedendo
# secondo l'ordine decrescente.

ammesse_pluri$SEGGI <- ammesse_pluri$SEGGI_FLIPPER

ammesse_pluri$DECIMALI_USATI <-
  ammesse_pluri$SEGGIO_DA_RESTI + 
  ammesse_pluri$RICEVUTO - 
  ammesse_pluri$CEDUTO > 0

ammesse_pluri <- merge(
  ammesse_pluri,
  aggregate(
    CANDIDATO ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
    dati$camera_candidati_pluri,
    length
  )
)

names(ammesse_pluri)[names(ammesse_pluri) == "CANDIDATO"] <- "CANDIDATI"

ammesse_pluri$ELETTI <- 
  pmin(ammesse_pluri$SEGGI, ammesse_pluri$CANDIDATI)

donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  
  cat(
    "Devo spostare",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    "seggi della lista",
    as.character(ammesse_pluri$LISTA[i]),
    "da",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    "\n"
  )
  
  accettori <- which(
    ammesse_pluri$CIRCOSCRIZIONE == ammesse_pluri$CIRCOSCRIZIONE[i] &
      ammesse_pluri$LISTA == ammesse_pluri$LISTA[i] &
      ammesse_pluri$ELETTI < ammesse_pluri$CANDIDATI,
  )
  
  accettori <- accettori[order(
    ammesse_pluri$DECIMALI_USATI[accettori],
    ammesse_pluri$RESTO[accettori],
    decreasing = c(FALSE, TRUE)
  )]
  
  for (j in accettori) {
    seggi_spostati <- min(
      ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
      ammesse_pluri$CANDIDATI[j] - ammesse_pluri$ELETTI[j] 
    )
    
    ammesse_pluri$SEGGI[i] <- ammesse_pluri$SEGGI[i] - seggi_spostati
    ammesse_pluri$SEGGI[j] <- ammesse_pluri$SEGGI[j] + seggi_spostati
    
    ammesse_pluri$ELETTI[c(i,j)] <- 
      pmin(ammesse_pluri$SEGGI[c(i,j)], ammesse_pluri$CANDIDATI[c(i,j)])
    
    cat(
      "Ho spostato",
      seggi_spostati,
      "seggi da",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
      "a",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[j]),
      "\n"
    )
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}

