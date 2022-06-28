# 5. Qualora al termine delle operazioni di cui al comma 4 residuino
# ancora seggi da assegnare ad una lista in un collegio plurinominale,
# questi sono attribuiti, nell'ambito del collegio plurinominale
# originario, alla lista facente parte della medesima coalizione della
# lista deficitaria che abbia la maggiore parte decimale del quoziente
# non utilizzata, procedendo secondo l'ordine decrescente; esaurite le
# liste con la parte decimale del quoziente non utilizzata, si procede
# con le liste facenti parte della medesima coalizione, sulla base
# delle parti decimali del quoziente gia' utilizzate, secondo l'ordine
# decrescente. 


donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  cat(
    "Devo ancora spostare ",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    " seggi della lista ",
    as.character(ammesse_pluri$LISTA[i]),
    " da ",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    ", ricorro a liste della stessa coalizione nello stesso collegio plurinominale.\n",
    sep = ""
  )
  
  accettori <- which(
    ammesse_pluri$COLLEGIOPLURINOMINALE == ammesse_pluri$COLLEGIOPLURINOMINALE[i] &
      ammesse_pluri$LISTA %in% dati$camera_coalizioni$LISTA[
        dati$camera_coalizioni$COALIZIONE == dati$camera_coalizioni$COALIZIONE[
          dati$camera_coalizioni$LISTA == ammesse_pluri$LISTA[i]
        ]
      ] &
      ammesse_pluri$ELETTI < ammesse_pluri$CANDIDATI
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
      "seggi dalla lista",
      as.character(ammesse_pluri$LISTA[i]),
      "nel collegio",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
      "alla lista",
      as.character(ammesse_pluri$LISTA[j]),
      "nel collegio",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[j]),
      "\n"
    )
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}

# Qualora al termine delle operazioni di cui al primo
# periodo residuino ancora seggi da assegnare alla lista, questi sono
# attribuiti alle liste facenti parte della medesima coalizione negli
# altri collegi plurinominali della circoscrizione, partendo da quello
# in cui la coalizione abbia la maggiore parte decimale del quoziente
# non utilizzata e procedendo secondo quanto previsto dal primo
# periodo; si procede successivamente nei collegi plurinominali in cui
# la coalizione abbia la maggiore parte decimale del quoziente gia'
# utilizzata, secondo l'ordine decrescente.

donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  cat(
    "Devo ancora spostare ",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    " seggi della lista ",
    as.character(ammesse_pluri$LISTA[i]),
    " da ",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    ", ricorro a liste della stessa coalizione nello stesso collegio plurinominale.\n",
    sep = ""
  )
  
  accettori <- which(
    ammesse_pluri$CIRCOSCRIZIONE == ammesse_pluri$CIRCOSCRIZIONE[i] &
      ammesse_pluri$LISTA %in% dati$camera_coalizioni$LISTA[
        dati$camera_coalizioni$COALIZIONE == dati$camera_coalizioni$COALIZIONE[
          dati$camera_coalizioni$LISTA == ammesse_pluri$LISTA[i]
        ]
      ] &
      ammesse_pluri$ELETTI < ammesse_pluri$CANDIDATI
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
      "seggi dalla lista",
      as.character(ammesse_pluri$LISTA[i]),
      "nel collegio",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
      "alla lista",
      as.character(ammesse_pluri$LISTA[j]),
      "nel collegio",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[j]),
      "\n"
    )
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}


