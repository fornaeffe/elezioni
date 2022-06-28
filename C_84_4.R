# 4. Qualora al termine delle operazioni di cui al comma 3 residuino
# ancora seggi da assegnare alla lista, l'Ufficio centrale nazionale,
# previa apposita comunicazione dell'Ufficio centrale circoscrizionale,
# individua la circoscrizione in cui la lista abbia la maggiore parte
# decimale del quoziente non utilizzata e procede a sua volta ad
# apposita comunicazione all'Ufficio centrale circoscrizionale
# competente. L'Ufficio centrale circoscrizionale provvede
# all'assegnazione dei seggi ai sensi del comma 2. 

donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  
  cat(
    "Devo ancora spostare ",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    " seggi della lista ",
    as.character(ammesse_pluri$LISTA[i]),
    " da ",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    ", ricorro ad altre circoscrizioni.\n",
    sep = ""
  )
  
  circ_accettrici <- aggregate(
    RESTO ~ CIRCOSCRIZIONE,
    ammesse_pluri[
      ammesse_pluri$LISTA == ammesse_pluri$LISTA[i] &
        !ammesse_pluri$DECIMALI_USATI,
    ],
    max
  )

  circ_accettrici <- circ_accettrici[order(
    circ_accettrici$RESTO,
    decreasing = TRUE
  ),]
  
  for (k in seq_along(circ_accettrici$CIRCOSCRIZIONE)) {
    accettori <- which(
      ammesse_pluri$CIRCOSCRIZIONE == circ_accettrici$CIRCOSCRIZIONE[k] &
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
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}

# Qualora al termine
# delle operazioni di cui ai precedenti periodi residuino ancora seggi
# da assegnare alla lista, questi le sono attribuiti nelle altre
# circoscrizioni in cui la stessa lista abbia la maggiore parte
# decimale del quoziente gia' utilizzata, procedendo secondo l'ordine
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
    ", ricorro ad altre circoscrizioni con decimali già usati.\n",
    sep = ""
  )
  
  circ_accettrici <- aggregate(
    RESTO ~ CIRCOSCRIZIONE,
    ammesse_pluri[
      ammesse_pluri$LISTA == ammesse_pluri$LISTA[i] &
        ammesse_pluri$DECIMALI_USATI,
    ],
    max
  )
  
  circ_accettrici <- circ_accettrici[order(
    circ_accettrici$RESTO,
    decreasing = TRUE
  ),]
  
  for (k in seq_along(circ_accettrici$CIRCOSCRIZIONE)) {
    accettori <- which(
      ammesse_pluri$CIRCOSCRIZIONE == circ_accettrici$CIRCOSCRIZIONE[k] &
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
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}

