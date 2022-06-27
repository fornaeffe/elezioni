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

# 5. Qualora al termine delle operazioni di cui al comma 4 residuino
# ancora seggi da assegnare ad una lista in un collegio plurinominale,
# questi sono attribuiti, nell'ambito del collegio plurinominale
# originario, alla lista facente parte della medesima coalizione della
# lista deficitaria che abbia la maggiore parte decimale del quoziente
# non utilizzata, procedendo secondo l'ordine decrescente; esaurite le
# liste con la parte decimale del quoziente non utilizzata, si procede
# con le liste facenti parte della medesima coalizione, sulla base
# delle parti decimali del quoziente gia' utilizzate, secondo l'ordine
# decrescente. Qualora al termine delle operazioni di cui al primo
# periodo residuino ancora seggi da assegnare alla lista, questi sono
# attribuiti alle liste facenti parte della medesima coalizione negli
# altri collegi plurinominali della circoscrizione, partendo da quello
# in cui la coalizione abbia la maggiore parte decimale del quoziente
# non utilizzata e procedendo secondo quanto previsto dal primo
# periodo; si procede successivamente nei collegi plurinominali in cui
# la coalizione abbia la maggiore parte decimale del quoziente gia'
# utilizzata, secondo l'ordine decrescente.
# 6. Qualora al termine delle operazioni di cui al comma 5 residuino
# ancora seggi da assegnare ad una lista, questi sono attribuiti ai
# candidati della lista nei collegi uninominali non proclamati eletti
# nelle altre circoscrizioni, secondo la graduatoria di cui
# all'articolo 77, comma 1, lettera h). A tale fine si procede con le
# modalita' previste dal comma 4.
# 7. Qualora al termine delle operazioni di cui al comma 6 residuino
# ancora seggi da assegnare ad una lista, questi sono attribuiti alle
# liste facenti parte della medesima coalizione della lista deficitaria
# nelle altre circoscrizioni. A tale fine si procede con le modalita'
# previste dai commi 4 e 5.
# 8. Nell'effettuare le operazioni di cui ai precedenti commi, in
# caso di parita' della parte decimale del quoziente, si procede
# mediante sorteggio.
# 9. Dell'avvenuta proclamazione effettuata ai sensi del presente
# articolo il presidente dell'Ufficio centrale circoscrizionale invia
# attestato ai deputati proclamati e ne da' immediata notizia alla
# segreteria generale della Camera dei deputati nonche' alle singole
# prefetture-uffici territoriali del Governo, che la portano a
# conoscenza del pubblico)).