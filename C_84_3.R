#   3. Qualora al termine delle operazioni di cui al comma 2 residuino
#   ancora seggi da assegnare ad una lista, questi sono attribuiti,
#   nell'ambito del collegio plurinominale originario, ai candidati della
# lista nei collegi uninominali non proclamati eletti secondo la
# graduatoria di cui all'articolo 77, comma 1, lettera h).

donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  
  cat(
    "Devo ancora spostare ",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    " seggi della lista ",
    as.character(ammesse_pluri$LISTA[i]),
    " da ",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    ", ricorro ai candidati uninominali del collegio plurinominale.\n",
    sep = ""
  )
  
  accettori <- which(
    cifre_ind$COLLEGIOPLURINOMINALE == ammesse_pluri$COLLEGIOPLURINOMINALE[i] &
      cifre_ind$ELETTO == FALSE &
      cifre_ind$CANDIDATO %in% cifre_uni$CANDIDATO[
        cifre_uni$LISTA == ammesse_pluri$LISTA[i]
      ]
  )
  
  accettori <- accettori[order(
    cifre_ind$CIFRA_PERCENTUALE[accettori],
    cifre_ind$DATA_NASCITA[accettori],
    decreasing = c(TRUE, TRUE)
  )]
  
  seggi_spostati <- min(
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    length(accettori)
  )
  
  for (j in seggi_spostati) {
    ammesse_pluri$SEGGI[i] <- ammesse_pluri$SEGGI[i] - 1
    cifre_ind$ELETTO[accettori[j]] <- TRUE
    # cifre_ind$RIPESCATO[accettori[j]] <- TRUE
    
    cat(
      "Ho ripescato",
      as.character(cifre_ind$CANDIDATO[accettori[j]]),
      "\n"
    )
  }
}

# Qualora
#   residuino ancora seggi da assegnare alla lista, questi sono
#   attribuiti ai candidati della lista nei collegi uninominali non
#   proclamati eletti nell'ambito della circoscrizione, secondo la
# graduatoria di cui all'articolo 77, comma 1, lettera h).

donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  
  cat(
    "Devo ancora spostare ",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    " seggi della lista ",
    as.character(ammesse_pluri$LISTA[i]),
    " da ",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    ", ricorro ai candidati uninominali della circoscrizione.\n",
    sep = ""
  )
  
  accettori <- which(
    cifre_ind$CIRCOSCRIZIONE == ammesse_pluri$CIRCOSCRIZIONE[i] &
      cifre_ind$ELETTO == FALSE &
      cifre_ind$CANDIDATO %in% cifre_uni$CANDIDATO[
        cifre_uni$LISTA == ammesse_pluri$LISTA[i]
      ]
  )
  
  accettori <- accettori[order(
    cifre_ind$CIFRA_PERCENTUALE[accettori],
    cifre_ind$DATA_NASCITA[accettori],
    decreasing = c(TRUE, TRUE)
  )]
  
  seggi_spostati <- min(
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    length(accettori)
  )
  
  for (j in seggi_spostati) {
    ammesse_pluri$SEGGI[i] <- ammesse_pluri$SEGGI[i] - 1
    cifre_ind$ELETTO[accettori[j]] <- TRUE
    # cifre_ind$RIPESCATO[accettori[j]] <- TRUE
    
    cat(
      "Ho ripescato",
      as.character(cifre_ind$CANDIDATO[accettori[j]]),
      "\n"
    )
  }
}

# 4. Qualora al termine delle operazioni di cui al comma 3 residuino
# ancora seggi da assegnare alla lista, l'Ufficio centrale nazionale,
# previa apposita comunicazione dell'Ufficio centrale circoscrizionale,
# individua la circoscrizione in cui la lista abbia la maggiore parte
# decimale del quoziente non utilizzata e procede a sua volta ad
# apposita comunicazione all'Ufficio centrale circoscrizionale
# competente. L'Ufficio centrale circoscrizionale provvede
# all'assegnazione dei seggi ai sensi del comma 2. Qualora al termine
# delle operazioni di cui ai precedenti periodi residuino ancora seggi
# da assegnare alla lista, questi le sono attribuiti nelle altre
# circoscrizioni in cui la stessa lista abbia la maggiore parte
# decimale del quoziente gia' utilizzata, procedendo secondo l'ordine
# decrescente.
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