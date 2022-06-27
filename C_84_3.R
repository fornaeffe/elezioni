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

