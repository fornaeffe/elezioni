subentro <- function(livello = "circ", uni = FALSE, coal = FALSE) {
  donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)
  for (i in donatori) {
    
    da_spostare <- ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i]
    
    cat(
      "Devo spostare ",
      da_spostare,
      " seggi della lista ",
      as.character(ammesse_pluri$LISTA[i]),
      " da ",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
      ".\n",
      "Considero i candidati ",
      ifelse(uni, "uninominali", "plurinominali"),
      " della stessa ",
      ifelse(coal, "coalizione", "lista"),
      " a livello ",
      switch (livello,
              pluri = "di collegio plurinominale",
              circ = "di circoscrizione",
              naz = "nazionale"
      ),
      ".\n",
      sep = ""
    )
    
    if (livello == "naz") {
      for (j in 1:da_spostare) {
        cerca_naz(i, FALSE, coal, uni)
      }
      da_spostare <- ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i]
      if (da_spostare == 0) next
      
      for (j in 1:da_spostare) {
        cerca_naz(i, TRUE, coal, uni)
      }
      next
    }
    
    if (uni) {
      for (j in 1:da_spostare) {
        cerca_accettori_uni(i, pluri = livello == "pluri")
      }
      next
    }
    for (j in 1:da_spostare) {
      cerca_accettori(i)
    }
  }
}

cerca_accettori <- function(
    i,
    circ = ammesse_pluri$CIRCOSCRIZIONE[i],
    coal = FALSE,
    pluri = FALSE
) {
  if (coal) {
    liste <- ammesse_pluri$LISTA %in% liste_naz$LISTA[
      liste_naz$COALIZIONE == liste_naz$COALIZIONE[
        liste_naz$LISTA == ammesse_pluri$LISTA[i]
      ]
    ]
  } else {
    liste <- ammesse_pluri$LISTA == ammesse_pluri$LISTA[i]
  }
  
  if (pluri) {
    ambito <- 
      ammesse_pluri$COLLEGIOPLURINOMINALE == 
      ammesse_pluri$COLLEGIOPLURINOMINALE[i]
  } else {
    ambito <- ammesse_pluri$CIRCOSCRIZIONE == circ
  }
  
  accettori <- which(
    ambito & liste & ammesse_pluri$ELETTI < ammesse_pluri$CANDIDATI
  )
  
  accettori <- accettori[order(
    # ammesse_pluri$SEGGIO_DA_DECIMALI[accettori],
    ammesse_pluri$DECIMALI_USATI[accettori],
    ammesse_pluri$DECIMALI[accettori],
    decreasing = c(FALSE, TRUE)
  )]
  
  if (length(accettori) == 0) return(FALSE)
  
  j <- accettori[1]
  
  ammesse_pluri$SEGGI[i] <<- ammesse_pluri$SEGGI[i] - 1
  ammesse_pluri$SEGGI[j] <<- ammesse_pluri$SEGGI[j] + 1
  
  ammesse_pluri$DECIMALI_USATI[j] <<- TRUE
  
  ammesse_pluri$ELETTI[c(i,j)] <<- 
    pmin(ammesse_pluri$SEGGI[c(i,j)], ammesse_pluri$CANDIDATI[c(i,j)])
  
  cat(
    "Ho spostato un seggio dalla lista",
    as.character(ammesse_pluri$LISTA[i]),
    "nel collegio",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    "alla lista",
    as.character(ammesse_pluri$LISTA[j]),
    "nel collegio",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[j]),
    "\n"
  )
  
  return(TRUE)
}

cerca_accettori_uni <- function(
    i,
    circ = ammesse_pluri$CIRCOSCRIZIONE[i],
    pluri = FALSE
){
  if (pluri) {
    ambito <- 
      candidati_uni$COLLEGIOPLURINOMINALE == 
      ammesse_pluri$COLLEGIOPLURINOMINALE[i]
  } else {
    ambito <- candidati_uni$CIRCOSCRIZIONE == circ
  }
  
  accettori <- which(
    ambito &
      candidati_uni$ELETTO == FALSE &
      candidati_uni$CANDIDATO %in% liste_uni$CANDIDATO[
        liste_uni$LISTA == ammesse_pluri$LISTA[i]
      ]
  )
  
  accettori <- accettori[order(
    candidati_uni$CIFRA_PERCENTUALE[accettori],
    candidati_uni$DATA_NASCITA[accettori],
    decreasing = c(TRUE, TRUE)
  )]
  
  if (length(accettori) == 0) return(FALSE)
  
  j <- accettori[1]
  
  candidati_pluri <<- rbind(
    candidati_pluri,
    data.frame(
      LISTA = ammesse_pluri$LISTA[i],
      CIRCOSCRIZIONE = ammesse_pluri$CIRCOSCRIZIONE[i],
      COLLEGIOPLURINOMINALE = ammesse_pluri$COLLEGIOPLURINOMINALE[i],
      NUMERO = 1 + max(candidati_pluri$NUMERO[
        candidati_pluri$LISTA == ammesse_pluri$LISTA[i] &
          candidati_pluri$COLLEGIOPLURINOMINALE == 
          ammesse_pluri$COLLEGIOPLURINOMINALE[i]
      ]),
      CANDIDATO = candidati_uni$CANDIDATO[j]
    )
  )
  
  ammesse_pluri$CANDIDATI[i] <<- ammesse_pluri$CANDIDATI[i] + 1
  ammesse_pluri$ELETTI[i] <<- ammesse_pluri$ELETTI[i] + 1
  
  cat(
    "Ho ripescato",
    as.character(candidati_uni$CANDIDATO[accettori[j]]),
    "\n"
  )
  
  return(TRUE)
  
}

cerca_naz <- function(
    i,
    decimali_usati = FALSE,
    coal = FALSE,
    uni = FALSE
) {
  if (decimali_usati) {
    decimali <- ammesse_pluri$DECIMALI_USATI
    # decimali <- ammesse_pluri$SEGGIO_DA_DECIMALI
  } else {
    decimali <- !ammesse_pluri$DECIMALI_USATI
    # decimali <- !ammesse_pluri$SEGGIO_DA_DECIMALI
  }
  
  if (coal) {
    liste <- ammesse_pluri$LISTA %in% liste_naz$LISTA[
      liste_naz$COALIZIONE == liste_naz$COALIZIONE[
        liste_naz$LISTA == ammesse_pluri$LISTA[i]
      ]
    ]
  } else {
    liste <- ammesse_pluri$LISTA == ammesse_pluri$LISTA[i]
  }
  
  circ_accettrici <- aggregate(
    DECIMALI ~ CIRCOSCRIZIONE,
    ammesse_pluri[liste & decimali, ],
    max
  )
  
  circ_accettrici <- circ_accettrici[order(
    circ_accettrici$DECIMALI,
    decreasing = TRUE
  ),]
  
  for (k in seq_along(circ_accettrici$CIRCOSCRIZIONE)) {
    
    if (uni) {
      if (cerca_accettori_uni(i, circ_accettrici$CIRCOSCRIZIONE[k])) break
    } else {
      if (cerca_accettori(i, circ_accettrici$CIRCOSCRIZIONE[k], coal)) break
    }
    
  }
}