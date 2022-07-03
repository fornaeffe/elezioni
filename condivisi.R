subentro <- function(livello = "circ", uni = FALSE, coal = FALSE) {
  donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)
  for (i in donatori) {
    cat(
      "Devo spostare ",
      ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
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
      cerca_naz(i, FALSE, coal, uni)
      if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) next
      
      cerca_naz(i, TRUE, coal, uni)
      next
    }
    
    if (uni) {
      cerca_accettori_uni(i, pluri = livello == "pluri")
      next
    }
    
    cerca_accettori(i)
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
    ammesse_pluri$DECIMALI_USATI[accettori],
    ammesse_pluri$DECIMALI[accettori],
    decreasing = c(FALSE, TRUE)
  )]
  
  for (j in accettori) {
    
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
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
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
  
  for (j in accettori) {
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
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
  
}

cerca_naz <- function(
    i,
    decimali_usati = FALSE,
    coal = FALSE,
    uni = FALSE
) {
  if (decimali_usati) {
    decimali <- ammesse_pluri$DECIMALI_USATI
  } else {
    decimali <- !ammesse_pluri$DECIMALI_USATI
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
      cerca_accettori_uni(i, circ_accettrici$CIRCOSCRIZIONE[k])
    } else {
      cerca_accettori(i, circ_accettrici$CIRCOSCRIZIONE[k], coal)
    }
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}