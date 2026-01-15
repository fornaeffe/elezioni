genera_candidati <- function(
    dati_candidati,
    dati_collegi,
    parametri_input,
    frazione_uni_in_pluri = 0,
    frazioni_pluricandidature = c(1,0,0,0,0),
    simulazioni = 1000
) {
  
  if (!(sum(frazioni_pluricandidature) == 1)) stop(
    "La somma di frazioni_pluricandidature è ", sum(frazioni_pluricandidature)
  )
  
  if (
    !all.equal(sort(
      frazioni_pluricandidature, decreasing = TRUE), 
      frazioni_pluricandidature
    )
  ) stop("frazioni_pluricandidature non è decrescente")
  
  genera_candidati_ramo <- function(
    ramo
  ) {
    message("Genero i candidati per ", ifelse(ramo == "camera", "la Camera", "il Senato"))
    
    pluri <- data.table::copy(dati_collegi[[ramo]]$pluri)
    uni <- data.table::copy(dati_collegi[[ramo]]$uni)
    candidati_pluri <- data.table::copy(dati_candidati[[ramo]]$candidati_pluri)
    candidati_uni <- data.table::copy(dati_candidati[[ramo]]$candidati_uni)
    
    #### Preparo il data frame dei candidati ####
    n_cand <- sum(pluri$MAX_CANDIDATI) + nrow(uni)
    candidati <- parametri_input$liste[
      LISTA != "astensione"
    ][
      rep(seq_len(.N), each = n_cand),
      .(LISTA, COALIZIONE)
    ]
    
    candidati[
      ,
      `:=`(
        CANDIDATO_ID = paste(LISTA, seq_len(.N))
      )
    ]
    
    liste_by_coalizione <- split(
      parametri_input$liste,
      by = "COALIZIONE",
      keep.by = FALSE
    )
    
    
    risultato <- replicate(
      simulazioni,
      sorteggio_candidati(
        candidati_uni = candidati_uni[,.(
          COALIZIONE,
          UNI_COD,
          LISTA_MINORANZA,
          CANDIDATO_ID,
          DATA_NASCITA
        )],
        candidati_pluri[,.(
          LISTA,
          PLURI_COD,
          NUMERO_CANDIDATO,
          MINORANZA,
          CANDIDATO_ID,
          DATA_NASCITA
        )],
        candidati,
        liste_by_coalizione,
        frazione_uni_in_pluri,
        frazioni_pluricandidature
      ),
      simplify = FALSE
    )
    
    candidati_uni_sim <- rbindlist(
      lapply(risultato, function(x) x$candidati_uni),
      idcol = "SIM"
    )
    candidati_pluri_sim <- rbindlist(
      lapply(risultato, function(x) x$candidati_pluri),
      idcol = "SIM"
    )
    
    
    # Aggiungo colonne informative
    
    candidati_uni_sim[
      uni,
      on = .(UNI_COD),
      `:=`(
        PLURI_COD = i.PLURI_COD,
        CIRC_COD = i.CIRC_COD
      )
    ]
    
    candidati_pluri_sim[
      pluri,
      on = .(PLURI_COD),
      `:=`(
        CIRC_COD = i.CIRC_COD
      )
    ]
    
    return(
      list(
        candidati_uni_sim = candidati_uni_sim,
        candidati_pluri_sim = candidati_pluri_sim
      )
    )
  }
  
  camera <- genera_candidati_ramo("camera")
  senato <- genera_candidati_ramo("senato")
  
  return(
    list(
      camera = camera,
      senato = senato
    )
  )
}

sorteggio_candidati <- function(
    candidati_uni,
    candidati_pluri,
    candidati,
    liste_by_coalizione,
    frazione_uni_in_pluri,
    frazioni_pluricandidature
){
  candidati[
    ,
    DATA_NASCITA := as.POSIXct(runif(
      .N,
      as.POSIXct("1950-01-01"), 
      as.POSIXct("2000-12-31"))
    )
  ]
  
  # Per ogni candidato uninominale, sorteggio a quale lista "appartiene" 
  # all'interno della coalizione
  candidati_uni[
    is.na(CANDIDATO_ID),
    lista_candidato := {
      blocco <- liste_by_coalizione[[COALIZIONE[1]]]
      sample(
        blocco$LISTA,
        size = .N,
        replace = TRUE,
        prob = blocco$PERCENTUALE
      )
    },
    by = .(COALIZIONE)
  ]
  
  # Sceglo il nome del candidato all'interno della lista di appartenenza
  # Qui non è necessario sorteggiare, perché ho già assegnato casualmente le date di
  # nascita
  candidati_uni[
    is.na(CANDIDATO_ID),
    CANDIDATO_ID := 
      candidati[LISTA == lista_candidato, CANDIDATO_ID][1:.N],
    by = lista_candidato
  ]
  
  candidati_uni$lista_candidato <- NULL
  
  # Sorteggio i candidati uninominali che si presenteranno anche al plurinominale
  candidati[
    ,
    UNI := CANDIDATO_ID %in% candidati_uni$CANDIDATO_ID
  ]
  
  candidati_uni_anche_pluri <- candidati[
    UNI == TRUE,
    .SD[sample.int(.N, size = round(.N * frazione_uni_in_pluri))],
    by = LISTA
  ]
  
  candidati_non_uni <- candidati[UNI == FALSE]
  
  # split per lista
  candidati_non_uni_per_lista <- split(
    candidati_non_uni$CANDIDATO_ID,
    candidati_non_uni$LISTA
  )
  candidati_uni_anche_pluri_per_lista <- split(
    candidati_uni_anche_pluri$CANDIDATO_ID,
    candidati_uni_anche_pluri$LISTA
  )
  
  # Sorteggio i candidati nei collegi plurinominali
  candidati_pluri[
    is.na(CANDIDATO_ID),
    CANDIDATO_ID := {
      assegna_candidati_pluri(
        PLURI_COD,
        candidati_non_uni_per_lista[[LISTA[1L]]],
        candidati_uni_anche_pluri_per_lista[[LISTA[1L]]],
        frazioni_pluricandidature
      )
    },
    by = LISTA
  ]
  
  # Riapplico le date di nascita
  candidati_pluri[
    candidati,
    on = .(CANDIDATO_ID),
    DATA_NASCITA := i.DATA_NASCITA
  ]
  candidati_uni[
    candidati,
    on = .(CANDIDATO_ID),
    DATA_NASCITA := i.DATA_NASCITA
  ]
  
  return(
    list(
      candidati_uni = candidati_uni,
      candidati_pluri = candidati_pluri
    )
  )
  
}

assegna_candidati_pluri <- function(
    pluri_cod,
    candidati_lista,
    candidati_uni_anche_pluri_lista,
    frazioni_pluricandidature
){
  n <- length(pluri_cod)
  
  lunghezze <- sort(
    Hare.Niemeyer(frazioni_pluricandidature, n),
    decreasing = TRUE
  )
  
  # i candidati da piazzare sono i candidati uninominali che si candidano
  # anche nei plurinominali più abbastanza candidati nuovi da riempire tutte
  # le prime candidature
  base <- c(candidati_uni_anche_pluri_lista, candidati_lista)[1:lunghezze[1]]
  
  # un solo campionamento
  base <- sample(base)
  
  
  
  # indici cumulativi
  idx <- unlist(
    mapply(
      seq_len,
      lunghezze,
      SIMPLIFY = FALSE
    ),
    use.names = FALSE
  )
  
  candidati_da_assegnare <- sample(base[idx])
  
  # Se lo stesso candidato è stato assegnato più volte nello stesso collegio
  # lo rimpiazzo
  dup <- duplicated(data.table::data.table(
    pluri_cod = pluri_cod,
    cand = candidati_da_assegnare
  ))
  
  if (any(dup)) {
    candidati_da_assegnare[dup] <- paste(
      candidati_da_assegnare[dup],
      "rimpiazzo",
      seq_len(sum(dup))
    )
  }
  
  candidati_da_assegnare
}