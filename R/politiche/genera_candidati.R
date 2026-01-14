# TODO: bypass in caso di assenza di pluricandidature
# TODO: regolare la porzione di candidati uninominali che sono candidati
# anche nei plurinominali

genera_candidati <- function(
    dati_candidati,
    dati_collegi,
    parametri_input,
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
    
    pluri <- dati_collegi[[ramo]]$pluri
    uni <- dati_collegi[[ramo]]$uni
    candidati_pluri <- dati_candidati[[ramo]]$candidati_pluri
    candidati_uni <- dati_candidati[[ramo]]$candidati_uni
    
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
        CANDIDATO_ID = paste(LISTA, seq_len(.N)),
        DATA_NASCITA = as.POSIXct(runif(.N, as.POSIXct("1950-01-01"), as.POSIXct("2000-12-31")))
      )
    ]
    
    candidati <- data.table::rbindlist(
      list(
        candidati,
        candidati_pluri[
          parametri_input$liste[, .(LISTA, COALIZIONE)],
          on = .(LISTA)
        ][
          !is.na(CANDIDATO_ID),
          .(LISTA, COALIZIONE, CANDIDATO_ID, DATA_NASCITA)
        ]
      )
    )
    
    
    candidati_non_scelti <- candidati[!candidati_uni, on = .(CANDIDATO_ID)]
    
    candidati_uni_sim <- data.table::rbindlist(
      replicate(
        simulazioni,
        sorteggio_candidati_uni(
          candidati_uni[,.(
            COALIZIONE,
            UNI_COD,
            LISTA_MINORANZA,
            CANDIDATO_ID
          )],
          candidati_non_scelti,
          parametri_input$liste[,c(
            "LISTA",
            "COALIZIONE",
            "PERCENTUALE"
          )]
        ),
        simplify = FALSE
      ),
      idcol = "SIM"
    )
    
    candidati_non_scelti <- candidati[!candidati_pluri, on = .(CANDIDATO_ID)]
    
    # pre-split per lista
    candidati_per_lista <- split(
      candidati_non_scelti$CANDIDATO_ID,
      candidati_non_scelti$LISTA
    )
    
    candidati_pluri_sim <- data.table::rbindlist(
      replicate(
        simulazioni,
        sorteggio_candidati_pluri(
          candidati_pluri[,.(
            LISTA,
            PLURI_COD,
            NUMERO_CANDIDATO,
            MINORANZA,
            CANDIDATO_ID
          )],
          candidati_per_lista,
          frazioni_pluricandidature
        ),
        simplify = FALSE
      ),
      idcol = "SIM"
    )
    
    # Aggiungo colonne informative
    candidati_uni_sim[
      candidati[, .(CANDIDATO_ID, DATA_NASCITA)],
      on = .(CANDIDATO_ID),
      DATA_NASCITA := i.DATA_NASCITA
    ]
    
    candidati_uni_sim[
      uni,
      on = .(UNI_COD),
      `:=`(
        PLURI_COD = i.PLURI_COD,
        CIRC_COD = i.CIRC_COD
      )
    ]
    
    candidati_pluri_sim[
      candidati[, .(CANDIDATO_ID, DATA_NASCITA)],
      on = .(CANDIDATO_ID),
      DATA_NASCITA := i.DATA_NASCITA
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

sorteggio_candidati_uni <- function(
    candidati_uni,
    candidati_non_scelti,
    liste
) {
  liste_by_coalizione <- split(
    liste,
    by = "COALIZIONE",
    keep.by = FALSE
  )
  
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
  
  candidati_uni[
    is.na(CANDIDATO_ID),
    CANDIDATO_ID := sample(
      candidati_non_scelti[LISTA == lista_candidato, CANDIDATO_ID],
      .N
    ),
    by = lista_candidato
  ]
  
  candidati_uni$lista_candidato <- NULL
  
  candidati_uni
  
}

sorteggio_candidati_pluri <- function(
    candidati_pluri,
    candidati_per_lista,
    frazioni_pluricandidature
){
  
  
  candidati_pluri[
    is.na(CANDIDATO_ID),
    CANDIDATO_ID := {
      assegna_candidati_pluri(
        PLURI_COD,
        candidati_per_lista[[LISTA[1L]]],
        frazioni_pluricandidature
      )
    },
    by = LISTA
  ]
  
  candidati_pluri
}

assegna_candidati_pluri <- function(
  pluri_cod,
  candidati_lista,
  frazioni_pluricandidature
){
  n <- length(pluri_cod)
  
  lunghezze <- sort(
    Hare.Niemeyer(frazioni_pluricandidature, n),
    decreasing = TRUE
  )
  
  # un solo campionamento
  base <- sample(candidati_lista, lunghezze[1])
  
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
  
  # duplicati
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
