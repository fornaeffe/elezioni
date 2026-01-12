genera_candidati <- function(
    dati_politiche,
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
    pluri <- dati_politiche[[ramo]]$pluri
    uni <- dati_politiche[[ramo]]$uni
    candidati_pluri <- dati_politiche[[ramo]]$candidati_pluri
    candidati_uni <- dati_politiche[[ramo]]$candidati_uni
    
    #### Preparo il data frame dei candidati ####
    n_cand <- sum(pluri$MAX_CANDIDATI) + nrow(uni)
    candidati <- parametri_input$liste[
      rep(seq_len(.N), each = n_cand),
      .(LISTA, COALIZIONE)
    ]
    
    candidati[
      ,
      CANDIDATO_ID := paste(LISTA, seq_len(.N))
    ]
    
    
    # TODO testare questa parte
    candidati[
      ,
      CANDIDATO_ID := {
        lista <- LISTA[1L]
        ids <- candidati_pluri[
          LISTA == lista & !is.na(CANDIDATO_ID),
          CANDIDATO_ID
        ]
        replace(CANDIDATO_ID, seq_along(ids), ids)
      },
      by = .(LISTA)
    ]
    
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
          candidati,
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
          candidati,
          frazioni_pluricandidature
        ),
        simplify = FALSE
      ),
      idcol = "SIM"
    )
    
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
    candidati,
    liste
) {
  liste_by_coalizione <- split(
    liste,
    by = "COALIZIONE",
    keep.by = FALSE
  )
  
  
  candidati_non_scelti <- candidati[!candidati_uni, on = .(CANDIDATO_ID)]
  
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
    candidati,
    frazioni_pluricandidature
){
  candidati_non_scelti <- candidati[!candidati_pluri, on = .(CANDIDATO_ID)]
  
  candidati_pluri[
    is.na(CANDIDATO_ID),
    CANDIDATO_ID := {
      lista <- LISTA[1]
      candidati_lista <- candidati_non_scelti[LISTA == lista, CANDIDATO_ID]
      assegna_candidati_pluri(
        PLURI_COD,
        candidati_lista,
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
  
  lunghezze_scaglioni <- sort(
    Hare.Niemeyer(frazioni_pluricandidature, n),
    decreasing = TRUE
  )
  
  scaglioni <- list()
  
  scaglioni[[1]] <- sample(candidati_lista, lunghezze_scaglioni[1])
  
  for (i in 2:length(lunghezze_scaglioni)) {
    scaglioni[[i]] <- sample(scaglioni[[i - 1]], lunghezze_scaglioni[i])
  }
  
  candidati_da_assegnare <- sample(unlist(scaglioni))
  
  da_cambiare <- which(duplicated(paste(pluri_cod, candidati_da_assegnare)))
  
  candidati_da_assegnare[da_cambiare] <- paste(
    candidati_da_assegnare[da_cambiare], 
    "rimpiazzo", 
    seq_along(da_cambiare)
  )
  
  candidati_da_assegnare
}
