genera_candidati <- function(
    dati_politiche,
    parametri_input,
    simulazioni = 1000
) {
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
    
    
    
  }
}

sorteggio_candidati <- function(
    candidati_uni,
    candidati,
    liste
) {
  liste_by_coalizione <- split(
    liste[, .(LISTA, PERCENTUALE, COALIZIONE)],
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
  
  
}