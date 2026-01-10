

carica_candidati <- function(dati_politiche, scenario, parametri_input) {
  
  carica_candidati_ramo <- function(ramo) {
    circ <- dati_politiche[[ramo]]$circ
    pluri <- dati_politiche[[ramo]]$pluri
    uni <- dati_politiche[[ramo]]$uni
    
    candidati_uni_input <- data.table::as.data.table(
      readxl::read_xlsx(
        scenario, 
        paste0(ramo, "_candidati_uni")
      )
    )
    candidati_pluri_input <- data.table::as.data.table(
      readxl::read_xlsx(
        scenario, 
        paste0(ramo, "_candidati_pluri")
      )
    )
    
    candidati_uni <- data.table::CJ(
      COALIZIONE = parametri_input$coalizioni$COALIZIONE, 
      UNI_COD = uni$UNI_COD
    )[
      uni,
      on = .(UNI_COD)
    ]
    
    candidati_uni <- data.table::rbindlist(
      list(
        candidati_uni[
          candidati_uni_input[CIRC_DEN == "*", !c("CIRC_DEN", "PLURI_DEN", "UNI_DEN")],
          on = .(COALIZIONE)
        ],
        candidati_uni[
          candidati_uni_input[PLURI_DEN == "*", !c("PLURI_DEN", "UNI_DEN")],
          on = .(COALIZIONE, CIRC_DEN)
        ],
        candidati_uni[
          candidati_uni_input[UNI_DEN == "*", !c("UNI_DEN")],
          on = .(COALIZIONE, CIRC_DEN, PLURI_DEN)
        ],
        candidati_uni[
          candidati_uni_input,
          on = .(COALIZIONE, CIRC_DEN, PLURI_DEN, UNI_DEN),
          nomatch = NULL
        ]
      )
    )
    
    liste_pluri <- data.table::CJ(
      LISTA = parametri_input$liste$LISTA, 
      PLURI_COD = pluri$PLURI_COD
    )[
      pluri[, .(
          CIRC_COD,
          CIRC_DEN,
          PLURI_COD,
          PLURI_DEN,
          MAX_CANDIDATI
        )
      ],
      on = .(PLURI_COD)
    ]
    
    candidati_pluri <- liste_pluri[
      rep(seq_len(.N), MAX_CANDIDATI)
    ][
      , NUMERO_CANDIDATO := sequence(liste_pluri$MAX_CANDIDATI)
    ]
    
    candidati_pluri <- data.table::rbindlist(
      list(
        candidati_pluri[
          candidati_pluri_input[CIRC_DEN == "*", !c("CIRC_DEN", "PLURI_DEN", "NUMERO_CANDIDATO")],
          on = .(LISTA),
          nomatch = NULL
        ],
        candidati_pluri[
          candidati_pluri_input[PLURI_DEN == "*", !c("PLURI_DEN", "NUMERO_CANDIDATO")],
          on = .(LISTA, CIRC_DEN),
          nomatch = NULL
        ],
        candidati_pluri[
          candidati_pluri_input[NUMERO_CANDIDATO == "*", !c("NUMERO_CANDIDATO")],
          on = .(LISTA, CIRC_DEN, PLURI_DEN),
          nomatch = NULL
        ],
        candidati_pluri[
          candidati_pluri_input,
          on = .(LISTA, CIRC_DEN, PLURI_DEN, NUMERO_CANDIDATO),
          nomatch = NULL
        ]
      )
    )
    
    return(
      list(
        candidati_uni = candidati_uni,
        candidati_pluri = candidati_pluri
      )
    )
    
  }
  
  camera <- c(dati_politiche$camera, carica_candidati_ramo("camera"))
  senato <- c(dati_politiche$senato, carica_candidati_ramo("senato"))
  
  return(list(camera = camera, senato = senato))
}