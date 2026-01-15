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
    
    candidati_pluri <- data.table::copy(dati_candidati[[ramo]]$candidati_pluri)
    candidati_uni <- data.table::copy(dati_candidati[[ramo]]$candidati_uni)
    liste <- parametri_input$liste[LISTA != "astensione"]
    
    # normalizziamo le percentuali per coalizione
    liste[, PERC_NORM := PERCENTUALE / sum(PERCENTUALE), by = COALIZIONE]
    
    # output preallocati come lista
    out_uni   <- vector("list", simulazioni)
    out_pluri <- vector("list", simulazioni)
    
    for (sim in seq_len(simulazioni)) {
      
      ## =========================
      ## UNINOMINALI
      ## =========================
      uni <- copy(candidati_uni)
      
      na_uni <- which(is.na(uni$CANDIDATO_ID))
      n_na_uni <- length(na_uni)
      
      if (n_na_uni > 0) {
        uni[na_uni,
            CANDIDATO_ID := paste0("UNI_", sim, "_", seq_len(.N))]
      }
      
      # selezione candidati uni → pluri
      n_uni_pluri <- floor(n_na_uni * frazione_uni_in_pluri)
      idx_uni_pluri <- if (n_uni_pluri > 0)
        sample(na_uni, n_uni_pluri)
      else integer()
      
      uni_pluri <- uni[idx_uni_pluri]
      
      ## =========================
      ## ASSEGNAZIONE UNINOMINALI ALLE LISTE
      ## =========================
      uni_pluri_liste <- uni_pluri[
        liste,
        on = .(COALIZIONE),
        allow.cartesian = TRUE
      ][
        , .SD[sample(.N, size = round(.N * PERC_NORM[1]))],
        by = .(COALIZIONE, LISTA)
      ]
      
      ## =========================
      ## PLURINOMINALI
      ## =========================
      pluri <- copy(candidati_pluri)
      
      na_pluri <- pluri[is.na(CANDIDATO_ID)]
      pluri_filled <- vector("list", length = length(unique(pluri$LISTA)))
      i <- 1
      
      for (lista in unique(pluri$LISTA)) {
        
        posti_lista <- na_pluri[LISTA == lista]
        n_posti <- nrow(posti_lista)
        if (n_posti == 0) next
        
        # distribuzione nelle 5 frazioni
        n_fraz <- Hare.Niemeyer(frazioni_pluricandidature, n_posti)
        
        # candidati disponibili
        uni_disp <- uni_pluri_liste[LISTA == lista, CANDIDATO_ID]
        
        candidati_lista <- character()
        
        # prima frazione
        n1 <- n_fraz[1]
        use_uni <- min(length(uni_disp), n1)
        
        if (use_uni > 0) {
          candidati_lista <- sample(uni_disp, use_uni)
        }
        
        if (use_uni < n1) {
          nuovi <- paste0("PLURI_", sim, "_", lista, "_",
                          seq_len(n1 - use_uni))
          candidati_lista <- c(candidati_lista, nuovi)
        }
        
        # frazioni successive
        prev <- candidati_lista
        if (length(prev) == 0) {
          prev <- paste0("PLURI_", sim, "_", lista, "_base")
        }
        
        for (f in 2:5) {
          if (n_fraz[f] == 0) next
          prev <- sample(prev, n_fraz[f], replace = length(prev) < n_fraz[f])
          candidati_lista <- c(candidati_lista, prev)
        }
        
        posti_lista[, CANDIDATO_ID := candidati_lista[seq_len(.N)]]
        pluri_filled[[i]] <- posti_lista
        i <- i + 1
      }
      
      pluri <- rbindlist(list(
        pluri[!is.na(CANDIDATO_ID)],
        rbindlist(pluri_filled, use.names = TRUE)
      ))
      
      ## =========================
      ## OUTPUT
      ## =========================
      out_uni[[sim]] <- uni[
        , .(SIM = sim,
            COALIZIONE,
            UNI_COD,
            LISTA_MINORANZA,
            CANDIDATO_ID,
            DATA_NASCITA)
      ]
      
      out_pluri[[sim]] <- pluri[
        , .(SIM = sim,
            LISTA,
            PLURI_COD,
            NUMERO_CANDIDATO,
            MINORANZA,
            CANDIDATO_ID,
            DATA_NASCITA)
      ]
    }
    
    candidati_uni_sim   <- rbindlist(out_uni)
    candidati_pluri_sim <- rbindlist(out_pluri)
    
    candidati_uni_sim[
      is.na(DATA_NASCITA),
      DATA_NASCITA := as.POSIXct("2000-01-01")
    ]
    candidati_pluri_sim[
      is.na(DATA_NASCITA),
      DATA_NASCITA := as.POSIXct("2000-01-01")
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