esegui_scrutini_politiche <- function(
    dati_collegi,
    parametri_input,
    dati_candidati,
    candidati,
    voti
){
  esegui_scrutini_politiche_ramo <- function(ramo) {
    uni_liste_sim <- data.table::copy(voti[[ramo]]$uni_liste_sim)
    candidati_uni_sim <- data.table::copy(voti[[ramo]]$candidati_uni_sim)
    candidati_pluri_sim <- data.table::copy(candidati[[ramo]]$candidati_pluri_sim)
    pluri <- data.table::copy(dati_collegi[[ramo]]$pluri)
    uni <- data.table::copy(dati_collegi[[ramo]]$uni)
    liste <- data.table::copy(parametri_input$liste)
    candidati_pluri <- data.table::copy(dati_candidati[[ramo]]$candidati_pluri)
    
    totale_seggi <- ifelse(ramo == "camera", 392, 196)
    
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
      CIRC_COD := i.CIRC_COD
    ]
    
    # Verifico quali liste rappresentanti di minoranze linguistiche si sono 
    # presentate solo in una regione
    if (ramo == "camera") {
      candidati_pluri[
        ,
        REG_COD := substr(CIRC_COD, 1, nchar(CIRC_COD) - 2)
      ]
    } else {
      candidati_pluri[
        ,
        REG_COD := CIRC_COD
      ]
    }
    
    liste_minoranza <- candidati_pluri[
      ,
      .(
        REGIONI = length(unique(REG_COD)),
        MINORANZA = sum(MINORANZA)
      ),
      by = .(LISTA)
    ][REGIONI == 1 & MINORANZA > 0, LISTA]
    
    liste[
      ,
      MINORANZA := LISTA %in% liste_minoranza
    ]
    
    # Torno ai data.frame
    uni_liste_sim_dt <- as.data.frame(uni_liste_sim[, .(
      CIRCOSCRIZIONE = CIRC_COD,
      COLLEGIOPLURINOMINALE = PLURI_COD,
      COLLEGIOUNINOMINALE = UNI_COD,
      CANDIDATO = CANDIDATO_ID,
      CAND_MINORANZA,
      LISTA,
      MINORANZA,
      VOTI_LISTA = VOTI_LISTA_SIM,
      SIM
    )])
    candidati_uni_sim_dt <- as.data.frame(candidati_uni_sim[, .(
      CIRCOSCRIZIONE = CIRC_COD,
      COLLEGIOPLURINOMINALE = PLURI_COD,
      COLLEGIOUNINOMINALE = UNI_COD,
      CANDIDATO = CANDIDATO_ID,
      DATA_NASCITA,
      VOTI_CANDIDATO,
      SIM
    )])
    candidati_pluri_sim_dt <- as.data.frame(candidati_pluri_sim[, .(
      CIRCOSCRIZIONE = CIRC_COD,
      COLLEGIOPLURINOMINALE = PLURI_COD,
      LISTA,
      NUMERO = NUMERO_CANDIDATO,
      CANDIDATO = CANDIDATO_ID,
      SIM
    )])
    pluri_dt <- as.data.frame(pluri[, .(
      CIRCOSCRIZIONE = CIRC_COD,
      COLLEGIOPLURINOMINALE = PLURI_COD,
      SEGGI = SEGGI_PLURI
    )])
    liste_dt <- as.data.frame(liste[, .(
      LISTA,
      COALIZIONE,
      MINORANZA
    )])
    
    uni_liste_sim_split <- split(uni_liste_sim_dt, uni_liste_sim$SIM)
    candidati_uni_sim_split <- split(candidati_uni_sim_dt, candidati_uni_sim$SIM)
    candidati_pluri_sim_split <- split(candidati_pluri_sim_dt, candidati_pluri_sim$SIM)
    
    # scrutini_sim_split <- mapply(
    #   scrutinio_politiche,
    #   uni_liste_sim_split,
    #   candidati_uni_sim_split,
    #   candidati_pluri_sim_split,
    #   MoreArgs = list(
    #     totali_pluri = pluri_dt,
    #     liste_naz = liste_dt,
    #     totale_seggi = totale_seggi,
    #     ramo = ramo
    #   ),
    #   SIMPLIFY = FALSE,
    #   USE.NAMES = FALSE # Per evitare che rbindlist restituisca una colonna SIM
    #   # character anziché integer
    # )
    
    cl <- parallel::makeCluster(parallel::detectCores() - 2)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    scrutini_sim_split <- parallel::clusterMap(
      cl,
      scrutinio_politiche,
      uni_liste_sim_split,
      candidati_uni_sim_split,
      candidati_pluri_sim_split,
      MoreArgs = list(
        totali_pluri = pluri_dt,
        liste_naz = liste_dt,
        totale_seggi = totale_seggi,
        ramo = ramo
      ),
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE # Per evitare che rbindlist restituisca una colonna SIM
      # character anziché integer
    )
    
    pluri_liste_sim_scrutinio <- data.table::rbindlist(
      lapply(scrutini_sim_split, function(x) x$liste_pluri),
      idcol = "SIM"
    )
    candidati_uni_sim_scrutinio <- data.table::rbindlist(
      lapply(scrutini_sim_split, function(x) x$candidati_uni),
      idcol = "SIM"
    )
    candidati_pluri_sim_scrutinio <- data.table::rbindlist(
      lapply(scrutini_sim_split, function(x) x$candidati_pluri),
      idcol = "SIM"
    )
    
    # Standardizzo i nomi
    data.table::setnames(
      pluri_liste_sim_scrutinio,
      c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE"
      ),
      c(
        "CIRC_COD",
        "PLURI_COD"
      )
    )
    data.table::setnames(
      candidati_uni_sim_scrutinio,
      c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "COLLEGIOUNINOMINALE",
        "CANDIDATO"
      ),
      c(
        "CIRC_COD",
        "PLURI_COD",
        "UNI_COD",
        "CANDIDATO_ID"
      )
    )
    data.table::setnames(
      candidati_pluri_sim_scrutinio,
      c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "CANDIDATO",
        "NUMERO"
      ),
      c(
        "CIRC_COD",
        "PLURI_COD",
        "CANDIDATO_ID",
        "NUMERO_CANDIDATO"
      )
    )
    
    
    return(
      list(
        pluri_liste_sim = pluri_liste_sim_scrutinio,
        candidati_uni_sim = candidati_uni_sim_scrutinio,
        candidati_pluri_sim_ = candidati_pluri_sim_scrutinio
      )
    )
  }
  
  camera <- esegui_scrutini_politiche_ramo("camera")
  senato <- esegui_scrutini_politiche_ramo("senato")
  
  return(list(camera = camera, senato = senato))
}