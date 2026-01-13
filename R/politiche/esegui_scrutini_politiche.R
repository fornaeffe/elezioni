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
    liste <- data.table::copy(parametri_input$liste)
    candidati_pluri <- data.table::copy(dati_candidati[[ramo]]$candidati_pluri)
    
    totale_seggi <- ifelse(ramo == "camera", 392, 196)
    
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
      MINORANZA == TRUE,
      .(REGIONI = length(unique(REG_COD))),
      by = .(LISTA)
    ][REGIONI == 1, LISTA]
    
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
    
    scrutini_sim_split <- mapply(
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
    
    # Aggiungo colonne informative
    pluri_liste_sim <- uni_liste_sim[
      ,
      .(
        VOTI_LISTA = sum(VOTI_LISTA_SIM)
      ),
      by = .(
        SIM,
        CIRC_COD,
        PLURI_COD,
        COALIZIONE,
        LISTA
      )
    ][
      pluri_liste_sim_scrutinio[, -c("CIRCOSCRIZIONE")],
      on = .(SIM, PLURI_COD = COLLEGIOPLURINOMINALE, LISTA)
    ][
      pluri,
      on = .(PLURI_COD),
      `:=`(
        CIRC_DEN = i.CIRC_DEN,
        PLURI_DEN = i.PLURI_DEN,
        SEGGI_PLURI = i.SEGGI_PLURI,
        MAX_CANDIDATI = i.MAX_CANDIDATI
      )
    ]
    
    # Calcolo la percentuale sui voti validi per ogni lista in ogni
    # collegio plurinominale
    pluri_liste_sim[
      ,
      PERCENTUALE := formattable::percent(VOTI_LISTA / sum(VOTI_LISTA), 2),
      by = .(SIM, PLURI_COD)
    ]
    
    # Raccolgo le info per ogni lista a livello nazionale
    liste_sim <- pluri_liste_sim[
      ,
      .(
        VOTI_LISTA = sum(VOTI_LISTA),
        ELETTI = sum(ELETTI),
        SEGGI_PRE_SUBENTRI = sum(SEGGI_PRE_SUBENTRI)
      ),
      by = .(
        SIM,
        COALIZIONE,
        LISTA
      )
    ]
    
    # Calcolo le percentuali nazionali
    liste_sim[
      ,
      PERCENTUALE := formattable::percent(VOTI_LISTA / sum(VOTI_LISTA), 2),
      by = .(SIM)
    ]
    
    # Costruisco il data.table candidati_uni_sim
    candidati_uni_sim[
      dati_collegi[[ramo]]$uni,
      on = .(UNI_COD),
      `:=`(
        CIRC_DEN = i.CIRC_DEN,
        PLURI_DEN = i.PLURI_DEN,
        UNI_DEN = i.UNI_DEN
      )
    ][
      candidati_uni_sim_scrutinio,
      on = .(
        SIM,
        UNI_COD = COLLEGIOUNINOMINALE,
        CANDIDATO_ID = CANDIDATO
      ),
      ELETTO := i.ELETTO
    ]
    
    
    return(
      list(
        pluri_liste_sim = pluri_liste_sim,
        candidati_uni_sim = candidati_uni_sim,
        candidati_pluri_sim = candidati_pluri_sim
      )
    )
    
    # DEBUG:
    # sim <- 10
    # liste_uni <- uni_liste_sim_split[[sim]]
    # candidati_uni <- candidati_uni_sim_split[[sim]]
    # candidati_pluri <- candidati_pluri_sim_split[[sim]]
    # totali_pluri <- pluri
    # liste_naz <- liste
    # 
    # scrutinio <- scrutinio_politiche(
    #   uni_liste_sim_split[[1]],
    #   candidati_uni_sim_split[[1]],
    #   candidati_pluri_sim_split[[1]],
    #   pluri,
    #   liste,
    #   totale_seggi,
    #   ramo
    # )
    
     
  }
}