
genera_voti <- function(
    comuni_liste,
    liste,
    data_elezione,
    simulazioni = 1000,
    colonna_localita = "CODICE_COMUNE"
) {
  
  message("\nSimulo i voti...\n")
  
  # Copie locali
  comuni_liste <- data.table::copy(comuni_liste)
  liste <- data.table::copy(liste)
  
  # ---- 1. Pre-computazioni temporali ----
  liste[
    ,
    DT := unclass(data_elezione - DATA)^0.5
  ]
  
  comuni_liste[
    ,
    DT := unclass(data_elezione - DATA)^0.5
  ]
  
  # ---- 2. Espando le simulazioni (ID SIM) ----
  comuni_liste_sim <- comuni_liste[
    rep(seq_len(.N), times = simulazioni)
  ]
  comuni_liste_sim[
    ,
    SIM := as.integer(rep(seq_len(simulazioni), each = .N / simulazioni))
  ]
  
  # ---- 3. Simulazione livello LISTA (globale) ----
  # una simulazione per LISTA × SIM
  liste_sim <- liste[
    ,
    .(
      SIM = seq_len(simulazioni),
      LOGIT_P_SIM_GLOBAL = rnorm(
        simulazioni,
        mean = LOGIT_P,
        sd   = SIGMA_GLOBAL * DT
      )
    ),
    by = LISTA
  ]
  
  # ---- 4. Join LISTA → COMUNI ----
  comuni_liste_sim <- comuni_liste_sim[
    liste_sim,
    on = .(LISTA, SIM)
  ]
  
  # ---- 5. Simulazione DELTA comunale ----
  comuni_liste_sim[
    ,
    DELTA_SIM := rnorm(
      .N,
      mean = DELTA,
      sd   = SIGMA_DELTA * DT
    )
  ]
  
  # ---- 6. Logit e percentuali ----
  comuni_liste_sim[
    ,
    LOGIT_P_SIM := LOGIT_P_SIM_GLOBAL + DELTA_SIM
  ]
  
  comuni_liste_sim[
    ,
    p := plogis(LOGIT_P_SIM)
  ]
  
  comuni_liste_sim[
    ,
    PERCENTUALE_SIM := p / sum(p),
    by = .(SIM, get(colonna_localita))
  ]
  
  # ---- 7. Conversione in voti ----
  comuni_liste_sim[
    ,
    VOTI_LISTA_SIM := round(PERCENTUALE_SIM * ELETTORI)
  ]
  
  # Pulizia colonne temporanee
  comuni_liste_sim[
    ,
    c("DT", "LOGIT_P_SIM_GLOBAL", "DELTA_SIM", "LOGIT_P_SIM", "p") := NULL
  ]
  
  data.table::setcolorder(comuni_liste_sim, c("SIM"))
  
  return(comuni_liste_sim)
}


