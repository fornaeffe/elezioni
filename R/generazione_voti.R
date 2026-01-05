# 
# # TODO verificare se usando un unico data.table anziché parallel diventa più veloce
# 
# simulazione <- function(
#   sim, 
#   comuni_liste, 
#   liste, 
#   data_elezione,
#   colonna_localita = "CODICE_COMUNE"
# ) {
#   data.table::setDTthreads(1)
#   
#   # Copio le tabelle per evitare di modificare l'originale
#   comuni_liste_temp <- data.table::copy(comuni_liste)
#   liste_temp <- data.table::copy(liste)
#   
#   # Simulo i logit della percentuale
#   liste_temp[
#     ,
#     LOGIT_P_SIM := rnorm(
#       LOGIT_P,
#       LOGIT_P,
#       SIGMA_GLOBAL * unclass(data_elezione - DATA)^0.5
#     )
#   ]
#   
#   # Passo i valori simulati ai singoli comuni
#   comuni_liste_temp <- comuni_liste_temp[
#     liste_temp[, .(LISTA, LOGIT_P_SIM_GLOBAL = LOGIT_P_SIM)],
#     on = .(LISTA)
#   ]
#   
#   # Simulo il drift del delta
#   comuni_liste_temp[
#     ,
#     DELTA_SIM := rnorm(
#       DELTA,
#       DELTA,
#       SIGMA_DELTA * unclass(data_elezione - DATA)^0.5
#     )
#   ]
#   
#   # Calcolo il logit della percentuale a livello comunale
#   comuni_liste_temp[
#     ,
#     LOGIT_P_SIM := LOGIT_P_SIM_GLOBAL + DELTA_SIM
#   ]
#   
#   # Ritrasformo in percentuale
#   comuni_liste_temp[
#     ,
#     PERCENTUALE_SIM := plogis(LOGIT_P_SIM) / sum(plogis(LOGIT_P_SIM)),
#     by = colonna_localita
#   ]
#   
#   # Converto la percentuale in numero di voti
#   comuni_liste_temp[
#     ,
#     VOTI_LISTA_SIM := round(PERCENTUALE_SIM * ELETTORI)
#   ]
#   
#   # Restituisco il risultato
#   return(comuni_liste_temp)
#   
# }
# 
# genera_voti <- function(
#     dati,
#     comuni_liste,
#     liste,
#     data_elezione,
#     simulazioni = 1000,
#     colonna_localita = "CODICE_COMUNE"
# ){
#   
#   cat("\nSimulo i voti...\n")
#   
#   # Simulo i voti nella futura elezione
#   cl <- parallel::makeCluster(parallel::detectCores())
#   
#   parallel::clusterEvalQ(cl, library(data.table))
#   
#   comuni_liste_sim <- parallel::parLapply(
#     cl,
#     seq_len(simulazioni),
#     simulazione,
#     comuni_liste = comuni_liste,
#     liste = liste,
#     data_elezione = data_elezione,
#     colonna_localita = colonna_localita
#   )
#   
#   parallel::stopCluster(cl)
#   
#   # Trasformo la lista in un data.table
#   comuni_liste_sim <- data.table::rbindlist(comuni_liste_sim, idcol = "SIM")
#   
#   return(comuni_liste_sim)
# }

genera_voti <- function(
    comuni_liste,
    liste,
    data_elezione,
    simulazioni = 1000,
    colonna_localita = "CODICE_COMUNE"
) {
  
  cat("\nSimulo i voti...\n")
  
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


