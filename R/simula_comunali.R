simula_comunali <- function(
    comune,
    scenario,
    data_elezione
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData", filtro = list(COMUNE = comune))
  
  # Stabilisco il numero di consiglieri
  num_consiglieri <- numero_consiglieri(dati$pop_legale[,POPOLAZIONE])
  
  # Simulo i voti
  dati_simulati <- genera_voti(dati, scenario, data_elezione)
  
  # Aggiungo la colonna coalizione al data.table delle liste
  liste_sim <- dati_simulati$comuni_liste_sim[
    dati_simulati$liste[, .(LISTA, CANDIDATO_SINDACO = COALIZIONE)],
    on = .(LISTA)
  ]
  
  liste_sim <- liste_sim[
    LISTA != "astensione"
  ]
  
  # Creo la tabella dei candidati sindaci
  candidati_sindaci_sim <- liste_sim[
    ,
    .(
      VOTI_SINDACO = sum(VOTI_LISTA_SIM),
      VOTI_BALLOTTAGGIO = sum(VOTI_LISTA_SIM)
    ),
    by = .(SIM, CANDIDATO_SINDACO)
  ]
  
  candidati_sindaci_sim[
    ,
    DATA_DI_NASCITA := sample(seq(as.Date('1940/01/01'), as.Date('2000/01/01'), by="day"), .N, replace = TRUE)
  ]
  
  # Separo i data.table
  liste_split <- split(
    liste_sim[
      ,
      .(SIM, LISTA, CANDIDATO_SINDACO, VOTI_LISTA = VOTI_LISTA_SIM)
    ], 
    by = "SIM",
    keep.by = FALSE
  )
  
  candidati_sindaci_split <- split(
    candidati_sindaci_sim, 
    by = "SIM",
    keep.by = FALSE
  )
  
  # Da commentare:
  liste <- liste_split[[1]]
  candidati_sindaci <- candidati_sindaci_split[[1]]
  pop_legale <- dati$pop_legale[COMUNE == comune, POPOLAZIONE]
  
  cl <- parallel::makeCluster(parallel::detectCores())
  
  parallel::clusterEvalQ(cl, library(data.table))
  
  scrutinio <- parallel::clusterMap(
    cl,
    scrutinio_comunali,
    liste = liste_split,
    candidati_sindaci = candidati_sindaci_split,
    MoreArgs = list(
      pop_legale = dati$pop_legale[COMUNE == comune, POPOLAZIONE],
      num_consiglieri = num_consiglieri
    ),
  )
  
  parallel::stopCluster(cl)
  
}