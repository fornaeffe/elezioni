#' Simulazione delle elezioni comunali
#'
#' @param comune string, nome del comune
#' @param scenario string, percorso per il file Excel di scenario
#' @param data_elezione POSIXct, data della futura elezione da simulare
#'
#' @returns una lista di data.table:
#'
#' @export
#'
#' @examples
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
    dati_simulati$liste[, .(LISTA, COALIZIONE)],
    on = .(LISTA)
  ]
  
  liste_sim <- liste_sim[
    LISTA != "astensione"
  ]
  
  # Creo la tabella dei candidati sindaci
  coalizioni_sim <- liste_sim[
    ,
    .(
      VOTI_SINDACO = sum(VOTI_LISTA_SIM),
      VOTI_BALLOTTAGGIO = sum(VOTI_LISTA_SIM)
    ),
    by = .(SIM, COALIZIONE)
  ]
  
  coalizioni_sim[
    ,
    DATA_DI_NASCITA := sample(seq(as.Date('1940/01/01'), as.Date('2000/01/01'), by="day"), .N, replace = TRUE)
  ]
  
  # Separo i data.table
  liste_split <- split(
    liste_sim[
      ,
      .(SIM, LISTA, COALIZIONE, VOTI_LISTA = VOTI_LISTA_SIM)
    ], 
    by = "SIM",
    keep.by = FALSE
  )
  
  coalizioni_split <- split(
    coalizioni_sim, 
    by = "SIM",
    keep.by = FALSE
  )
  
  input_scrutinio <- Map(
    function(liste, coalizioni) {
      list(
        liste = liste,
        coalizioni = coalizioni
      )
    },
    liste_split,
    coalizioni_split
  )
  
  # Da commentare:
  # liste <- liste_split[[1]]
  # coalizioni <- coalizioni_split[[1]]
  pop_legale <- dati$pop_legale[COMUNE == comune, POPOLAZIONE]
  
  
  
  cl <- parallel::makeCluster(parallel::detectCores())
  
  parallel::clusterEvalQ(cl, {
    library(data.table)
  })
  
  parallel::clusterExport(
    cl,
    c("scrutinio_comunali", "scrutinio_worker"),
    envir = environment()
  )
  
  scrutinio <- parallel::parLapply(
    cl,
    input_scrutinio,
    scrutinio_worker,
    pop_legale = pop_legale,
    num_consiglieri = num_consiglieri
  )
  
  parallel::stopCluster(cl)
  
  liste_sim <- rbindlist(lapply(
    scrutinio,
    function(x) x$liste
  ), idcol = "SIM")
  
  coalizioni_sim <- rbindlist(lapply(
    scrutinio,
    function(x) x$coalizioni
  ), idcol = "SIM")
  
  liste_sim[
    dati_simulati$liste[,.(LISTA, COLORE)],
    on = .(LISTA)
  ]
  
  return(
    list(
      liste_sim = liste_sim,
      coalizioni_sim = coalizioni_sim,
      liste = dati_simulati$liste
    )
  )
}