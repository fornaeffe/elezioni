#' Simulazione delle elezioni comunali
#'
#' @param comune string, nome del comune
#' @param scenario string, percorso per il file Excel di scenario
#' @param data_elezione POSIXct, data della futura elezione da simulare
#' @param simulaizoni numero di simulazioni da eseguire
#'
#' @returns una lista di data.table:
#' \describe{
#'  \item{liste_sim}{data.table con i risultati della simulazione per ciascuna 
#'    lista e ciascuna simulazione}
#'  \item{coalizioni_sim}{data.table con i risultati della simulazione per 
#'    ciascun candidato sindaco e ciascuna simulazione}
#'  \item{liste}{data.table con i parametri di input per ciascuna lista}
#' }
#'
#' @export
#' 
#' @details
#' Il data.table \code{liste_sim} contiene queste colonne:
#' \describe{
#'  \item{SIM}{numero della simulazione}
#'  \item{LISTA}{nome della lista}
#'  \item{COALIZIONE}{cognome e nome del candidato sindaco, eventualmente 
#'    disambiguato con la data di nascita}
#'  \item{VOTI_LISTA}{numero di voti simulati ricevuti dalla lista}
#'  \item{PERCENTUALE}{rapporto tra voti ricevuti dalla lista e voti validi 
#'    totali alle liste}
#'  \item{SEGGI}{numero di seggi ottenuti dalla lista}
#' }
#' Il data.table \code{coalizioni_sim} contiene queste colonne:
#' \describe{
#'  \item{SIM}{numero della simulazione}
#'  \item{COALIZIONE}{cognome e nome del candidato sindaco, eventualmente 
#'    disambiguato con la data di nascita}
#'  \item{VOTI_SINDACO}{i voti simulati al candidato sindaco}
#'  \item{DATA_DI_NASCITA}{data di nascita del candidato sindaco}
#'  \item{VOTI_LISTA}{la somma dei voti ricevuti dalle liste collegate al
#'  candidato sindaco}
#'  \item{PERCENTUALE_SINDACO}{rapporto tra i voti a questo candidato sindaco
#'  e i voti validi totali ai candidati sindaci}
#'  \item{PERCENTUALE_LISTE}{rapporto tra i voti alle liste collegate al 
#'  candidato sindaco e i voti validi totali alle liste}
#'  \item{SINDACO}{\code{TRUE} se il candidato è eletto sindaco}
#'  \item{SEGGI}{numero di seggi assegnati al gruppo di liste, compreso quello
#'  eventualmente assegnato al candidato sindaco non vincente}
#'  \item{SEGGIO_CANDIDATO_SINDACO}{TRUE se uno dei seggi è riservato al 
#'  candidato sindaco non vincente}
#' }
#' Il data.table \code{liste} contiene queste colonne:
#' \describe{
#'  \item{LISTA}{nome della lista}
#'  \item{COALIZIONE}{cognome e nome del candidato sindaco, eventualmente 
#'    disambiguato con la data di nascita}
#'  \item{COLORE}{codice esadecimale del colore associato alla lista}
#'  \item{SIGMA_GLOBAL}{parametro che controlla la variabilità globale dei voti
#'  alla lista}
#'  \item{PERCENTUALE}{frazione degli elettori che hanno votato per questa lista
#'  alle ultime elezioni}
#'  \item{LOGIT_P}{il logit di \code{PERCENTUALE}. La simulazione parte da 
#'  questo valore per simulare i possibili risultati.}
#'  \item{DATA}{data dell'ultima elezione in POSIXct}
#'  \item{SIGMA_DELTA}{parametro che controlla la variabilità locale dei voti
#'  alla lista}
#' }
#' Attenzione: al momento non è implementata la simulazione dei voti al 
#' candidato sindaco e al ballottaggio, vengono semplicemente copiati i voti
#' alla lista anche nelle colonne \code{VOTI_SINDACO} e \code{VOTI_BALLOTTAGGIO}
#'
#' @examples
simula_comunali <- function(
    comune,
    scenario,
    data_elezione,
    simulazioni = 1000
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData", filtro = list(COMUNE = comune))
  
  # Stabilisco il numero di consiglieri
  num_consiglieri <- numero_consiglieri(dati$pop_legale[,POPOLAZIONE])
  
  # Simulo i voti
  dati_simulati <- genera_voti(dati, scenario, data_elezione, simulazioni)
  
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
  
  liste_sim <- data.table::rbindlist(lapply(
    scrutinio,
    function(x) x$liste
  ), idcol = "SIM")
  
  coalizioni_sim <- data.table::rbindlist(lapply(
    scrutinio,
    function(x) x$coalizioni
  ), idcol = "SIM")
  
  liste_sim <- dati_simulati$liste[,.(LISTA, COLORE)][
    liste_sim,
    on = .(LISTA)
  ]
  
  # Formattazione risultati
  liste_sim[
    ,
    PERCENTUALE := formattable::percent(PERCENTUALE, 2)
  ]
  
  coalizioni_sim[
    ,
    `:=`(
      PERCENTUALE_SINDACO = formattable::percent(PERCENTUALE_SINDACO, 2),
      PERCENTUALE_LISTE = formattable::percent(PERCENTUALE_LISTE, 2)
    )
  ]
  
  dati_simulati$liste[
    ,
    PERCENTUALE := formattable::percent(PERCENTUALE, 2)
  ]
  
  return(
    list(
      liste_sim = liste_sim,
      coalizioni_sim = coalizioni_sim,
      liste = dati_simulati$liste
    )
  )
}