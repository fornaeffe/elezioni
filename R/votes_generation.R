calcola_parametri_di_voto <- function(
    dati,
    scenario
){
  
  
  liste <- data.table::as.data.table(readxl::read_xlsx(scenario, "liste_future"))
  corrispondenza_liste <- data.table::as.data.table(readxl::read_xlsx(scenario, "flussi_previsti"))
  
  # TODO: validare lo scenario
  
  # FIX
  # TODO evitare che si generi il problema di una lista senza nome
  corrispondenza_liste[
    is.na(LISTA),
    LISTA := ""
  ]
  
  
  ##### Corrispondenza liste vecchie - liste nuove #####
  
  # TODO: uniformare i nomi
  
  # Converto i nomi
  data.table::setnames(
    corrispondenza_liste, 
    c("LISTA", "LISTA_FUTURA", "FRAZIONE"),
    c("LISTA_ORIGINALE", "LISTA", "FATTORE")
  )
  
  data.table::setnames(
    liste,
    "LISTA_FUTURA",
    "LISTA"
  )
  
  # Normalizzo
  data.table::setkey(corrispondenza_liste, ELEZIONE, LISTA_ORIGINALE, LISTA)
  corrispondenza_liste[
    , 
    FATTORE := FATTORE / sum(FATTORE), 
    by=.(ELEZIONE, LISTA_ORIGINALE)
  ]
  
  # Riassumo i dati per comune
  comuni_liste_elezioni <- dati$comuni_liste_elezioni
  data.table::setnames(comuni_liste_elezioni, "LISTA", "LISTA_ORIGINALE")
  
  # Individuo le liste corrispondenti, e calcolo i voti per le nuove liste
  comuni_liste_elezioni <- comuni_liste_elezioni[
    corrispondenza_liste,
    on = .(DATA, ELEZIONE, LISTA_ORIGINALE)
  ]
  
  comuni_liste_elezioni[, VOTI := VOTI * FATTORE]
  
  comuni_liste_elezioni <- comuni_liste_elezioni[
    ,
    .(VOTI = sum(VOTI)),
    keyby = .(CODICE_REGIONE, REGIONE, CODICE_PROVINCIA, PROVINCIA, CODICE_COMUNE, COMUNE, LISTA, DATA, ELEZIONE)
  ]
  
  # Calcolo gli elettori per ciascuna provincia e ciascuna elezione
  comuni_liste_elezioni[
    ,
    ELETTORI := sum(VOTI),
    by = .(CODICE_COMUNE, ELEZIONE)
  ]
  
  # Calcolo le percentuali per ciascuna provincia e ciascuna elezione
  comuni_liste_elezioni[, PERCENTUALE := VOTI / ELETTORI]
  
  # Ne calcolo il logit
  comuni_liste_elezioni[, LOGIT_P := qlogis(pmax(PERCENTUALE, 0.5 / ELETTORI))]
  
  # Calcolo i voti totali ricevuti da ogni lista in ogni elezione
  liste_elezioni <- comuni_liste_elezioni[
    ,
    .(VOTI = sum(VOTI)),
    keyby = .(LISTA, DATA, ELEZIONE)
  ]
  
  # Calcolo la percentuale
  liste_elezioni[
    ,
    PERCENTUALE := VOTI / sum(VOTI),
    by = ELEZIONE
  ]
  
  # Ne calcolo il logit
  liste_elezioni[, LOGIT_P := qlogis(PERCENTUALE)]
  
  # Per ogni lista, calcolo la deviazione standard della velocità di cambiamento
  # del logit della percentuale
  # Riporto, inoltre, il logit della percentuale e la data relativi all'ultima elezione
  data.table::setkey(liste_elezioni, DATA)
  
  liste <- liste[
    liste_elezioni[
      ,
      .(
        SIGMA_R = sd(diff(.SD$LOGIT_P) / unclass(diff(.SD$DATA))^0.5),
        LOGIT_P = tail(.SD$LOGIT_P, n=1),
        DATA = tail(.SD$DATA, n=1)
      ),
      by = .(LISTA)
    ],
    on = .(LISTA)
  ]
  
  # Calcolo la differenza tra il logit provinciale e il logit regionale
  comuni_liste_elezioni[
    liste_elezioni[, .(LISTA, ELEZIONE, LOGIT_P)],
    on = .(LISTA, ELEZIONE),
    DELTA := LOGIT_P  - i.LOGIT_P
  ]
  
  # Per ogni lista, calcolo la deviazione standard della velocità di cambiamento
  # della differenza tra il logit provinciale e il logit regionale
  liste <- liste[
    comuni_liste_elezioni[
      ,
      .(
        SIGMA_P = sd(
          .SD[
            ,
            .(DELTA_DRIFT = diff(DELTA) / unclass(diff(DATA))^0.5),
            by = CODICE_COMUNE
          ]$DELTA_DRIFT
        )
      ),
      by = LISTA
    ],
    on = .(LISTA)
  ]
  
  # Rimpiazzo i sigma mancanti con la media delle altre liste
  
  liste$SIGMA_R[is.na(liste$SIGMA_R)] <- mean(liste$SIGMA_R, na.rm = TRUE)
  liste$SIGMA_P[is.na(liste$SIGMA_P)] <- mean(liste$SIGMA_P, na.rm = TRUE)
  
  # Per ciascuna lista seleziono solo l'ultima elezione
  data.table::setorder(comuni_liste_elezioni, CODICE_COMUNE, LISTA, -DATA)
  comuni_liste <- comuni_liste_elezioni[
    ,
    .SD[1],
    by = .(CODICE_COMUNE, LISTA),
    .SDcols = c("DATA", "DELTA", "ELETTORI", "CODICE_REGIONE", "REGIONE", "CODICE_PROVINCIA", "PROVINCIA", "COMUNE")
  ]
  
  # Copio per ogni comune il sigma P della lista
  comuni_liste <- comuni_liste[
    liste[, .(LISTA, SIGMA_P)],
    on = .(LISTA)
  ]
  
  return(list(
    liste = liste,
    comuni_liste = comuni_liste,
    liste_elezioni = liste_elezioni
  ))
}


simula <- function(
    iterazioni = 200,
    parametri_simulazione,
    pop_legale,
    data_elezione
) {
  
  liste <- parametri_simulazione$liste
  comuni_liste <- parametri_simulazione$comuni_liste
  
  iterazione <- function(
    iter = 1,
    liste,
    comuni_liste,
    pop_legale,
    data_elezione
  ) {
    #### Simulazione percentuali regionali ####
    liste$LOG_P_ITER <- rnorm(
      liste$LOGIT_P,
      liste$LOGIT_P,
      liste$SIGMA_R * unclass(data_elezione - liste$DATA)^0.5
    )
    
    #### Simulazione percentuali per provincia ####
    comuni_liste <- merge(
      comuni_liste,
      liste[, c("LISTA", "LOG_P_ITER")]
    )
    
    names(comuni_liste)[names(comuni_liste) == "LOG_P_ITER"] <- "LOG_P_ITER_R"
    
    comuni_liste$DELTA_ITER <- rnorm(
      comuni_liste$DELTA,
      comuni_liste$DELTA,
      comuni_liste$SIGMA_P * unclass(data_elezione - comuni_liste$DATA)^0.5
    )
    
    comuni_liste$LOG_P_ITER <- comuni_liste$LOG_P_ITER_R + comuni_liste$DELTA_ITER
    
    comuni_liste$PERCENTUALE_ITER <- ave(
      comuni_liste$LOG_P_ITER,
      comuni_liste$CODICE_COMUNE,
      FUN = function(x) plogis(x) / sum(plogis(x))
    )
    
    
    comuni_liste$VOTI_LISTA_ITER <- comuni_liste$ELETTORI * comuni_liste$PERCENTUALE_ITER
    
    scrutinio <- scrutinio_regionali_ER(
      comuni_liste,
      pop_legale,
      liste
    )
    
    
    scrutinio$prov_lista <- merge(
      scrutinio$prov_lista,
      aggregate(
        VOTI_LISTA_ITER ~ PROVINCIA,
        scrutinio$prov_lista,
        sum
      ),
      by = "PROVINCIA",
      suffixes = c("", "_TOT")
    )
    
    scrutinio$prov_lista$PERCENTUALE <-
      scrutinio$prov_lista$VOTI_LISTA_ITER / 
      scrutinio$prov_lista$VOTI_LISTA_ITER_TOT
    
    scrutinio$liste <- merge(
      liste[, c(
        "COALIZIONE",
        "LISTA"
      )],
      aggregate(
        cbind(ELETTI, VOTI_LISTA_ITER) ~ LISTA,
        scrutinio$prov_lista,
        sum
      )
    )
    
    scrutinio$liste$PERCENTUALE <-
      scrutinio$liste$VOTI_LISTA_ITER / 
      sum(scrutinio$liste$VOTI_LISTA_ITER)
    
    scrutinio$coalizioni <- merge(
      scrutinio$coalizioni,
      aggregate(
        cbind(VOTI_LISTA_ITER, ELETTI) ~ COALIZIONE,
        scrutinio$liste,
        sum
      )
    )
    
    scrutinio$coalizioni$PERCENTUALE <-
      scrutinio$coalizioni$VOTI_LISTA_ITER / 
      sum(scrutinio$coalizioni$VOTI_LISTA_ITER)
    
    scrutinio$coalizioni$ELETTI_TOT <-
      scrutinio$coalizioni$PRESIDENTE +
      scrutinio$coalizioni$MIGLIOR_PERDENTE +
      scrutinio$coalizioni$ELETTI
    
    
    
    
    list(
      coalizioni = scrutinio$coalizioni,
      liste = scrutinio$liste,
      prov_lista = scrutinio$prov_lista
    )
    
  }
  
  cl <- parallel::makeCluster(parallel::detectCores())
  
  parallel::clusterEvalQ(
    cl,
    {
      source("R/scrutinio.R")
      library(data.table)
    }
  )
  
  lista_risultati <- parallel::parLapply(
    cl,
    seq_len(iterazioni),
    iterazione,
    liste = liste,
    comuni_liste = comuni_liste,
    pop_legale = pop_legale,
    data_elezione = data_elezione
  )
  
  parallel::stopCluster(cl)
  
  risultato <- list()
  
  risultato$coalizioni <-
    data.table::rbindlist(lapply(lista_risultati, function(l) l$coalizioni), idcol = "SIM")
  risultato$liste <-
    data.table::rbindlist(lapply(lista_risultati, function(l) l$liste), idcol = "SIM")
  risultato$prov_lista <-
    data.table::rbindlist(lapply(lista_risultati, function(l) l$prov_lista), idcol = "SIM")
  
  lista_risultati <- NULL
  
  risultato
}
