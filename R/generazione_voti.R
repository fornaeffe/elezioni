# TODO usare rstan con modello Dirichlet
# TODO verificare se usando un unico data.table anziché parallel diventa più veloce

simulazione <- function(sim, comuni_liste, liste, data_elezione) {
  data.table::setDTthreads(1)
  
  # Copio le tabelle per evitare di modificare l'originale
  comuni_liste_temp <- data.table::copy(comuni_liste)
  liste_temp <- data.table::copy(liste)
  
  # Simulo i logit della percentuale
  liste_temp[
    ,
    LOGIT_P_SIM := rnorm(
      LOGIT_P,
      LOGIT_P,
      SIGMA_GLOBAL * unclass(data_elezione - DATA)^0.5
    )
  ]
  
  # Passo i valori simulati ai singoli comuni
  comuni_liste_temp <- comuni_liste_temp[
    liste_temp[, .(LISTA, LOGIT_P_SIM_GLOBAL = LOGIT_P_SIM)],
    on = .(LISTA)
  ]
  
  # Simulo il drift del delta
  comuni_liste_temp[
    ,
    DELTA_SIM := rnorm(
      DELTA,
      DELTA,
      SIGMA_DELTA * unclass(data_elezione - DATA)^0.5
    )
  ]
  
  # Calcolo il logit della percentuale a livello comunale
  comuni_liste_temp[
    ,
    LOGIT_P_SIM := LOGIT_P_SIM_GLOBAL + DELTA_SIM
  ]
  
  # Ritrasformo in percentuale
  comuni_liste_temp[
    ,
    PERCENTUALE_SIM := plogis(LOGIT_P_SIM) / sum(plogis(LOGIT_P_SIM)),
    by = CODICE_COMUNE
  ]
  
  # Converto la percentuale in numero di voti
  comuni_liste_temp[
    ,
    VOTI_LISTA_SIM := round(PERCENTUALE_SIM * ELETTORI)
  ]
  
  # Restituisco il risultato
  return(comuni_liste_temp)
  
}

genera_voti <- function(
    dati,
    scenario,
    data_elezione,
    simulazioni = 1000
){
  
  liste <- data.table::as.data.table(readxl::read_xlsx(scenario, "liste_future"))
  coalizioni <- data.table::as.data.table(readxl::read_xlsx(scenario, "coalizioni_future"))
  corrispondenza_liste <- data.table::as.data.table(readxl::read_xlsx(scenario, "flussi_previsti"))
  
  # Riempio i colori mancanti:
  liste$COLORE[is.na(liste$COLORE)] <- "#DDDDDD"
  
  # Se non ci sono le date di nascita dei candidati sindaci, le genero casualmente
  if ("DATA_DI_NASCITA" %in% names(coalizioni)) {
    coalizioni[
      ,
      DATA_DI_NASCITA := as.Date(DATA_DI_NASCITA)
    ]
    coalizioni[
      is.na(DATA_DI_NASCITA),
      DATA_DI_NASCITA := sample(seq(as.Date('1940/01/01'), as.Date('2000/01/01'), by="day"), .N, replace = TRUE)
    ]  
  }
  
  
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
    coalizioni,
    "CANDIDATO_SINDACO",
    "COALIZIONE",
    skip_absent = TRUE
  )
  
  data.table::setnames(
    liste,
    c("LISTA_FUTURA", "CANDIDATO_SINDACO"),
    c("LISTA", "COALIZIONE"),
    skip_absent = TRUE
  )
  
  # Normalizzo
  data.table::setkey(corrispondenza_liste, ELEZIONE, LISTA_ORIGINALE, LISTA)
  corrispondenza_liste[
    , 
    FATTORE := FATTORE / sum(FATTORE), 
    by=.(ELEZIONE, LISTA_ORIGINALE)
  ]
  
  # Riassumo i dati per comune
  comuni_liste_elezioni <- data.table::copy(dati$comuni_liste_elezioni)
  data.table::setnames(comuni_liste_elezioni, "LISTA", "LISTA_ORIGINALE")
  
  # Individuo le liste corrispondenti, e calcolo i voti per le nuove liste
  comuni_liste_elezioni <- comuni_liste_elezioni[
    corrispondenza_liste,
    on = .(DATA, ELEZIONE, LISTA_ORIGINALE),
    nomatch = NULL
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
    keyby = .(DATA, ELEZIONE, LISTA)
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
  data.table::setorder(liste_elezioni, DATA)
  
  liste <- liste[
    liste_elezioni[
      ,
      .(
        SIGMA_GLOBAL = sd(diff(.SD$LOGIT_P) / unclass(diff(.SD$DATA))^0.5),
        PERCENTUALE = PERCENTUALE[.N],
        LOGIT_P = LOGIT_P[.N],
        VOTI_LISTA = VOTI[.N],
        DATA = DATA[.N]
      ),
      by = .(LISTA)
    ],
    on = .(LISTA)
  ]
  
  # Calcolo la differenza tra il logit comunale e il logit globale
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
        SIGMA_DELTA = sd(
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
  
  liste$SIGMA_GLOBAL[is.na(liste$SIGMA_GLOBAL)] <- mean(liste$SIGMA_GLOBAL, na.rm = TRUE)
  liste$SIGMA_DELTA[is.na(liste$SIGMA_DELTA)] <- mean(liste$SIGMA_DELTA, na.rm = TRUE)
  
  # Per ciascuna lista seleziono solo l'ultima elezione
  data.table::setorder(comuni_liste_elezioni, CODICE_COMUNE, LISTA, -DATA)
  comuni_liste <- comuni_liste_elezioni[
    ,
    .SD[1],
    by = .(CODICE_COMUNE, LISTA),
    .SDcols = c("DATA", "DELTA", "ELETTORI", "CODICE_REGIONE", "REGIONE", "CODICE_PROVINCIA", "PROVINCIA", "COMUNE")
  ]
  
  # Copio per ogni comune il sigma delta della lista
  comuni_liste <- comuni_liste[
    liste[, .(LISTA, SIGMA_DELTA)],
    on = .(LISTA)
  ]
  
  # Simulo i voti nella futura elezione
  cl <- parallel::makeCluster(parallel::detectCores())
  
  parallel::clusterEvalQ(cl, library(data.table))
  
  comuni_liste_sim <- parallel::parLapply(
    cl,
    seq_len(simulazioni),
    simulazione,
    comuni_liste = comuni_liste,
    liste = liste,
    data_elezione = data_elezione
  )
  
  parallel::stopCluster(cl)
  
  # Trasformo la lista in un data.table
  comuni_liste_sim <- data.table::rbindlist(comuni_liste_sim, idcol = "SIM")
  
  return(list(
    comuni_liste_sim = comuni_liste_sim,
    liste = liste,
    comuni_liste = comuni_liste,
    liste_elezioni = liste_elezioni,
    coalizioni = coalizioni
  ))
}



