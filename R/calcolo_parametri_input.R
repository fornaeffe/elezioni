# TODO usare rstan con modello Dirichlet

calcola_parametri_input <- function(
    dati,
    scenario,
    percentuali_partenza = NULL
){
  message("\nCalcolo i parametri di input...\n")
  
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
    nomatch = NULL,
    allow.cartesian = TRUE
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
  # TODO verificare se si può riusare il valore elettori preso dai dati
  
  # Calcolo le percentuali per ciascuna provincia e ciascuna elezione
  comuni_liste_elezioni[, PERCENTUALE := formattable::percent(VOTI / ELETTORI, 2)]
  
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
    PERCENTUALE := formattable::percent(VOTI / sum(VOTI), 2),
    by = ELEZIONE
  ]
  
  # Ne calcolo il logit
  liste_elezioni[, LOGIT_P := qlogis(PERCENTUALE)]
  
  # Per ogni lista, calcolo la deviazione standard della velocità di cambiamento
  # del logit della percentuale
  
  data.table::setorder(liste_elezioni, DATA)
  
  liste <- liste[
    liste_elezioni[
      ,
      .(
        SIGMA_GLOBAL = sd(diff(.SD$LOGIT_P) / unclass(diff(.SD$DATA))^0.5)
      ),
      by = .(LISTA)
    ],
    on = .(LISTA)
  ]
  
  # Riporto, inoltre, il logit della percentuale e la data relativi all'ultima elezione
  # Se disponibile una elezione di tipo indicato, prende l'ultima, se no
  # ricade sull'ultima elezione di qualsiasi tipo.
  if (is.null(percentuali_partenza)) {
    liste_elezioni$liste_elezioni_da_considerare <- TRUE
  } else {
    liste_elezioni[, liste_elezioni_da_considerare := startsWith(ELEZIONE, percentuali_partenza)]
  }
  
  data.table::setorder(liste_elezioni, liste_elezioni_da_considerare, DATA)
  
  liste <- liste[
    liste_elezioni[
      ,
      .(
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
  
  # Controllo se ci sono liste non presenti in tutti i comuni
  cj <- data.table::CJ(
    COMUNE = comuni_liste$COMUNE,
    LISTA = comuni_liste$LISTA,
    unique = TRUE
  )[!comuni_liste, on = .(COMUNE, LISTA)]
  
  cjtb <- table(cj$LISTA)
  
  if (length(cjtb) > 0) {
    message("Alcune liste non sono presenti in tutti i comuni. Di seguito il numero
        di comuni nei quali la lista non è presente:\n")
    print(cjtb)
  }
  
  return(
    list(
      liste = liste,
      comuni_liste = comuni_liste,
      liste_elezioni = liste_elezioni,
      coalizioni = coalizioni,
      corrispondenza_liste = corrispondenza_liste
    )
  )
}
