# TODO: trasformare le coppie CODICE/NOME in factors
# TODO: dati VdA 2022

fonti <- list(
  list(
    elezione = "camera 2018",
    data = "2018-03-04",
    url = "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/camera/camera-20180304.zip",
    files = list(
      list(
        internal_path = "Camera2018_livComune.txt",
        encoding = "Latin-1",
        tipo = "scrutinio",
        colonne = list(
          VOTI = "VOTI_LISTA"
        ),
        funzione = function(DT) {
          DT$REGIONE <- DT$CIRCOSCRIZIONE
          # Poiché VOTI_LISTA è NA per la circoscrizione AOSTA, copio in quella 
          # colonna i voti per il candidato (Nella circoscrizione Aosta ci sono solo 
          # candidati uninominali)
          return(DT[
            CIRCOSCRIZIONE == "AOSTA",
            `:=`(
              VOTI_LISTA = VOTI_CANDIDATO,
              REGIONE = "VALLE D'AOSTA"
            )
          ])
        }
      )
    )
  ),
  
  list(
    elezione = "camera 2022",
    data = "2022-09-25",
    url = "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/camera/camera-20220925.zip",
    files = list(
      list(
        internal_path = "Camera_Italia_LivComune.csv",
        tipo = "scrutinio",
        colonne = list(
          VOTI = "VOTILISTA",
          ELETTORI = "ELETTORITOT",
          LISTA = "DESCRLISTA"
        ),
        funzione = function(DT) {
          DT$REGIONE <- DT$`CIRC-REG`
          return(DT)
        }
        
      )
    )
  ),
  
  list(
    elezione = "regionali 2020",
    data = "2020-01-26",
    url = "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/regionali/regionali-20200126.zip",
    files = list(
      list(
        internal_path = "regionali-20200126.txt",
        tipo = "scrutinio",
        colonne = list(
          VOTI = "VOTI_LISTA"
        )
      )
    )
  ),
  
  list(
    elezione = "europee 2019",
    data = "2019-05-26",
    url = "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/europee/europee-20190526.zip",
    files = list(
      list(
        internal_path = "europee-20190526.txt",
        encoding = "Latin-1",
        tipo = "scrutinio",
        colonne = list(
          VOTI = "VOTI_LISTA"
        )
      )
    )
  ),
  
  list(
    elezione = "europee 2024",
    data = "2024-06-08",
    url = "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/europee/europee-20240609.zip",
    files = list(
      list(
        internal_path = "EUROPEE_ITALIA_LivComune.csv",
        encoding = "Latin-1",
        tipo = "scrutinio",
        colonne = list(
          VOTI = "NUMVOTI",
          LISTA = "DESCLISTA",
          COMUNE = "DESCCOMUNE",
          REGIONE = "DESCREGIONE"
        )
      )
    )
  ),
  
  list(
    elezione = "regionali 2024",
    data = "2024-11-17",
    url = "https://elezionistorico.interno.gov.it/daithome/documenti/opendata/regionali/regionali-20241117.zip",
    files = list(
      list(
        internal_path = "Regionali_EmiliaRomagna_2024_Scrutini.csv",
        tipo = "scrutinio",
        colonne = list(
          VOTI = "VOTI_LISTA",
          ELETTORI = "ELETTORI_TOTALI"
        )
      ),
      
      list(
        internal_path = "Regionali_EmiliaRomagna_2024_Preferenze.csv",
        tipo = "preferenze",
        encoding = "Latin-1"
      )
    )
  )
)


ISTAT_API <- function(url) {
  data.table::as.data.table(
    jsonlite::fromJSON(
      paste0(url, format(Sys.time(), "%d/%m/%Y"))
    )[[1]]
  )
}

#' Normalizzazione robusta di nomi geografici
#'
#' Converte in minuscolo, rimuove accenti, apostrofi, slash e trattini,
#' normalizza gli spazi. Opzionalmente mantiene solo il primo token.
#'
#' @param x Character vector
#' @param first_token Logical; se TRUE mantiene solo la prima parola
#'
#' @return Character vector normalizzato
#' @keywords internal
normalize_name <- function(x, first_token = FALSE) {
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[’']", " ", x)
  x <- gsub("[-/]", " ", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)
  
  if (first_token) {
    x <- sub("\\s+.*$", "", x)
  }
  
  x
}

#' Prepara la tabella ISTAT per il matching dei comuni
#'
#' @param istat data.table con colonne COMUNE e DEN_REG_DT_FI
#'
#' @return data.table con colonne COMUNE_NORM e REGIONE_NORM aggiunte
#'
#' @export
prep_istat <- function(istat) {
  if (!data.table::is.data.table(istat)) {
    stop("istat deve essere un data.table")
  }
  
  richieste <- c("COMUNE", "DEN_REG_DT_FI")
  mancanti <- setdiff(richieste, names(istat))
  if (length(mancanti) > 0) {
    stop("Colonne mancanti in istat: ", paste(mancanti, collapse = ", "))
  }
  
  istat[
    ,
    `:=`(
      COMUNE_NORM  = normalize_name(COMUNE, first_token = FALSE),
      REGIONE_NORM = normalize_name(DEN_REG_DT_FI, first_token = TRUE)
    )
  ]
  
  istat
}



#' Aggiunge codice e nome attuale del comune ai risultati elettorali
#'
#' Effettua il matching tra risultati elettorali e tabella ISTAT,
#' usando prima un confronto esatto su nomi normalizzati e regione,
#' poi un fallback fuzzy basato su distanza di Levenshtein.
#'
#' @param dt data.table dei risultati elettorali
#' @param istat data.table preparato con prep_istat()
#' @param max_dist distanza massima ammessa per il matching fuzzy
#' @param verbose se TRUE, stampa le associazioni fuzzy effettuate
#'
#' @return data.table con colonne CODICE_COMUNE e COMUNE_ATTUALE
#'
#' @export
aggiungi_codice_comune <- function(
    dt,
    istat,
    max_dist = 2L,
    verbose = TRUE
) {
  if (!data.table::is.data.table(dt)) {
    stop("dt deve essere un data.table")
  }
  if (!data.table::is.data.table(istat)) {
    stop("istat deve essere un data.table")
  }
  
  richieste_dt <- c("COMUNE", "REGIONE")
  mancanti_dt <- setdiff(richieste_dt, names(dt))
  if (length(mancanti_dt) > 0) {
    stop("Colonne mancanti in dt: ", paste(mancanti_dt, collapse = ", "))
  }
  
  richieste_istat <- c(
    "COMUNE_NORM", "REGIONE_NORM",
    "PRO_COM_T_DT_FI", "COMUNE_DT_FI"
  )
  mancanti_istat <- setdiff(richieste_istat, names(istat))
  if (length(mancanti_istat) > 0) {
    stop(
      "istat non preparato correttamente. ",
      "Usa prep_istat() prima."
    )
  }
  
  dt <- data.table::copy(dt)
  
  # normalizzazione input
  dt[
    ,
    `:=`(
      COMUNE_NORM  = normalize_name(COMUNE, first_token = FALSE),
      REGIONE_NORM = normalize_name(REGIONE, first_token = TRUE)
    )
  ]
  
  # chiavi uniche
  chiavi <- unique(dt[, .(COMUNE_NORM, REGIONE_NORM)])
  
  # MATCH ESATTO
  match_esatto <- istat[
    chiavi,
    on = .(COMUNE_NORM, REGIONE_NORM),
    nomatch = 0L,
    .(
      COMUNE_NORM,
      REGIONE_NORM,
      CODICE_COMUNE  = PRO_COM_T_DT_FI,
      COMUNE_ATTUALE = COMUNE_DT_FI,
      tipo_match = "esatto"
    )
  ]
  
  # Appaiamento manuale perché altrimenti lo appaia con San Giovanni d'Asso
  match_esatto <- rbindlist(
    list(
      match_esatto,
      data.table::data.table(
        COMUNE_NORM    = "san giovanni di fassa",
        REGIONE_NORM   = "trentino",
        CODICE_COMUNE  = "022250",
        COMUNE_ATTUALE = "San Giovanni di Fassa-Sèn Jan",
        tipo_match     = "manuale"
      )
    )
  )
  
  irrisolti <- chiavi[!match_esatto, on = .(COMUNE_NORM, REGIONE_NORM)]
  
  # MATCH FUZZY
  match_fuzzy <- NULL
  log_fuzzy <- character()
  
  if (nrow(irrisolti) > 0) {
    for (i in seq_len(nrow(irrisolti))) {
      c_norm <- irrisolti$COMUNE_NORM[i]
      r_norm <- irrisolti$REGIONE_NORM[i]
      
      d <- stringdist::stringdist(c_norm, istat$COMUNE_NORM, method = "jw")
      best <- which.min(d)
      
      
      match_fuzzy <- data.table::rbindlist(
        list(
          match_fuzzy,
          data.table::data.table(
            COMUNE_NORM    = c_norm,
            REGIONE_NORM   = r_norm,
            CODICE_COMUNE  = istat$PRO_COM_T_DT_FI[best],
            COMUNE_ATTUALE = istat$COMUNE_DT_FI[best],
            tipo_match     = "fuzzy"
          )
        ),
        fill = TRUE
      )
      
      log_fuzzy <- c(
        log_fuzzy,
        sprintf(
          "Fuzzy match: '%s' (regione '%s') → '%s' (regione: '%s'). Distanza = %f",
          c_norm,
          r_norm,
          istat$COMUNE_DT_FI[best],
          istat$REGIONE_NORM[best],
          d[best]
        )
      )
      
    }
  }
  
  if (verbose && length(log_fuzzy) > 0) {
    message(
      "Associazioni fuzzy effettuate:\n",
      paste(log_fuzzy, collapse = "\n")
    )
  }
  
  mappa <- data.table::rbindlist(
    list(match_esatto, match_fuzzy),
    fill = TRUE
  )
  
  dt[
    mappa,
    on = .(COMUNE_NORM, REGIONE_NORM),
    `:=`(
      CODICE_COMUNE  = i.CODICE_COMUNE,
      COMUNE_ATTUALE = i.COMUNE_ATTUALE
    )
  ]
  
  dt[, c("COMUNE_NORM", "REGIONE_NORM") := NULL]
  
  dt
}


scarica_dati <- function(
    cache = TRUE, 
    cache_path = file.path(tempdir(), "dati.RData"),
    cache_raw = FALSE,
    cache_raw_path = tempdir()
) {
  
  cat("\nScarico i dati dal web...\n")
  
  #### Codici statistici e unità territoriali ####
  
  ISTAT <- ISTAT_API("https://situas-servizi.istat.it/publish/reportspooljson?pfun=61&pdata=")
  
  #### Variazioni amministrative territoriali ####
  
  ISTAT_traslazione <- ISTAT_API("https://situas-servizi.istat.it/publish/reportspooljson?pfun=304&pdatada=01/01/1991&pdataa=")
  
  ISTAT_traslazione <- prep_istat(ISTAT_traslazione)
  
  scarica_fonte <- function(
    elezione,
    data_elezione,
    url_fonte,
    files
  ) {
    tryCatch(
      {
        cat("\nDownload dei dati dell'elezione", elezione,"...\n")
        
        # Preparo il nome del file temporaneo da scaricare
        file_path <- tempfile(fileext = ".zip")
        
        # Scarico il file zip
        download.file(url_fonte, file_path)
        
        # Estraggo i file
        unzip(file_path, exdir = tempdir())
        
        # Elaboro i file
        lapply(
          files,
          function(dettagli_file) {
            
            # Leggo il file
            DT <- data.table::fread(
              file.path(tempdir(), dettagli_file$internal_path), 
              encoding = ifelse(is.null(dettagli_file$encoding), "unknown", dettagli_file$encoding)
            )
            
            # Se necessario applico una funzione custom per pulire il file
            if (!is.null(dettagli_file$funzione)) {
              DT <- dettagli_file$funzione(DT)
            }
            
            # Standardizzo i nomi
            mapply(
              function(nome_standard, nome_non_standard) data.table::setnames(DT, nome_non_standard, nome_standard),
              names(dettagli_file$colonne),
              dettagli_file$colonne
            )
            
            # Salvo il singolo data.table
            if (cache_raw) {
              raw_file_path <- file.path(cache_raw_path, elezione, paste0(dettagli_file$internal_path, ".RData"))
              if (!dir.exists(dirname(raw_file_path))) {
                dir.create(dirname(raw_file_path), recursive = TRUE)
              }
              saveRDS(DT, file = raw_file_path)
            }
            
            # Aggiorno i nomi dei comuni
            DT <- aggiungi_codice_comune(DT, ISTAT_traslazione)
            
            if (dettagli_file$tipo == "scrutinio") {
              
              astensione <- DT[
                ,
                .(
                  VOTI = ELETTORI - sum(VOTI),
                  LISTA = "astensione"
                ),
                by = c("COMUNE_ATTUALE", "CODICE_COMUNE", "ELETTORI", intersect("SEZIONE", names(DT)))
              ]
              DT <- rbind(DT, astensione, fill = TRUE)
              
              comuni_liste_elezioni <<-  rbind(
                comuni_liste_elezioni,
                data.table::data.table(
                  DATA = as.POSIXct(data_elezione),
                  ELEZIONE = elezione,
                  COMUNE = DT$COMUNE_ATTUALE,
                  CODICE_COMUNE = DT$CODICE_COMUNE,
                  LISTA = DT$LISTA,
                  VOTI = DT$VOTI
                )
              )
            } else {
              # Tengo solo le colonne di interesse e sommo le preferenze per comune
              DT <- DT[
                ,
                .(
                  PREFERENZE = sum(PREFERENZE)
                ),
                by = .(
                  COMUNE_ATTUALE,
                  CODICE_COMUNE,
                  LISTA,
                  NOME,
                  COGNOME
                )
              ]
              
              preferenze <<- rbind(
                preferenze,
                data.table::data.table(
                  DATA = as.POSIXct(data_elezione),
                  ELEZIONE = elezione,
                  COMUNE = DT$COMUNE_ATTUALE,
                  CODICE_COMUNE = DT$CODICE_COMUNE,
                  LISTA = DT$LISTA,
                  NOME = DT$NOME,
                  COGNOME = DT$COGNOME,
                  PREFERENZE = DT$PREFERENZE
                )
              )
            }
            
            
            
          }
        )
        
      },
      error = function(e) warning(
        "Non sono riuscito a caricare i dati delle elezioni ",
        elezione, ", a causa di questo errore: ", e
      )
    )
  }
  
  #### Inizializzo data.table dati ####
  
  comuni_liste_elezioni <- data.table::data.table(
    DATA = as.POSIXct(character(0)),
    ELEZIONE = character(0),
    COMUNE = character(0),
    CODICE_COMUNE = character(0),
    LISTA = character(0),
    VOTI = numeric(0)
  )
  
  preferenze <- data.table::data.table(
    DATA = as.POSIXct(character(0)),
    ELEZIONE = character(0),
    COMUNE = character(0),
    CODICE_COMUNE = character(0),
    LISTA = character(0),
    NOME = character(0),
    COGNOME = character(0),
    PREFERENZE = numeric(0)
  )
  
  #### Scarico le fonti ####
  
  lapply(
    fonti,
    function(fonte) {
      scarica_fonte(
        fonte$elezione,
        fonte$data,
        fonte$url,
        fonte$files
      )
    }
  )
  
  
  
  # Controlla che non siano presenti codici comune sconosciuti
  stopifnot(length(setdiff(comuni_liste_elezioni$CODICE_COMUNE, ISTAT$PRO_COM_T)) == 0)
  stopifnot(length(setdiff(preferenze$CODICE_COMUNE, ISTAT$PRO_COM_T)) == 0)
  
  # Aggiungo i codici e i nomi di provincia e regione
  comuni_liste_elezioni <- merge(
    comuni_liste_elezioni,
    ISTAT[
      ,
      .(
        CODICE_COMUNE = PRO_COM_T,
        CODICE_PROVINCIA = COD_UTS,
        PROVINCIA = DEN_UTS,
        CODICE_REGIONE = COD_REG,
        REGIONE = DEN_REG
      )
    ]
  )
  
  preferenze <- merge(
    preferenze,
    ISTAT[
      ,
      .(
        CODICE_COMUNE = PRO_COM_T,
        CODICE_PROVINCIA = COD_UTS,
        PROVINCIA = DEN_UTS,
        CODICE_REGIONE = COD_REG,
        REGIONE = DEN_REG
      )
    ]
  )
  
  # Sommo le righe riferite allo stesso comune nella stessa elezione
  # (dovute ad esempio a comuni che si sono fusi insieme successivamente)
  comuni_liste_elezioni <- comuni_liste_elezioni[
    ,
    .(VOTI = sum(VOTI)),
    by = .(
      DATA,
      ELEZIONE,
      CODICE_REGIONE,
      REGIONE,
      CODICE_PROVINCIA,
      PROVINCIA,
      CODICE_COMUNE,
      COMUNE,
      LISTA
    )
  ]
  
  # Popolazione legale ----
  
  sdmx <- rsdmx::readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,DF_DCSS_POP_LEGALE_TV,1.0/A../ALL/?detail=full&dimensionAtObservation=TIME_PERIOD")
  pop_legale <- data.table::as.data.table(as.data.frame(sdmx))
  
  # Aggiorno i codici dei comuni
  pop_legale[ISTAT_traslazione,
             on = .(REF_AREA = PRO_COM_T),
             `:=`(
               PRO_COM_T_DT_FI = i.PRO_COM_T_DT_FI
             )
  ]
  
  pop_legale <- pop_legale[!is.na(PRO_COM_T_DT_FI)]
  
  # Sommo la popolazione dei comuni fusi insieme
  pop_legale <- pop_legale[
    , .(POPOLAZIONE = sum(obsValue, na.rm = TRUE)),
    by = PRO_COM_T_DT_FI
  ]
  
  #Rinomino le colonne
  pop_legale[
    ISTAT,
    on = .(PRO_COM_T_DT_FI = PRO_COM_T),
    `:=`(
      COMUNE = i.COMUNE,
      CODICE_PROVINCIA = i.COD_UTS,
      PROVINCIA = i.DEN_UTS,
      CODICE_REGIONE = i.COD_REG,
      REGIONE = i.DEN_REG
    )
  ]
  data.table::setnames(pop_legale, "PRO_COM_T_DT_FI", "CODICE_COMUNE")
  
  
  # Salvataggio ----
  dati <- list(
    comuni_liste_elezioni = comuni_liste_elezioni, 
    pop_legale = pop_legale, 
    preferenze = preferenze,
    ISTAT = ISTAT,
    ISTAT_traslazione = ISTAT_traslazione
  )
  
  # Salvo il file
  if (cache) {
    if (!dir.exists(dirname(cache_path))) {
      dir.create(
        dirname(cache_path),
        recursive = TRUE
      )
    }
    saveRDS(dati, file = cache_path)
  }
  
  return(dati)
  
  # TODO: parallelizzare e/o usare i join di data.table
  
}


#' Filter a data.table
#'
#' @param DT data.table to be filtered 
#' @param filters named list of vectors, each element should have the name of a 
#'  DT column and should be a vector of elements of that column selecting the 
#'  rows to keep
#'
#' @returns a filtered data.table, with only the rows that contains, in every
#'  column listed in filters, a value present in the respective filter element
#' 
#'
#' @examples
filter_dt <- function(DT, filters) {
  stopifnot(data.table::is.data.table(DT), is.list(filters))
  
  if (length(filters) == 0) return(DT)
  
  cols_ok <- names(filters) %in% names(DT)
  if (!all(cols_ok)) {
    stop("Columns not present in dt: ",
         paste(names(filters)[!cols_ok], collapse = ", "))
  }
  
  DT[
    Reduce(
      `&`,
      lapply(
        names(filters),
        function(col) get(col) %in% filters[[col]]
      )
    )
  ]
}

carica_dati <- function(cache = TRUE, cache_path = file.path(tempdir(), "dati.RData"), filtro = list()) {
  if (cache & file.exists(cache_path)) {
    dati <- readRDS(cache_path)
  } else {
    
    dati <- scarica_dati(cache, cache_path)
  }
  
  dati[1:3] <- lapply(dati[1:3], filter_dt, filter = filtro)
  
  return(dati)
}




