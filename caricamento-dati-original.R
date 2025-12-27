library(data.table)
library(stringr)
library(rsdmx)

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
          # Poiché VOTI_LISTA è NA per la circoscrizione AOSTA, copio in quella 
          # colonna i voti per il candidato (Nella circoscrizione Aosta ci sono solo 
          # candidati uninominali)
          return(DT[
            CIRCOSCRIZIONE == "AOSTA",
            VOTI_LISTA := VOTI_CANDIDATO
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
        )
        
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
          COMUNE = "DESCCOMUNE"
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
  as.data.table(
    jsonlite::fromJSON(
      paste0(url, format(Sys.time(), "%d/%m/%Y"))
    )[[1]]
  )
}


#### Codici statistici e unità territoriali ####

ISTAT <- ISTAT_API("https://situas-servizi.istat.it/publish/reportspooljson?pfun=61&pdata=")

#### Variazioni amministrative territoriali ####

ISTAT_traslazione <- ISTAT_API("https://situas-servizi.istat.it/publish/reportspooljson?pfun=304&pdatada=01/01/1991&pdataa=")


# Funzione che aggiorna i nomi dei comuni e aggiunge i codici
aggiorna_comuni <- function(DT) {
  
  cat("Uniformo e aggiorno i nomi dei comuni...\n")
  
  nomi_comuni <- unique(DT[,"COMUNE"][[1]])
  
  tutti_i_nomi <- c(
    ISTAT$COMUNE,
    ISTAT$COMUNE_IT,
    ISTAT_traslazione$COMUNE
  )
  
  
  
  # Funzione che cerca un nome di comune scritto nello stesso modo
  cerca_nome_identico <- function(nome) {
    # Cerco il nome nei comuni attuali
    matches <- which(
      toupper(ISTAT$COMUNE) == nome
    )
    
    if (length(matches) > 0) return(list(
      comune = ISTAT$COMUNE[matches[1]],
      codice = ISTAT$PRO_COM_T[matches[1]]
    ))
    
    # Cerco il nome nei comuni attuali
    matches <- which(
      toupper(ISTAT$COMUNE_IT) == nome
    )
    
    if (length(matches) > 0) return(list(
      comune = ISTAT$COMUNE[matches[1]],
      codice = ISTAT$PRO_COM_T[matches[1]]
    ))
    
    # Cerco il nome nei comuni variati,
    # se lo trovo aggiorno il nome con il nome attuale
    matches <- which(
      toupper(ISTAT_traslazione$COMUNE) == nome
    )
    
    if (length(matches) > 0) return(list(
      comune = ISTAT_traslazione$COMUNE_DT_FI[matches[1]],
      codice = ISTAT_traslazione$PRO_COM_T_DT_FI[matches[1]]
    ))
    
    return(NA)
  }
  
  # Funzione che cerca il nome del comune anche scritto in modo diverso
  cerca_nome <- function(nome) {
    
    # Comincio cercando il nome così come è scritto
    risultato <- cerca_nome_identico(nome)
    
    if (length(risultato) > 1) return (risultato)
    
    # Se non lo ho trovato, converto gli apostrofi in accenti
    # e lo cerco nuovamente
    risultato <- cerca_nome_identico(
      str_replace_all(
        nome,
        c(
          "A'" = toupper("à"),
          "E'" = toupper("è"),
          "I'" = toupper("ì"),
          "O'" = toupper("ò"),
          "U'" = toupper("ù")
        )
      )
    )
    
    if(length(risultato) > 1) return (risultato)
    
    # Controlla se il nome è da cambiare manualmente
    if (nome == "MALE'") return(cerca_nome_identico(toupper("Malé")))
    if (nome == "S+N JAN DI FASSA") return(cerca_nome_identico("SAN GIOVANNI DI FASSA"))
    if (nome == "HONE") return(cerca_nome_identico(toupper("Hône")))
    
    # Se ancora non lo ho trovato,
    # cerco nomi simili tra tutti i nomi possibili
    distanze <- adist(
      tutti_i_nomi, 
      nome, 
      ignore.case = TRUE
    )
    matches <- which(distanze == min(distanze))
    
    # Se non lo trovo avviso e restituisco NA
    if (length(matches) == 0) {
      warning("Comune ", nome, " non trovato negli elenchi ISTAT")
      
      return(list(
        comune = NA,
        codice = NA
      ))
    }
    
    # Se lo trovo avviso della corrispondenza trovata
    cat(nome, "corrisponde a", tutti_i_nomi[matches[1]], "\n")
    
    # In base alla posizione dentro "tutti_i_nomi", recupero il codice
    # e il nome del comune dagli elenchi ISTAT
    if (matches[1] <= nrow(ISTAT)) return(list(
      comune = ISTAT$COMUNE[matches[1]],
      codice = ISTAT$PRO_COM_T[matches[1]]
    ))
    
    if (matches[1] <= nrow(ISTAT) * 2) return(list(
      comune = ISTAT$COMUNE[matches[1] - nrow(ISTAT)],
      codice = ISTAT$PRO_COM_T[matches[1] - nrow(ISTAT)]
    ))
    
    # Se devo andarlo a cercare nei nomi passati, aggiorno il nome
    # e il codice a quelli attuali
    return(list(
      comune = ISTAT_traslazione$COMUNE_DT_FI[matches[1] - nrow(ISTAT) * 2],
      codice = ISTAT_traslazione$PRO_COM_T_DT_FI[matches[1] - nrow(ISTAT) * 2]
    ))
    
  }
  
  # Creo un data.table con tutti i nomi e codici aggiornati, associati al
  # nome come è scritto nella tabella dei dati elettorali
  risultato <- rbindlist(lapply(nomi_comuni, cerca_nome))
  risultato$nome_originario <- nomi_comuni
  
  cat("\nTerminato l'aggiornamento dei nomi dei comuni\n")
  
  # Associo i nomi e i codici nuovi alla tabella dei dati elettorali
  return(merge(DT, risultato, by.x = "COMUNE", by.y = "nome_originario"))
  
}

scarica_fonte <- function(
    elezione,
    data_elezione,
    url_fonte,
    files
) {
  tryCatch(
    {
      cat("\nDownload dei dati delll'elezione", elezione,"...\n")
      
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
          DT <- fread(
            file.path(tempdir(), dettagli_file$internal_path), 
            encoding = ifelse(is.null(dettagli_file$encoding), "unknown", dettagli_file$encoding)
          )
          
          # Se necessario applico una funzione custom per pulire il file
          if (!is.null(dettagli_file$funzione)) {
            DT <- dettagli_file$funzione(DT)
          }
          
          # Standardizzo i nomi
          mapply(
            function(nome_standard, nome_non_standard) setnames(DT, nome_non_standard, nome_standard),
            names(dettagli_file$colonne),
            dettagli_file$colonne
          )
          
          # Aggiorno i nomi dei comuni
          DT <- aggiorna_comuni(DT)
          
          if (dettagli_file$tipo == "scrutinio") {
            # Tengo solo le colonne di interesse
            DT <- DT[,c(
              "comune",
              "codice",
              "ELETTORI",
              "LISTA",
              "VOTI"
            )]
            
            astensione <- DT[
              ,
              .(
                VOTI = ELETTORI - sum(VOTI),
                LISTA = "astensione"
              ),
              by = .(
                comune,
                codice,
                ELETTORI
              )
            ]
            DT <- rbind(DT, astensione, fill = TRUE)
            
            dati <<-  rbind(
              dati,
              data.table(
                DATA = as.POSIXct(data_elezione),
                ELEZIONE = elezione,
                COMUNE = DT$comune,
                CODICE_COMUNE = DT$codice,
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
                comune,
                codice,
                LISTA,
                NOME,
                COGNOME
              )
            ]
            
            preferenze <<- rbind(
              preferenze,
              data.table(
                DATA = as.POSIXct(data_elezione),
                ELEZIONE = elezione,
                COMUNE = DT$comune,
                CODICE_COMUNE = DT$codice,
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

dati <- data.table(
  DATA = as.POSIXct(character(0)),
  ELEZIONE = character(0),
  COMUNE = character(0),
  CODICE_COMUNE = character(0),
  LISTA = character(0),
  VOTI = numeric(0)
)

preferenze <- data.table(
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
stopifnot(length(setdiff(dati$CODICE_COMUNE, ISTAT$PRO_COM_T)) == 0)
stopifnot(length(setdiff(preferenze$CODICE_COMUNE, ISTAT$PRO_COM_T)) == 0)

# Aggiungo i codici e i nomi di provincia e regione
dati <- merge(
  dati,
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
dati <- dati[
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

sdmx <- readSDMX("https://esploradati.istat.it/SDMXWS/rest/data/IT1,DF_DCSS_POP_LEGALE_TV,1.0/A../ALL/?detail=full&dimensionAtObservation=TIME_PERIOD")
pop_legale <- as.data.table(as.data.frame(sdmx))

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
setnames(pop_legale, "PRO_COM_T_DT_FI", "CODICE_COMUNE")


# Filtro e salvataggio ----

# Tiene solo i dati dell'Emilia-Romagna
pop_legale <- pop_legale[CODICE_REGIONE == "08"]
dati <- dati[CODICE_REGIONE == "08"]

# Salvo il file
save(dati, pop_legale, preferenze, file = "dati/dati.RData")

# TODO: parallelizzare e/o usare i join di data.table

