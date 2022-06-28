library(readxl)
library(stringr)

camera_seggi <- 232 + 386

#### Caricamento dati di voto camera #### 

camera <- read.csv2(
  "camera-20180304_2.txt",
  colClasses = c(
    CIRCOSCRIZIONE = "factor",
    COLLEGIOPLURINOMINALE = "factor",
    COLLEGIOUNINOMINALE = "factor",
    COMUNE = "factor",
    SESSO = "factor",
    LISTA = "factor"
  ),
  fileEncoding = "utf-8"
)

camera$DATA_NASCITA <- as.POSIXct(
  camera$DATA_NASCITA, 
  format="%d/%m/%Y %H:%M:%S"
)

camera$CANDIDATO <- paste(
  camera$COGNOME, 
  camera$NOME
)


#### Caricamento elenco candidati plurinominale ####

camera_candidati_pluri <- read.csv2(
  "WCamPluri.csv",
  col.names = c(
    "LISTA",
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "NUMERO",
    "CANDIDATO"
  ),
  fileEncoding = "utf-8"
)

camera_candidati_pluri$CANDIDATO <- str_remove(
  camera_candidati_pluri$CANDIDATO,
  " DETT[AO] .*"
)

camera_candidati_pluri$LISTA[camera_candidati_pluri$LISTA == " +EUROPA"] <-
  "+EUROPA"


camera_candidati_pluri$LISTA <- factor(
  camera_candidati_pluri$LISTA, 
  levels = levels(camera$LISTA)
)

camera_candidati_pluri$CIRCOSCRIZIONE <- factor(
  camera_candidati_pluri$CIRCOSCRIZIONE, 
  levels = levels(camera$CIRCOSCRIZIONE)
)

camera_candidati_pluri$COLLEGIOPLURINOMINALE <- factor(
  camera_candidati_pluri$COLLEGIOPLURINOMINALE, 
  levels = levels(camera$COLLEGIOPLURINOMINALE)
)

camera_candidati_pluri$CANDIDATO <- trimws(camera_candidati_pluri$CANDIDATO)

#### Unico factor per i candidati ####

candidati <- factor(unique(c(
  camera$CANDIDATO, 
  camera_candidati_pluri$CANDIDATO
)))

camera$CANDIDATO <- factor(camera$CANDIDATO, levels = levels(candidati))
camera_candidati_pluri$CANDIDATO <- factor(
  camera_candidati_pluri$CANDIDATO,
  levels = levels(candidati)
)

#### Carico i dati dai fogli excel ####

coalizioni <- read_excel("coalizioni.xlsx")

camera_pluri <- read_excel("camera_pluri.xlsx")

#### Creo altri data frame ####


camera_candidati_uni <- unique(
  camera[
    ,
    c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "DATA_NASCITA"
    )
  ]
)

camera_voti_lista_per_comune <- camera[
  ,
  c(
    "CIRCOSCRIZIONE", 
    "COLLEGIOPLURINOMINALE", 
    "COLLEGIOUNINOMINALE", 
    "COMUNE",
    "CANDIDATO", 
    "LISTA",
    "VOTI_LISTA"
  )
]

camera_voti_candidato_per_comune <- unique(
  camera[
    ,
    c(
      "CIRCOSCRIZIONE", 
      "COLLEGIOPLURINOMINALE", 
      "COLLEGIOUNINOMINALE", 
      "COMUNE", 
      "CANDIDATO",
      "VOTI_CANDIDATO"
    )
  ]
)

if (
  sum(
    table(
      camera_voti_candidato_per_comune$CANDIDATO, 
      camera_voti_candidato_per_comune$COMUNE
    ) > 1
  ) > 0
) stop("Almeno un candidato uninominale ha voti diversi nello stesso comune")



dati <- list(
  camera_pluri = camera_pluri,
  camera_seggi = camera_seggi,
  camera_coalizioni = coalizioni,
  camera_candidati_uni = camera_candidati_uni,
  camera_candidati_pluri = camera_candidati_pluri,
  camera_voti_lista_per_comune = camera_voti_lista_per_comune,
  camera_voti_candidato_per_comune = camera_voti_candidato_per_comune
)

save(dati, file = "dati.RData")
