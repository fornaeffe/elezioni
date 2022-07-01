library(readxl)
library(stringr)

camera_seggi <- 232 + 386

#### Caricamento dati di voto camera #### 

liste_comune <- read.csv2(
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

liste_comune$DATA_NASCITA <- as.POSIXct(
  liste_comune$DATA_NASCITA, 
  format="%d/%m/%Y %H:%M:%S"
)

liste_comune$CANDIDATO <- paste(
  liste_comune$COGNOME, 
  liste_comune$NOME
)

liste_comune$CANDIDATO[liste_comune$CANDIDATO == "CANDIDATO NON PRESENTE "] <- 
  NA

#### Caricamento elenco candidati plurinominale ####

candidati_pluri <- read.csv2(
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

candidati_pluri$CANDIDATO <- str_remove(
  candidati_pluri$CANDIDATO,
  " DETT[AO] .*"
)

candidati_pluri$LISTA[candidati_pluri$LISTA == " +EUROPA"] <-
  "+EUROPA"


candidati_pluri$LISTA <- factor(
  candidati_pluri$LISTA, 
  levels = levels(liste_comune$LISTA)
)

candidati_pluri$CIRCOSCRIZIONE <- factor(
  candidati_pluri$CIRCOSCRIZIONE, 
  levels = levels(liste_comune$CIRCOSCRIZIONE)
)

candidati_pluri$COLLEGIOPLURINOMINALE <- factor(
  candidati_pluri$COLLEGIOPLURINOMINALE, 
  levels = levels(liste_comune$COLLEGIOPLURINOMINALE)
)

candidati_pluri$CANDIDATO <- trimws(candidati_pluri$CANDIDATO)

#### Unico factor per i candidati ####

candidati <- factor(unique(c(
  liste_comune$CANDIDATO, 
  candidati_pluri$CANDIDATO
)))

liste_comune$CANDIDATO <- factor(liste_comune$CANDIDATO, levels = levels(candidati))
candidati_pluri$CANDIDATO <- factor(
  candidati_pluri$CANDIDATO,
  levels = levels(candidati)
)

#### Carico i dati dai fogli excel ####

coalizioni <- read_excel("coalizioni.xlsx")

camera_pluri <- read_excel("camera_pluri.xlsx")

#### Separo i dati della Val d'Aosta ####

liste_comune_AOSTA <- liste_comune[liste_comune$CIRCOSCRIZIONE == "AOSTA",]
liste_comune <- liste_comune[!(liste_comune$CIRCOSCRIZIONE == "AOSTA"),]

#### Creo altri data frame ####

candidati_comune <- unique(
  liste_comune[
    ,
    c(
      "CIRCOSCRIZIONE", 
      "COLLEGIOPLURINOMINALE", 
      "COLLEGIOUNINOMINALE", 
      "COMUNE", 
      "CANDIDATO",
      "DATA_NASCITA",
      "VOTI_CANDIDATO"
    )
  ]
)

liste_comune <- liste_comune[
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



if (
  sum(
    table(
      candidati_comune$CANDIDATO, 
      candidati_comune$COMUNE
    ) > 1
  ) > 0
) stop("Almeno un candidato uninominale ha voti diversi nello stesso comune")


### Inizio applicazione della legge ###

source("C_77_1_ab.R")

source("C_77_1_c.R")

source("C_77_1_def.R")

source("C_77_1_gh.R")

source("C_77_1_il.R")

source("C_83_1_ab.R")
