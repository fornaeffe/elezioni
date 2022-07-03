library(readxl)
library(stringr)

totale_seggi <- 232 + 386

#### Caricamento dati di voto camera #### 

liste_comune <- read.csv2(
  "dati_2018/camera-20180304_2.txt",
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
  "dati_2018/WCamPluri.csv",
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

liste_naz <- read_excel("dati_2018/dati_2018.xlsx", "camera_liste")
liste_naz$LISTA <- factor(liste_naz$LISTA, levels = levels(liste_comune$LISTA))

totali_pluri <- read_excel("dati_2018/dati_2018.xlsx", "camera_pluri")

totali_pluri$CIRCOSCRIZIONE <- factor(
  totali_pluri$CIRCOSCRIZIONE, 
  levels = levels(liste_comune$CIRCOSCRIZIONE)
)

totali_pluri$COLLEGIOPLURINOMINALE <- factor(
  totali_pluri$COLLEGIOPLURINOMINALE, 
  levels = levels(liste_comune$COLLEGIOPLURINOMINALE)
)


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

candidati_uni <- aggregate(
  VOTI_CANDIDATO ~ 
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    CANDIDATO +
    DATA_NASCITA, 
  candidati_comune, 
  sum
)

liste_uni <- aggregate(
  VOTI_LISTA ~
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    LISTA
  ,
  liste_comune,
  sum
)

liste_uni <- merge(
  liste_uni,
  unique(liste_comune[, c("COLLEGIOUNINOMINALE", "CANDIDATO", "LISTA")])
)

liste_uni <- merge(
  liste_uni,
  liste_naz[,c("LISTA", "MINORANZA")]
)

liste_uni$CAND_MINORANZA <- liste_uni$MINORANZA &
  !(liste_uni$CANDIDATO %in% liste_uni$CANDIDATO[
    duplicated(liste_uni[, c(
      "CIRCOSCRIZIONE", 
      "COLLEGIOPLURINOMINALE", 
      "COLLEGIOUNINOMINALE",
      "CANDIDATO"
    )])
  ])


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

source("C_83_1_cd.R")

source("C_83_1_e.R")

source("C_83_1_f.R")

source("C_83_1_g.R")

source("C_83_1_h.R")

source("C_83_1_i.R")

source("C_83bis.R")

source("C_84.R")

source("C_85.R")

#### Esportazione risultati ####

liste_naz$CL <- as.character(liste_naz$COALIZIONE)
liste_naz$CL[is.na(liste_naz$CL)] <- 
  as.character(liste_naz$LISTA[is.na(liste_naz$CL)])

liste_pluri <- merge(
  liste_pluri,
  liste_naz[,c("LISTA", "CL")]
)

liste_circ <- merge(
  liste_circ,
  liste_naz[,c("LISTA", "CL")]
)

liste_pluri <- merge(
  liste_pluri,
  ammesse_pluri[,c("COLLEGIOPLURINOMINALE", "LISTA", "ELETTI")],
  all.x = TRUE
)
liste_pluri$ELETTI[is.na(liste_pluri$ELETTI)] <- 0

liste_circ <- merge(
  liste_circ,
  aggregate(
    ELETTI ~ CIRCOSCRIZIONE + LISTA,
    liste_pluri,
    sum
  )
)

liste_naz <- merge(
  liste_naz,
  aggregate(
    ELETTI ~ LISTA,
    liste_circ,
    sum
  )
)

cl_pluri <- aggregate(
  ELETTI ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + CL,
  liste_pluri,
  sum
)

cl_circ <- aggregate(
  ELETTI ~ CIRCOSCRIZIONE + CL,
  cl_pluri,
  sum
)

cl_naz <- aggregate(
  ELETTI ~ CL,
  cl_circ,
  sum
)

liste_pluri <- merge(
  liste_pluri,
  cl_pluri,
  by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "CL"),
  suffixes = c("", "_CL")
)

liste_circ <- merge(
  liste_circ,
  cl_circ,
  by = c("CIRCOSCRIZIONE", "CL"),
  suffixes = c("", "_CL")
)

liste_naz <- merge(
  liste_naz,
  cl_naz,
  by = "CL",
  suffixes = c("", "_CL")
)

write.csv2(
  cl_naz,
  "output/cl_naz.csv",
  row.names = FALSE
)
write.csv2(
  cl_circ,
  "output/cl_circ.csv",
  row.names = FALSE
)
write.csv2(
  cl_pluri,
  "output/cl_pluri.csv",
  row.names = FALSE
)

write.csv2(
  liste_naz[,c("LISTA", "ELETTI", "CL", "ELETTI_CL")],
  "output/liste_naz.csv",
  row.names = FALSE
)
write.csv2(
  liste_circ[,c("CIRCOSCRIZIONE", "LISTA", "ELETTI", "CL", "ELETTI_CL")],
  "output/liste_circ.csv",
  row.names = FALSE
)
write.csv2(
  liste_pluri[,c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "LISTA", "ELETTI", "CL", "ELETTI_CL")],
  "output/liste_pluri.csv",
  row.names = FALSE
)
