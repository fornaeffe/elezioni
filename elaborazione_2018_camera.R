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

candidati_pluri$CANDIDATO <- trimws(candidati_pluri$CANDIDATO)

# Risolvo omonimie

candidati_pluri$CANDIDATO[
  candidati_pluri$CANDIDATO == "RIZZO MARCO" &
    candidati_pluri$LISTA == "10 VOLTE MEGLIO"
] <- "RIZZO MARCO 10"

liste_comune$CANDIDATO[
  liste_comune$CANDIDATO == "RIZZO MARCO" &
    liste_comune$LISTA == "10 VOLTE MEGLIO"
] <- "RIZZO MARCO 10"

candidati_pluri$CANDIDATO[
  candidati_pluri$CANDIDATO == "IORIO ALESSANDRO" &
    candidati_pluri$LISTA == "ITALIA EUROPA INSIEME"
] <- "IORIO ALESSANDRO IEI"

candidati_pluri$CANDIDATO[
  candidati_pluri$CANDIDATO == "SILVESTRI FRANCESCO" &
    candidati_pluri$LISTA == "MOVIMENTO 5 STELLE"
] <- "SILVESTRI FRANCESCO M5S"

liste_comune$CANDIDATO[
  liste_comune$CANDIDATO == "ROSSI MONICA" &
    liste_comune$LISTA == "PARTITO REPUBBLICANO ITALIANO - ALA"
] <- "ROSSI MONICA ALA"

liste_comune$CANDIDATO[
  liste_comune$CANDIDATO == "RUSSO GIOVANNI" &
    liste_comune$LISTA == "IL POPOLO DELLA FAMIGLIA"
] <- "RUSSO GIOVANNI PDF"

liste_comune$CANDIDATO[
  liste_comune$CANDIDATO == "MAURO GIOVANNI" &
    liste_comune$LISTA == "PARTITO REPUBBLICANO ITALIANO - ALA"
] <- "MAURO GIOVANNI ALA"

# Uniformo i factor

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

liste_naz$CL <- liste_naz$COALIZIONE
liste_naz$CL[is.na(liste_naz$CL)] <- liste_naz$LISTA[is.na(liste_naz$CL)]

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
  liste_naz
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

#### Controllo omonimie ####

tab_cand_uni <- table(candidati_uni$CANDIDATO)
if (sum(tab_cand_uni > 1) > 0) {
  print(tab_cand_uni[tab_cand_uni > 1])
  stop("Omonimie nei candidati uninominali")
} 

candidati_e_liste <- unique(candidati_pluri[, c("CANDIDATO", "LISTA")])
tab_candidati <- table(candidati_e_liste$CANDIDATO)
if (sum(tab_candidati > 1) > 0) {
  print(tab_candidati[tab_candidati > 1])
  stop("Omonimie nei candidati plurinominali")
}



candidati_pluri <- merge(
  candidati_pluri,
  liste_naz[, c("LISTA", "CL")]
)

candidati_pluri$OMONIMIA_UNI <- FALSE

for (i in seq_along(candidati_pluri$OMONIMIA_UNI)) {
  candidati_pluri$OMONIMIA_UNI[i] <- 
    candidati_pluri$CANDIDATO[i] %in% liste_uni$CANDIDATO[
      liste_uni$CL != candidati_pluri$CL[i]
    ]
}

if (sum(candidati_pluri$OMONIMIA_UNI) > 0) {
  print(candidati_pluri[candidati_pluri$OMONIMIA_UNI, ])
  stop("Omonimie tra candidati uni e plurinominali")
}


### Inizio applicazione della legge ###

source("C.R")

risultato <- C_scrutinio(
  liste_uni,
  liste_naz,
  candidati_uni,
  candidati_pluri,
  totali_pluri,
  totale_seggi
)
