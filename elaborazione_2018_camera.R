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

#### Statistiche ###

liste_pluri <- aggregate(
  VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
  liste_uni,
  sum
)

liste_circ <- aggregate(
  VOTI_LISTA ~ CIRCOSCRIZIONE + LISTA,
  liste_uni,
  sum
)

liste_naz <- merge(
  liste_naz, aggregate(
    VOTI_LISTA ~ LISTA,
    liste_uni,
    sum
  ),
  all.x = TRUE
)

liste_circ <- merge(
  liste_circ,
  aggregate(
    VOTI_LISTA ~ CIRCOSCRIZIONE,
    liste_circ,
    sum
  ),
  by = "CIRCOSCRIZIONE",
  suffixes = c("", "_TOT")
)

liste_pluri <- merge(
  liste_pluri,
  aggregate(
    VOTI_LISTA ~ COLLEGIOPLURINOMINALE,
    liste_pluri,
    sum
  ),
  by = "COLLEGIOPLURINOMINALE",
  suffixes = c("", "_TOT")
)

liste_uni <- merge(
  liste_uni,
  aggregate(
    VOTI_LISTA ~ COLLEGIOUNINOMINALE,
    liste_uni,
    sum
  ),
  by = "COLLEGIOUNINOMINALE",
  suffixes = c("", "_TOT")
)

liste_naz$PERCENTUALE <- 
  liste_naz$VOTI_LISTA / sum(liste_naz$VOTI_LISTA)

liste_circ$PERCENTUALE <-
  liste_circ$VOTI_LISTA / liste_circ$VOTI_LISTA_TOT

liste_pluri$PERCENTUALE <-
  liste_pluri$VOTI_LISTA / liste_pluri$VOTI_LISTA_TOT

liste_uni$PERCENTUALE <-
  liste_uni$VOTI_LISTA / liste_uni$VOTI_LISTA_TOT

liste_pluri <- merge(
  liste_pluri,
  aggregate(
    PERCENTUALE ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
    liste_uni,
    function(x) sd(qlogis(x))
  ),
  by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "LISTA"),
  suffixes = c("", "_SD")
)

liste_circ <- merge(
  liste_circ,
  aggregate(
    PERCENTUALE ~ CIRCOSCRIZIONE + LISTA,
    liste_pluri,
    function(x) sd(qlogis(x))
  ),
  by = c("CIRCOSCRIZIONE", "LISTA"),
  suffixes = c("", "_SD")
)

liste_naz <- merge(
  liste_naz,
  aggregate(
    PERCENTUALE ~ LISTA,
    liste_circ,
    function(x) sd(qlogis(x))
  ),
  by = "LISTA",
  suffixes = c("", "_SD"),
  all.x = TRUE
)


cat("Deviazione standard mediana del logit della percentuale di circoscrizione:\n") 
print(median(liste_naz$PERCENTUALE_SD, na.rm = TRUE))
cat("Deviazione standard mediana del logit della percentuale di collegio plurinominale:\n") 
print(median(liste_circ$PERCENTUALE_SD, na.rm = TRUE))
cat("Deviazione standard mediana del logit della percentuale di collegio uninominale:\n")
print(median(liste_pluri$PERCENTUALE_SD, na.rm = TRUE))

#### Calcolo la percentuale di pluricandidature ####
cat(
  "Frazione di candidature (oltre la prima), nei collegi plurinominali,",
  " di persone già candidate in altri collegi uni o plurinominali\n",
  sep = ""
)
print(sapply(
  as.character(liste_naz$LISTA),
  function(l) {
    sum(duplicated(c(
      liste_uni$CANDIDATO[liste_uni$LISTA == l],
      candidati_pluri$CANDIDATO[candidati_pluri$LISTA == l]
    ))) /
      length(candidati_pluri$CANDIDATO[candidati_pluri$LISTA == l])
  }
))



#### Inizio applicazione della legge ####

source("C.R")

risultato <- C_scrutinio(
  liste_uni,
  liste_naz,
  candidati_uni,
  candidati_pluri,
  totali_pluri,
  totale_seggi
)

#### Confronto con i dati di github.com/ondata/elezionipolitiche2018 ####

camera_geopolitico_italia <- read.csv("dati_2018/camera_geopolitico_italia.txt")
scrutiniCI_p <- read.csv("dati_2018/scrutiniCI_p.txt")
scrutiniCI_p <- scrutiniCI_p[scrutiniCI_p$tipo_riga == "LI", ]
scrutiniCI_p <- merge(
  scrutiniCI_p,
  camera_geopolitico_italia,
  by.x = "codice",
  by.y = "id",
  all.x = TRUE
)

if (sum(!(
  scrutiniCI_p$nome %in% levels(risultato$liste_pluri$COLLEGIOPLURINOMINALE)
)) > 0) {
  print(scrutiniCI_p[
    !(scrutiniCI_p$nome %in% levels(risultato$liste_pluri$COLLEGIOPLURINOMINALE)),
  ])
  stop("I collegi plurinominali non combaciano")
}


if (sum(!(
  scrutiniCI_p$descr_lista %in% levels(risultato$liste_pluri$LISTA)
)) > 0) {
  print(scrutiniCI_p[
    !(scrutiniCI_p$descr_lista %in% levels(risultato$liste_pluri$LISTA)),
  ])
  stop("Le liste non combaciano")
}

confronto_pluri <- merge(
  risultato$liste_pluri,
  scrutiniCI_p[, c("nome", "descr_lista", "seggi")],
  by.x = c("COLLEGIOPLURINOMINALE", "LISTA"),
  by.y = c("nome", "descr_lista")
)

confronto_pluri$seggi[confronto_pluri$seggi == "-"] <- 0
confronto_pluri$seggi <- as.numeric(confronto_pluri$seggi)

confronto_pluri$CORRISPONDE <- confronto_pluri$ELETTI == confronto_pluri$seggi
confronto_pluri[!confronto_pluri$CORRISPONDE, ]
