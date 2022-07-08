library(readxl)
library(stringr)

totale_seggi <- 116 + 193

#### Caricamento dati di voto camera ####

liste_comune <- read.csv2(
  "dati_2018/senato-20180304.txt",
  colClasses = c(
    REGIONE = "factor",
    COLLEGIOPLURINOMINALE = "factor",
    COLLEGIOUNINOMINALE = "factor",
    COMUNE = "factor",
    SESSO = "factor",
    LISTA = "factor"
  ),
  fileEncoding = "utf-8"
)

names(liste_comune)[names(liste_comune) == "REGIONE"] <- "CIRCOSCRIZIONE"


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

levels(liste_comune$COLLEGIOPLURINOMINALE)[
  levels(liste_comune$COLLEGIOPLURINOMINALE) == 
    "TRENTINO-ALTO ADIGE/S_DTIROL - 01"
] <- "TRENTINO-ALTO ADIGE/SÜDTIROL - 01"


#### Caricamento elenco candidati plurinominale ####

candidati_pluri <- read.csv2(
  "dati_2018/WSenPluri.csv",
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

candidati_pluri$CANDIDATO <- trimws(candidati_pluri$CANDIDATO)

candidati_pluri$LISTA[candidati_pluri$LISTA == " +EUROPA"] <-
  "+EUROPA"

# Risolvo le omonimie

candidati_pluri$CANDIDATO[
  candidati_pluri$CANDIDATO == "COLOMBO DANIELA" &
    candidati_pluri$LISTA == "+EUROPA"
] <- "COLOMBO DANIELA +EUROPA"

candidati_pluri$CANDIDATO[
  candidati_pluri$CANDIDATO == "STACCIOLI MARINA" &
    candidati_pluri$LISTA == "FRATELLI D'ITALIA CON GIORGIA MELONI"
] <- "STACCIOLI MARINA FI"

candidati_pluri$CANDIDATO[
  candidati_pluri$CANDIDATO == "SANTANGELO VINCENZO" &
    candidati_pluri$LISTA == "MOVIMENTO 5 STELLE"
] <- "SANTANGELO VINCENZO M5S"

candidati_pluri$CANDIDATO[
  candidati_pluri$CANDIDATO == "PISANI GIUSEPPE" &
    candidati_pluri$LISTA == "PARTITO VALORE UMANO"
] <- "PISANI GIUSEPPE PVU"

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

liste_naz <- read_excel("dati_2018/dati_2018.xlsx", "senato_liste")

liste_naz$CL <- liste_naz$COALIZIONE
liste_naz$CL[is.na(liste_naz$CL)] <- liste_naz$LISTA[is.na(liste_naz$CL)]

liste_naz$LISTA <- factor(liste_naz$LISTA, levels = levels(liste_comune$LISTA))



totali_pluri <- read_excel("dati_2018/dati_2018.xlsx", "senato_pluri")

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

if (sum(table(candidati_uni$CANDIDATO) > 1) > 0) stop(
  "Omonimie nei candidati uninominali"
)

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

source("scrutinio.R")

risultato <- Scrutinio(
  "Senato",
  liste_uni,
  liste_naz,
  candidati_uni,
  candidati_pluri,
  totali_pluri,
  totale_seggi
)

#### Confronto con i dati di github.com/ondata/elezionipolitiche2018 ####

senato_geopolitico_italia <- read.csv("dati_2018/senato_geopolitico_italia.txt")
scrutiniSI_p <- read.csv("dati_2018/scrutiniSI_p.txt")
scrutiniSI_p <- scrutiniSI_p[scrutiniSI_p$tipo_riga == "LI", ]
scrutiniSI_p <- merge(
  scrutiniSI_p,
  senato_geopolitico_italia,
  by.x = "codice",
  by.y = "id",
  all.x = TRUE
)

if (sum(!(
  scrutiniSI_p$nome %in% levels(risultato$liste_pluri$COLLEGIOPLURINOMINALE)
)) > 0) {
  print(scrutiniSI_p[
    !(scrutiniSI_p$nome %in% levels(risultato$liste_pluri$COLLEGIOPLURINOMINALE)),
  ])
  stop("I collegi plurinominali non combaciano")
}


if (sum(!(
  scrutiniSI_p$descr_lista %in% levels(risultato$liste_pluri$LISTA)
)) > 0) {
  print(scrutiniSI_p[
    !(scrutiniSI_p$descr_lista %in% levels(risultato$liste_pluri$LISTA)),
  ])
  stop("Le liste non combaciano")
}

confronto_pluri <- merge(
  risultato$liste_pluri,
  scrutiniSI_p[, c("nome", "descr_lista", "seggi")],
  by.x = c("COLLEGIOPLURINOMINALE", "LISTA"),
  by.y = c("nome", "descr_lista")
)

confronto_pluri$seggi[confronto_pluri$seggi == "-"] <- 0
confronto_pluri$seggi <- as.numeric(confronto_pluri$seggi)

confronto_pluri$CORRISPONDE <- confronto_pluri$ELETTI == confronto_pluri$seggi
print(confronto_pluri[!confronto_pluri$CORRISPONDE, ])

