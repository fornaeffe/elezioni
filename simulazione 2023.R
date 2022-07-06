#### Carico librerie e script ####

library(readxl)
source("C.R")

#### Caricamento dati ####



liste_naz <- read_xlsx("dati_2023/dati_2023.xlsx")
liste_naz$CL <- as.character(liste_naz$COALIZIONE)
liste_naz$CL[is.na(liste_naz$CL)] <- 
  as.character(liste_naz$LISTA[is.na(liste_naz$CL)])




load("dati_collegi/collegi.RData")

load("dati_2018/C_liste_uni.RData")
load("dati_2018/C_candidati_pluri.RData")

names(camera$collegi_pluri)[names(camera$collegi_pluri) == "SEGGI_PLURI"] <-
  "SEGGI"

#### Calcolo variabilità elezioni 2018 ####

liste_uni_2018 <- liste_uni
candidati_pluri_2018 <- candidati_pluri

liste_pluri_2018 <- aggregate(
  VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
  liste_uni_2018,
  sum
)

liste_circ_2018 <- aggregate(
  VOTI_LISTA ~ CIRCOSCRIZIONE + LISTA,
  liste_uni_2018,
  sum
)

liste_naz_2018 <- aggregate(
  VOTI_LISTA ~ LISTA,
  liste_uni_2018,
  sum
)

liste_circ_2018 <- merge(
  liste_circ_2018,
  aggregate(
    VOTI_LISTA ~ CIRCOSCRIZIONE,
    liste_circ_2018,
    sum
  ),
  by = "CIRCOSCRIZIONE",
  suffixes = c("", "_TOT")
)

liste_pluri_2018 <- merge(
  liste_pluri_2018,
  aggregate(
    VOTI_LISTA ~ COLLEGIOPLURINOMINALE,
    liste_pluri_2018,
    sum
  ),
  by = "COLLEGIOPLURINOMINALE",
  suffixes = c("", "_TOT")
)

liste_uni_2018 <- merge(
  liste_uni_2018,
  aggregate(
    VOTI_LISTA ~ COLLEGIOUNINOMINALE,
    liste_uni_2018,
    sum
  ),
  by = "COLLEGIOUNINOMINALE",
  suffixes = c("", "_TOT")
)

liste_naz_2018$PERCENTUALE <- 
  liste_naz_2018$VOTI_LISTA / sum(liste_naz_2018$VOTI_LISTA)

liste_circ_2018$PERCENTUALE <-
  liste_circ_2018$VOTI_LISTA / liste_circ_2018$VOTI_LISTA_TOT

liste_pluri_2018$PERCENTUALE <-
  liste_pluri_2018$VOTI_LISTA / liste_pluri_2018$VOTI_LISTA_TOT

liste_uni_2018$PERCENTUALE <-
  liste_uni_2018$VOTI_LISTA / liste_uni_2018$VOTI_LISTA_TOT

liste_pluri_2018 <- merge(
  liste_pluri_2018,
  aggregate(
    PERCENTUALE ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
    liste_uni_2018,
    function(x) sd(qlogis(x))
  ),
  by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "LISTA"),
  suffixes = c("", "_SD")
)

liste_circ_2018 <- merge(
  liste_circ_2018,
  aggregate(
    PERCENTUALE ~ CIRCOSCRIZIONE + LISTA,
    liste_pluri_2018,
    function(x) sd(qlogis(x))
  ),
  by = c("CIRCOSCRIZIONE", "LISTA"),
  suffixes = c("", "_SD")
)

liste_naz_2018 <- merge(
  liste_naz_2018,
  aggregate(
    PERCENTUALE ~ LISTA,
    liste_circ_2018,
    function(x) sd(qlogis(x))
  ),
  by = "LISTA",
  suffixes = c("", "_SD")
)


sd_naz <- median(liste_naz_2018$PERCENTUALE_SD, na.rm = TRUE)
sd_circ <- median(liste_circ_2018$PERCENTUALE_SD, na.rm = TRUE)
sd_pluri <- median(liste_pluri_2018$PERCENTUALE_SD, na.rm = TRUE)

#### Calcolo la percentuale di pluricandidature ####

cand <- c(liste_uni_2018$CANDIDATO, candidati_pluri_2018$CANDIDATO)
prob_pluricand <- sum(
  duplicated(cand)) / length(candidati_pluri_2018$CANDIDATO
) / 10

#### Calcolo il numero massimo di candidati per collegio pluri ####
camera$collegi_pluri$CANDIDATI_MAX <- 
  pmin(4, ceiling(camera$collegi_pluri$SEGGI / 2))

#### Preparo il data frame dei candidati ####
n_cand <- sum(camera$collegi_pluri$CANDIDATI_MAX) + nrow(camera$collegi_uni)
camera$candidati <- data.frame(
  LISTA = rep(liste_naz$LISTA, each = n_cand),
  CL = rep(liste_naz$CL, each = n_cand)
)

camera$candidati$CANDIDATO <- 
  factor(paste(camera$candidati$LISTA, seq_along(camera$candidati$LISTA)))


#### Preparo i data frame delle liste ai diversi livelli ####

camera$liste_circ <- merge(
  camera$circoscrizioni,
  liste_naz[, c("LISTA", "PERCENTUALE")]
)

camera$liste_pluri <- merge(
  camera$collegi_pluri,
  liste_naz[, c("LISTA")]
)

camera$liste_uni <- merge(
  camera$collegi_uni,
  liste_naz[, c("LISTA", "CL")]
)

camera$liste_uni$CAND_MINORANZA <- FALSE
camera$liste_uni$MINORANZA <- FALSE

#### Randomizzo i voti ####

camera$liste_circ$PERC_GREZZA <- plogis(rnorm(
  camera$liste_circ$PERCENTUALE,
  qlogis(camera$liste_circ$PERCENTUALE),
  sd_naz
))

camera$liste_circ$PERCENTUALE_CIRC <- ave(
  camera$liste_circ$PERC_GREZZA,
  camera$liste_circ$CIRCOSCRIZIONE,
  FUN = function(x) x / sum(x)
)

camera$liste_pluri$PERCENTUALE_CIRC <- NULL

camera$liste_pluri <- merge(
  camera$liste_pluri,
  camera$liste_circ[, c("CIRCOSCRIZIONE", "LISTA", "PERCENTUALE_CIRC")]
)

camera$liste_pluri$PERC_GREZZA <- plogis(rnorm(
  camera$liste_pluri$PERCENTUALE,
  qlogis(camera$liste_pluri$PERCENTUALE),
  sd_circ
))

camera$liste_pluri$PERCENTUALE_PLURI <- ave(
  camera$liste_pluri$PERC_GREZZA,
  paste(
    camera$liste_pluri$CIRCOSCRIZIONE,
    camera$liste_pluri$COLLEGIOPLURINOMINALE
  ),
  FUN = function(x) x / sum(x)
)

camera$liste_uni$PERCENTUALE_PLURI <- NULL

camera$liste_uni <- merge(
  camera$liste_uni,
  camera$liste_pluri[, c("COLLEGIOPLURINOMINALE", "LISTA", "PERCENTUALE_PLURI")]
)

camera$liste_uni$PERC_GREZZA <- plogis(rnorm(
  camera$liste_uni$PERCENTUALE,
  qlogis(camera$liste_uni$PERCENTUALE),
  sd_pluri
))

camera$liste_uni$PERCENTUALE_UNI <- ave(
  camera$liste_uni$PERC_GREZZA,
  paste(
    camera$liste_uni$CIRCOSCRIZIONE,
    camera$liste_uni$COLLEGIOPLURINOMINALE,
    camera$liste_uni$COLLEGIOUNINOMINALE
  ),
  FUN = function(x) x / sum(x)
)

camera$liste_uni$VOTI_LISTA <- 
  camera$liste_uni$POP_2011 * camera$liste_uni$PERCENTUALE_UNI


#### Sorteggio i candidati ####

camera$candidati_uni <- unique(camera$liste_uni[, c(
  "CIRCOSCRIZIONE",
  "COLLEGIOPLURINOMINALE",
  "COLLEGIOUNINOMINALE",
  "CL"
)])

camera$candidati$SCELTO_UNI <- FALSE
camera$candidati_uni$CANDIDATO <- 
  factor(NA, levels = levels(camera$candidati$CANDIDATO))


for (i in seq_along(camera$candidati_uni$CL)) {
  candidato <- sample(which(
    camera$candidati$CL == camera$candidati_uni$CL[i] &
      !camera$candidati$SCELTO_UNI
  ), 1)
  
  camera$candidati_uni$CANDIDATO[i] <- camera$candidati$CANDIDATO[candidato]
  camera$candidati$SCELTO_UNI[candidato] <- TRUE
}

if (sum(duplicated(camera$candidati_uni$CANDIDATO)) > 0) stop(
  "Candidati uninominali duplicati"
)


camera$candidati_pluri <- camera$liste_pluri[
  rep(
    seq_along(camera$liste_pluri$CANDIDATI_MAX), 
    camera$liste_pluri$CANDIDATI_MAX
  ),
  c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "LISTA"
  )
]

camera$candidati_pluri$NUMERO <- ave(
  seq_along(camera$candidati_pluri$LISTA),
  paste(
    camera$candidati_pluri$CIRCOSCRIZIONE,
    camera$candidati_pluri$COLLEGIOPLURINOMINALE,
    camera$candidati_pluri$LISTA
  ),
  FUN = seq_along
)

camera$candidati$SCELTO_PLURI <- 0
camera$candidati_pluri$CANDIDATO <- 
  factor(NA, levels = levels(camera$candidati$CANDIDATO))

for (i in seq_along(camera$candidati_pluri$NUMERO)) {
  if (runif(1) < prob_pluricand) {
    papabili <- which(
      camera$candidati$LISTA == camera$candidati_pluri$LISTA[i] &
        camera$candidati$SCELTO_PLURI < 5 &
        (camera$candidati$SCELTO_UNI | camera$candidati$SCELTO_PLURI > 0)
    )
    
    if (length(papabili) > 0) {
      candidato <- sample(papabili, 1)
      
      camera$candidati_pluri$CANDIDATO[i] <- camera$candidati$CANDIDATO[candidato]
      camera$candidati$SCELTO_PLURI[candidato] <- 
        camera$candidati$SCELTO_PLURI[candidato] + 1
      
      next
    }
  }
  
  candidato <- which(
    camera$candidati$LISTA == camera$candidati_pluri$LISTA[i] &
      camera$candidati$SCELTO_PLURI == 0 &
      !camera$candidati$SCELTO_UNI
  )[1]
  
  camera$candidati_pluri$CANDIDATO[i] <- camera$candidati$CANDIDATO[candidato]
  camera$candidati$SCELTO_PLURI[candidato] <- 1
}

# Debug
table(table(camera$candidati_pluri$CANDIDATO))

#### Preparo i data frame per lo scrutinio ####

camera$liste_uni <- merge(
  camera$liste_uni,
  camera$candidati_uni
)


camera$candidati_uni <- merge(
  camera$candidati_uni,
  aggregate(
    VOTI_LISTA ~ 
      CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + COLLEGIOUNINOMINALE + CANDIDATO,
    camera$liste_uni,
    sum
  )
)
names(camera$candidati_uni)[names(camera$candidati_uni) == "VOTI_LISTA"] <-
  "VOTI_CANDIDATO"

camera$candidati_uni$DATA_NASCITA <- as.POSIXct("1990-01-01")

risultato <- C_scrutinio(
  camera$liste_uni[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "CANDIDATO",
    "CAND_MINORANZA",
    "LISTA",
    "MINORANZA",
    "VOTI_LISTA"
  )],
  liste_naz[, c(
    "LISTA",
    "COALIZIONE",
    "MINORANZA"
  )],
  camera$candidati_uni[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "CANDIDATO",
    "DATA_NASCITA",
    "VOTI_CANDIDATO"
  )],
  camera$candidati_pluri[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "LISTA",
    "NUMERO",
    "CANDIDATO"
  )],
  camera$collegi_pluri[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "SEGGI"
  )],
  392
)

print(sum(risultato$liste_pluri$ELETTI))
print(sum(risultato$candidati_pluri$ELETTO))
print(sum(risultato$candidati_uni$ELETTO))

liste_naz <- merge(
  liste_naz,
  aggregate(
    ELETTO ~ LISTA,
    risultato$candidati_pluri,
    sum
  ),
  all.x = TRUE
)
barplot(
  ELETTO ~ LISTA,
  data = liste_naz
)
