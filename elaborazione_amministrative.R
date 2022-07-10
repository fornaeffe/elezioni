library(data.table)
library(readxl)
library(stringr)
library(rstan)

#### Impostazioni ####
scenario <- "01"
campione_sondaggio <- 1000

#### Importazione dati ####

##### Base dati #####

base_dati <- read.csv2(
  "dati_collegi/BaseDati_Proposta_Commissione.csv",
  fileEncoding = "utf-8",
  colClasses = c(
    CIRCOCAM_20_DEN = "factor",
    CP20_DEN = "factor",
    CU20_DEN = "factor"
  ),
  na.strings = ""
)

base_dati$DEN_PRO_CM20 <- toupper(base_dati$DEN_PRO_CM20)
base_dati$DEN_COM20 <- toupper(base_dati$DEN_COM20)
province <- unique(base_dati$DEN_PRO_CM20)
comuni <- unique(base_dati[, c("DEN_PRO_CM20", "DEN_COM20")])


##### Politiche #####

camera <- read.csv2(
  "dati_2018/camera-20180304_2.txt",
  fileEncoding = "utf-8"
)

camera$PROV_TEMP <- str_remove(camera$COLLEGIOUNINOMINALE, "\\A[0-9]{2} (- )?")
camera$PROV_TEMP <- str_remove(camera$PROV_TEMP, " - .*\\Z")
camera$PROV_TEMP <- str_remove(camera$PROV_TEMP, " AREA STATISTICA .*\\Z")

camera <- merge(
  camera,
  comuni,
  by.x = "PROV_TEMP",
  by.y = "DEN_COM20",
  all.x = TRUE
)

camera$PROVINCIA <- camera$DEN_PRO_CM20
camera$DEN_PRO_CM20 <- NULL

camera$PROVINCIA[camera$PROV_TEMP == ""] <- "AOSTA"
camera$PROVINCIA[camera$PROV_TEMP == "BOLZANO/BOZEN"] <- "BOLZANO"
camera$PROVINCIA[camera$PROV_TEMP == "BRESSANONE/BRIXEN"] <- "BOLZANO"
camera$PROVINCIA[camera$PROV_TEMP == "CANT+"] <- "COMO"
camera$PROVINCIA[camera$PROV_TEMP == "CORIGLIANO CALABRO"] <- "COSENZA"
camera$PROVINCIA[camera$PROV_TEMP == "FORL¦"] <- "FORLI'-CESENA"
camera$PROVINCIA[camera$PROV_TEMP == "MERANO/MERAN"] <- "BOLZANO"
camera$PROVINCIA[camera$PROV_TEMP == "NARDÊ"] <- "LECCE"
camera$PROVINCIA[camera$PROV_TEMP == "PATERNÊ"] <- "CATANIA"
camera$PROVINCIA[camera$PROV_TEMP == "SAN DONA' DI PIAVE"] <- "VENEZIA"

# Checks
setdiff(unique(camera$PROVINCIA), province)
sum(is.na(camera$PROVINCIA))

##### Amministrative #####

lista_files <- list.files("dati_amministrative")

lista_dataframes <- lapply(
  paste0("dati_amministrative/", lista_files),
  read.csv2,
  fileEncoding = "utf-8"
)

lista_dataframes <- mapply(
  function(df, nome_file) {
    names(df) <- toupper(names(df))
    df$ANNO <- substr(nome_file, 11, 14)
    df$MESE <- substr(nome_file, 15, 16)
    df
  },
  df = lista_dataframes,
  nome_file = lista_files,
  SIMPLIFY = FALSE
)

dati <- rbindlist(lista_dataframes)

dati$PROVINCIA[dati$PROVINCIA == "REGGIO NELL' EMILIA"] <- "REGGIO NELL'EMILIA"

# Checks
setdiff(unique(dati$PROVINCIA), province)

# Questo è servito per esportare i nomi delle liste
# write.csv2(
#   dati[!duplicated(dati$LISTA), ],
#   "output/liste.csv",
#   fileEncoding = "utf-8"
# )

##### Corrispondenza liste - aree #####

liste <- read_xlsx("sandbox/liste.xlsx", "aree")
liste <- liste[
  !duplicated(liste$LISTA) & !is.na(liste$LISTA) & !is.na(liste$AREA)
  ,]

##### Ultimo sondaggio #####
liste_naz <- read_xlsx(paste0("dati_2023/", scenario, ".xlsx"))


#### Unione dataframes ####

camera$VOTAZIONE <- "politiche 2018"
dati$VOTAZIONE <- paste("regionali", dati$ANNO, dati$MESE)

camera$AREA <- factor(camera$LISTA, levels = liste$LISTA, labels = liste$AREA)
dati$AREA <- factor(dati$LISTA, levels = liste$LISTA, labels = liste$AREA)

df <- rbind(
  aggregate(
    VOTI_LISTA ~ PROVINCIA + AREA + VOTAZIONE,
    camera,
    sum
  ),
  aggregate(
    VOTI_LISTA ~ PROVINCIA + AREA + VOTAZIONE,
    dati,
    sum
  )
)

df$PROVINCIA <- factor(df$PROVINCIA)

tabella <- reshape(df, direction = "wide", idvar = c("PROVINCIA", "VOTAZIONE"), timevar = "AREA")

tabella[is.na(tabella)] <- 0

pop_province <- aggregate(
  POP_2011 ~ DEN_PRO_CM20,
  base_dati,
  sum
)

pop_province <- pop_province[pop_province$DEN_PRO_CM20 %in% levels(tabella$PROVINCIA), ]

pop_province$pop <- pop_province$POP_2011 / sum(pop_province$POP_2011)

liste_naz$AREA <- factor(liste_naz$AREA, levels = levels(df$AREA))

liste_naz$y0 <- round(liste_naz$PERCENTUALE * campione_sondaggio)

aree_naz <- aggregate(
  y0 ~ AREA,
  liste_naz,
  sum
)

tabella <- tabella[order(
  as.integer(tabella$PROVINCIA)
), ]

iniziali <- t(apply(
  tabella[, (ncol(tabella) - length(levels(df$AREA)) + 1):ncol(tabella)],
  1,
  function(x) pmax(-5, log(x / sum(x)))
))

iniziali2 <- iniziali[!duplicated(tabella$PROVINCIA),]

fit <- stan(
  "voto.stan",
  data = list(
    N = length(levels(tabella$PROVINCIA)),
    L = nrow(tabella),
    K = length(levels(df$AREA)),
    provincia = as.integer(tabella$PROVINCIA),
    y = tabella[, (ncol(tabella) - length(levels(df$AREA)) + 1):ncol(tabella)],
    pop = pop_province$pop,
    y0 = aree_naz$y0
  ),
  init = function() list(
    THETA = log( aree_naz$y0 / campione_sondaggio ),
    Theta = iniziali2,
    theta0 = iniziali2,
    theta = iniziali
  ),
  cores = 4
)

library(shinystan)
launch_shinystan(fit)
