# Importo la base di dati della commissione
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

base_dati$SP20_DEN[base_dati$DEN_REG20 == "Trentino-Alto Adige"] <- 
  "Trentino-Alto Adige/Südtirol - P01"

# Preparo le liste
camera <- list()
senato <- list()

#### Camera ####

# Estraggo le circoscrizioni
camera$circoscrizioni <- aggregate(
  POP_2011 ~ CIRCOCAM_20_DEN,
  base_dati,
  sum
)

# Estraggo i collegi plurinominali
camera$collegi_pluri <- aggregate(
  POP_2011 ~ CIRCOCAM_20_DEN + CP20_DEN,
  base_dati,
  sum
)

# Controllo che non ci siano collegi plurinominali duplicati
if (sum(duplicated(camera$collegi_pluri$CP20_DEN)) > 0) warning(
  "Collegi plurinominali per la camera duplicati"
)

camera$collegi_uni <- aggregate(
  POP_2011 ~ CIRCOCAM_20_DEN + CP20_DEN + CU20_DEN,
  base_dati,
  sum
)

# Controllo che non ci siano collegi uninominali duplicati
if (sum(duplicated(camera$collegi_uni$CU20_DEN)) > 0) warning(
  "Collegi plurinominali per la camera duplicati"
)

##### Rinomino le colonne ####
names(camera$circoscrizioni)[1] <- "CIRCOSCRIZIONE"
names(camera$collegi_pluri)[1] <- "CIRCOSCRIZIONE"
names(camera$collegi_pluri)[2] <- "COLLEGIOPLURINOMINALE"
names(camera$collegi_uni)[1] <- "CIRCOSCRIZIONE"
names(camera$collegi_uni)[2] <- "COLLEGIOPLURINOMINALE"
names(camera$collegi_uni)[3] <- "COLLEGIOUNINOMINALE"

##### Assegnazione dei seggi ####

###### Circoscrizione #####

camera$circoscrizioni <- merge(
  camera$circoscrizioni,
  aggregate(
    COLLEGIOUNINOMINALE ~ CIRCOSCRIZIONE,
    camera$collegi_uni,
    length
  )
)
names(camera$circoscrizioni)[
  names(camera$circoscrizioni) == "COLLEGIOUNINOMINALE"
] <- "COLLEGI_UNI"


camera$seggi <- 400 - 8 - 1 # Tolgo i seggi per l'estero e la Val d'Aosta

camera$popolazione <- sum(camera$circoscrizioni$POP_2011)

camera$quoziente <- popolazione / camera$seggi

camera$circoscrizioni$PARTE_INTERA <- 
  camera$circoscrizioni$POP_2011 %/% camera$quoziente

camera$circoscrizioni$RESTO <- 
  camera$circoscrizioni$POP_2011 %% camera$quoziente

camera$da_assegnare <- camera$seggi - sum(camera$circoscrizioni$PARTE_INTERA)

camera$circoscrizioni <- camera$circoscrizioni[order(
  camera$circoscrizioni$RESTO,
  decreasing = TRUE
), ]

camera$circoscrizioni$SEGGIO_DA_RESTO <- FALSE
camera$circoscrizioni$SEGGIO_DA_RESTO[1:camera$da_assegnare] <- TRUE

camera$circoscrizioni$SEGGI <- 
  camera$circoscrizioni$PARTE_INTERA + camera$circoscrizioni$SEGGIO_DA_RESTO

camera$circoscrizioni$QUOZIENTE <- 
  camera$circoscrizioni$POP_2011 / camera$circoscrizioni$SEGGI

###### Collegio plurinominale #####

camera$collegi_pluri <- merge(
  camera$collegi_pluri,
  aggregate(
    COLLEGIOUNINOMINALE ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
    camera$collegi_uni,
    length
  )
)
names(camera$collegi_pluri)[
  names(camera$collegi_pluri) == "COLLEGIOUNINOMINALE"
] <- "COLLEGI_UNI"

camera$collegi_pluri <- merge(
  camera$collegi_pluri,
  camera$circoscrizioni[, c("CIRCOSCRIZIONE", "QUOZIENTE")]
)

camera$collegi_pluri$PARTE_INTERA <-
  camera$collegi_pluri$POP_2011 %/% camera$collegi_pluri$QUOZIENTE

camera$collegi_pluri$RESTO <-
  camera$collegi_pluri$POP_2011 %% camera$collegi_pluri$QUOZIENTE

camera$circoscrizioni <- merge(
  camera$circoscrizioni,
  aggregate(
    PARTE_INTERA ~ CIRCOSCRIZIONE,
    camera$collegi_pluri,
    sum
  ),
  by = "CIRCOSCRIZIONE",
  suffixes = c("", "_PLURI")
)

camera$circoscrizioni$DA_ASSEGNARE <-
  camera$circoscrizioni$SEGGI - camera$circoscrizioni$PARTE_INTERA_PLURI

camera$collegi_pluri <- merge(
  camera$collegi_pluri,
  camera$circoscrizioni[, c("CIRCOSCRIZIONE", "DA_ASSEGNARE")]
)

camera$collegi_pluri <- camera$collegi_pluri[order(
  camera$collegi_pluri$CIRCOSCRIZIONE,
  camera$collegi_pluri$RESTO,
  decreasing = c("FALSE", "TRUE")
), ]

camera$collegi_pluri$ORDINE <- ave(
  camera$collegi_pluri$RESTO,
  camera$collegi_pluri$CIRCOSCRIZIONE,
  FUN = seq_along
)

camera$collegi_pluri$SEGGIO_DA_RESTO <- 
  camera$collegi_pluri$ORDINE <= camera$collegi_pluri$DA_ASSEGNARE

camera$collegi_pluri$SEGGI <- 
  camera$collegi_pluri$PARTE_INTERA + camera$collegi_pluri$SEGGIO_DA_RESTO

camera$collegi_pluri$SEGGI_PLURI <-
  camera$collegi_pluri$SEGGI - camera$collegi_pluri$COLLEGI_UNI




#### Senato ####

# Estraggo le circoscrizioni
senato$circoscrizioni <- aggregate(
  POP_2011 ~ DEN_REG20,
  base_dati,
  sum
)

# Estraggo i collegi plurinominali
senato$collegi_pluri <- aggregate(
  POP_2011 ~ DEN_REG20 + SP20_DEN,
  base_dati,
  sum
)

# Controllo che non ci siano collegi plurinominali duplicati
if (sum(duplicated(senato$collegi_pluri$SP20_DEN[
  !is.na(senato$collegi_pluri$SP20_DEN)
])) > 0) warning(
  "Collegi plurinominali per il senato duplicati"
)

senato$collegi_uni <- aggregate(
  POP_2011 ~ DEN_REG20 + SP20_DEN + SU20_DEN,
  base_dati,
  sum
)

# Controllo che non ci siano collegi uninominali duplicati
if (sum(duplicated(camera$collegi_uni$CU20_DEN)) > 0) warning(
  "Collegi plurinominali per la camera duplicati"
)



##### Rinomino le colonne ####
names(senato$circoscrizioni)[1] <- "CIRCOSCRIZIONE"
names(senato$collegi_pluri)[1] <- "CIRCOSCRIZIONE"
names(senato$collegi_pluri)[2] <- "COLLEGIOPLURINOMINALE"
names(senato$collegi_uni)[1] <- "CIRCOSCRIZIONE"
names(senato$collegi_uni)[2] <- "COLLEGIOPLURINOMINALE"
names(senato$collegi_uni)[3] <- "COLLEGIOUNINOMINALE"

##### Separo i dati del TAA #####

senato$circoscrizioni_TAA <- senato$circoscrizioni[
  senato$circoscrizioni$CIRCOSCRIZIONE == "Trentino-Alto Adige",
]

senato$collegi_pluri_TAA <- senato$collegi_pluri[
  senato$collegi_pluri$CIRCOSCRIZIONE == "Trentino-Alto Adige",
]

senato$collegi_uni_TAA <- senato$collegi_uni[
  senato$collegi_uni$CIRCOSCRIZIONE == "Trentino-Alto Adige",
]

senato$circoscrizioni <- senato$circoscrizioni[
  senato$circoscrizioni$CIRCOSCRIZIONE != "Trentino-Alto Adige",
]

senato$collegi_pluri <- senato$collegi_pluri[
  senato$collegi_pluri$CIRCOSCRIZIONE != "Trentino-Alto Adige",
]

senato$collegi_uni <- senato$collegi_uni[
  senato$collegi_uni$CIRCOSCRIZIONE != "Trentino-Alto Adige",
]



##### Assegnazione dei seggi ####

###### Circoscrizione #####

senato$circoscrizioni <- merge(
  senato$circoscrizioni,
  aggregate(
    COLLEGIOUNINOMINALE ~ CIRCOSCRIZIONE,
    senato$collegi_uni,
    length
  )
)
names(senato$circoscrizioni)[
  names(senato$circoscrizioni) == "COLLEGIOUNINOMINALE"
] <- "COLLEGI_UNI"

# Tolgo i seggi per estero, Val d'Aosta e TAA
senato$seggi <- 200 - 4 - 1 - 6 

senato$popolazione <- sum(senato$circoscrizioni$POP_2011)

senato$quoziente <- popolazione / senato$seggi

senato$circoscrizioni$PARTE_INTERA <- 
  senato$circoscrizioni$POP_2011 %/% senato$quoziente

senato$circoscrizioni$RESTO <- 
  senato$circoscrizioni$POP_2011 %% senato$quoziente

senato$da_assegnare <- senato$seggi - sum(senato$circoscrizioni$PARTE_INTERA)

senato$circoscrizioni <- senato$circoscrizioni[order(
  senato$circoscrizioni$RESTO,
  decreasing = TRUE
), ]

senato$circoscrizioni$SEGGIO_DA_RESTO <- FALSE
senato$circoscrizioni$SEGGIO_DA_RESTO[1:senato$da_assegnare] <- TRUE

senato$circoscrizioni$SEGGI <- 
  senato$circoscrizioni$PARTE_INTERA + senato$circoscrizioni$SEGGIO_DA_RESTO

senato$circoscrizioni$QUOZIENTE <- 
  senato$circoscrizioni$POP_2011 / senato$circoscrizioni$SEGGI

###### Collegio plurinominale #####

senato$collegi_pluri <- merge(
  senato$collegi_pluri,
  aggregate(
    COLLEGIOUNINOMINALE ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
    senato$collegi_uni,
    length
  )
)
names(senato$collegi_pluri)[
  names(senato$collegi_pluri) == "COLLEGIOUNINOMINALE"
] <- "COLLEGI_UNI"

senato$collegi_pluri <- merge(
  senato$collegi_pluri,
  senato$circoscrizioni[, c("CIRCOSCRIZIONE", "QUOZIENTE")]
)

senato$collegi_pluri$PARTE_INTERA <-
  senato$collegi_pluri$POP_2011 %/% senato$collegi_pluri$QUOZIENTE

senato$collegi_pluri$RESTO <-
  senato$collegi_pluri$POP_2011 %% senato$collegi_pluri$QUOZIENTE

senato$circoscrizioni <- merge(
  senato$circoscrizioni,
  aggregate(
    PARTE_INTERA ~ CIRCOSCRIZIONE,
    senato$collegi_pluri,
    sum
  ),
  by = "CIRCOSCRIZIONE",
  suffixes = c("", "_PLURI")
)

senato$circoscrizioni$DA_ASSEGNARE <-
  senato$circoscrizioni$SEGGI - senato$circoscrizioni$PARTE_INTERA_PLURI

senato$collegi_pluri <- merge(
  senato$collegi_pluri,
  senato$circoscrizioni[, c("CIRCOSCRIZIONE", "DA_ASSEGNARE")]
)

senato$collegi_pluri <- senato$collegi_pluri[order(
  senato$collegi_pluri$CIRCOSCRIZIONE,
  senato$collegi_pluri$RESTO,
  decreasing = c("FALSE", "TRUE")
), ]

senato$collegi_pluri$ORDINE <- ave(
  senato$collegi_pluri$RESTO,
  senato$collegi_pluri$CIRCOSCRIZIONE,
  FUN = seq_along
)

senato$collegi_pluri$SEGGIO_DA_RESTO <- 
  senato$collegi_pluri$ORDINE <= senato$collegi_pluri$DA_ASSEGNARE

senato$collegi_pluri$SEGGI <- 
  senato$collegi_pluri$PARTE_INTERA + senato$collegi_pluri$SEGGIO_DA_RESTO

senato$collegi_pluri$SEGGI_PLURI <-
  senato$collegi_pluri$SEGGI - senato$collegi_pluri$COLLEGI_UNI


