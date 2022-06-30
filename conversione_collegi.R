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
