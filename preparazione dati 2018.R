camera <- read.csv2(
  "camera-20180304.txt",
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
coalizioni <- read.csv2("coalizioni.csv")
camera <- merge(camera, coalizioni)

camera$DATA_NASCITA <- as.POSIXct(
  camera$DATA_NASCITA, 
  format="%d/%m/%Y %H:%M:%S"
)

camera$CANDIDATO <- factor(
  paste(
    camera$COGNOME, 
    camera$NOME, 
    camera$DATA_NASCITA
  )
)

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
    "COALIZIONE", 
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
      "COALIZIONE", 
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
  camera_coalizioni = coalizioni,
  camera_candidati_uni = camera_candidati_uni,
  camera_voti_lista_per_comune = camera_voti_lista_per_comune,
  camera_voti_candidato_per_comune = camera_voti_candidato_per_comune
)
