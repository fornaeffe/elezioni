library(jsonlite)
library(data.table)

dati <- fromJSON(
  gzfile(
    "data/derived/r26-manual-tests/r26-risultati (1).json.gz",
    open = "rb"
  ),
  simplifyVector = TRUE,
  simplifyDataFrame = TRUE,
  flatten = FALSE
)