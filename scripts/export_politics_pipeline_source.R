#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/politiche/pipeline_source_debug.json"

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required")
}

normalize_frame <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (name in names(df)) {
    column <- df[[name]]
    if (inherits(column, "POSIXt")) {
      df[[name]] <- format(column, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    } else if (inherits(column, "Date")) {
      df[[name]] <- format(column, "%Y-%m-%d")
    } else if (inherits(column, "formattable")) {
      df[[name]] <- as.numeric(column)
    } else if (is.factor(column)) {
      df[[name]] <- as.character(column)
    } else if (inherits(column, "AsIs")) {
      df[[name]] <- as.vector(column)
    }
  }
  df
}

select_frame <- function(dt, columns) {
  normalize_frame(data.table::as.data.table(dt)[, ..columns])
}

load("dati/debug_scrutinio.RData")

frazione_uni_in_pluri_value <- if (exists("frazione_uni_in_pluri")) {
  frazione_uni_in_pluri
} else {
  0
}

default_data_nascita <- normalize_frame(data.frame(
  DATA_NASCITA = as.POSIXct("2000-01-01")
))$DATA_NASCITA[[1]]

pipeline_source <- list(
  data_elezione = format(data_elezione, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  simulazioni = 1,
  frazione_uni_in_pluri = frazione_uni_in_pluri_value,
  frazioni_pluricandidature = as.list(frazioni_pluricandidature),
  default_data_nascita = default_data_nascita,
  liste = select_frame(parametri_input$liste, c(
    "LISTA",
    "COALIZIONE",
    "PERCENTUALE",
    "DATA",
    "LOGIT_P",
    "SIGMA_GLOBAL"
  )),
  comuni_liste = select_frame(parametri_input$comuni_liste, c(
    "CODICE_COMUNE",
    "LISTA",
    "DATA",
    "DELTA",
    "SIGMA_DELTA"
  )),
  base_dati = select_frame(dati_collegi$base_dati, c(
    "CODICE_COMUNE",
    "CODITA_20N",
    "ELETTORI",
    "CU20_COD",
    "SU20_COD"
  )),
  camera = list(
    uni = select_frame(dati_collegi$camera$uni, c(
      "CIRC_COD",
      "PLURI_COD",
      "UNI_COD"
    )),
    pluri = select_frame(dati_collegi$camera$pluri, c(
      "CIRC_COD",
      "PLURI_COD",
      "SEGGI_PLURI"
    )),
    candidati_uni = select_frame(dati_candidati$camera$candidati_uni, c(
      "COALIZIONE",
      "UNI_COD",
      "LISTA_MINORANZA",
      "CANDIDATO_ID",
      "DATA_NASCITA"
    )),
    candidati_pluri = select_frame(dati_candidati$camera$candidati_pluri, c(
      "CIRC_COD",
      "LISTA",
      "PLURI_COD",
      "NUMERO_CANDIDATO",
      "MINORANZA",
      "CANDIDATO_ID",
      "DATA_NASCITA"
    ))
  ),
  senato = list(
    uni = select_frame(dati_collegi$senato$uni, c(
      "CIRC_COD",
      "PLURI_COD",
      "UNI_COD"
    )),
    pluri = select_frame(dati_collegi$senato$pluri, c(
      "CIRC_COD",
      "PLURI_COD",
      "SEGGI_PLURI"
    )),
    candidati_uni = select_frame(dati_candidati$senato$candidati_uni, c(
      "COALIZIONE",
      "UNI_COD",
      "LISTA_MINORANZA",
      "CANDIDATO_ID",
      "DATA_NASCITA"
    )),
    candidati_pluri = select_frame(dati_candidati$senato$candidati_pluri, c(
      "CIRC_COD",
      "LISTA",
      "PLURI_COD",
      "NUMERO_CANDIDATO",
      "MINORANZA",
      "CANDIDATO_ID",
      "DATA_NASCITA"
    ))
  )
)

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "dati/debug_scrutinio.RData",
    purpose = paste(
      "Compact real politics generation-source snapshot for TypeScript",
      "pipeline smoke tests. The default simulation count is one and can be",
      "overridden by tests or worker code."
    )
  ),
  source = pipeline_source
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  fixture,
  path = output_path,
  dataframe = "rows",
  null = "null",
  na = "null",
  auto_unbox = TRUE,
  digits = NA,
  pretty = FALSE
)

cat("Wrote politics pipeline source fixture:", output_path, "\n")
