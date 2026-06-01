#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/politiche/generated_adapter.json"

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

prepare_ramo_source <- function(ramo) {
  list(
    uni_liste_sim = select_frame(voti[[ramo]]$uni_liste_sim, c(
      "SIM",
      "UNI_COD",
      "LISTA",
      "VOTI_LISTA_SIM",
      "PLURI_COD",
      "CIRC_COD",
      "CANDIDATO_ID",
      "CAND_MINORANZA",
      "MINORANZA"
    )),
    candidati_uni_sim = select_frame(voti[[ramo]]$candidati_uni_sim, c(
      "SIM",
      "UNI_COD",
      "CANDIDATO_ID",
      "DATA_NASCITA",
      "VOTI_CANDIDATO",
      "PLURI_COD",
      "CIRC_COD"
    )),
    candidati_pluri_sim = select_frame(candidati[[ramo]]$candidati_pluri_sim, c(
      "SIM",
      "LISTA",
      "PLURI_COD",
      "NUMERO_CANDIDATO",
      "CANDIDATO_ID",
      "CIRC_COD"
    )),
    uni = select_frame(dati_collegi[[ramo]]$uni, c(
      "CIRC_COD",
      "PLURI_COD",
      "UNI_COD"
    )),
    pluri = select_frame(dati_collegi[[ramo]]$pluri, c(
      "CIRC_COD",
      "PLURI_COD",
      "SEGGI_PLURI"
    )),
    liste = select_frame(parametri_input$liste, c(
      "LISTA",
      "COALIZIONE"
    )),
    candidati_pluri_template = select_frame(dati_candidati[[ramo]]$candidati_pluri, c(
      "LISTA",
      "PLURI_COD",
      "CIRC_COD",
      "MINORANZA"
    ))
  )
}

load("dati/debug_scrutinio.RData")

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "dati/debug_scrutinio.RData",
    purpose = paste(
      "Generated politics adapter fixture for the TypeScript migration.",
      "It stores the R-style generated vote/candidate tables consumed by",
      "esegui_scrutini_politiche(), before direct scrutiny inputs are split by simulation."
    )
  ),
  rami = list(
    camera = prepare_ramo_source("camera"),
    senato = prepare_ramo_source("senato")
  )
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

cat("Wrote politics generated adapter fixture:", output_path, "\n")
