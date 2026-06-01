#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/politiche/vote_preparation.json"

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

prepare_raw_votes <- function(ramo) {
  raw <- data.table::copy(voti[[ramo]]$uni_liste_sim[, .(
    SIM,
    UNI_COD,
    LISTA,
    VOTI_LISTA_SIM
  )])

  probe <- data.table::copy(voti[[ramo]]$uni_liste_sim[1])
  raw <- data.table::rbindlist(list(
    raw,
    data.table::data.table(
      SIM = probe$SIM,
      UNI_COD = probe$UNI_COD,
      LISTA = "astensione",
      VOTI_LISTA_SIM = 123
    )
  ), use.names = TRUE)

  validi <- unique(dati_candidati[[ramo]]$candidati_pluri[, .(
    CIRC_COD,
    PLURI_COD,
    LISTA
  )])

  liste_presenti <- validi[
    CIRC_COD == probe$CIRC_COD & PLURI_COD == probe$PLURI_COD,
    LISTA
  ]
  lista_non_valida <- setdiff(parametri_input$liste[LISTA != "astensione", LISTA], liste_presenti)

  raw <- data.table::rbindlist(list(
    raw,
    data.table::data.table(
      SIM = probe$SIM,
      UNI_COD = probe$UNI_COD,
      LISTA = if (length(lista_non_valida) > 0) lista_non_valida[[1]] else "__lista_non_valida__",
      VOTI_LISTA_SIM = 321
    )
  ), use.names = TRUE)

  raw
}

prepare_ramo_fixture <- function(ramo) {
  raw_votes <- prepare_raw_votes(ramo)
  prepared <- prepara_dts(
    data.table::copy(raw_votes),
    ramo,
    dati_collegi,
    dati_candidati,
    candidati,
    parametri_input
  )

  list(
    source = list(
      uni_liste_sim = select_frame(raw_votes, c(
        "SIM",
        "UNI_COD",
        "LISTA",
        "VOTI_LISTA_SIM"
      )),
      uni = select_frame(dati_collegi[[ramo]]$uni, c(
        "CIRC_COD",
        "PLURI_COD",
        "UNI_COD"
      )),
      liste = select_frame(parametri_input$liste, c(
        "LISTA",
        "COALIZIONE"
      )),
      candidati_uni_sim = select_frame(candidati[[ramo]]$candidati_uni_sim, c(
        "SIM",
        "COALIZIONE",
        "UNI_COD",
        "LISTA_MINORANZA",
        "CANDIDATO_ID",
        "DATA_NASCITA",
        "PLURI_COD",
        "CIRC_COD"
      )),
      candidati_pluri_template = select_frame(dati_candidati[[ramo]]$candidati_pluri, c(
        "CIRC_COD",
        "PLURI_COD",
        "LISTA",
        "MINORANZA"
      ))
    ),
    expected = list(
      uni_liste_sim = select_frame(prepared$uni_liste_sim, c(
        "SIM",
        "UNI_COD",
        "LISTA",
        "VOTI_LISTA_SIM",
        "PLURI_COD",
        "CIRC_COD",
        "COALIZIONE",
        "CANDIDATO_ID",
        "CAND_MINORANZA",
        "MINORANZA"
      )),
      candidati_uni_sim = select_frame(prepared$candidati_uni_sim, c(
        "SIM",
        "COALIZIONE",
        "UNI_COD",
        "LISTA_MINORANZA",
        "CANDIDATO_ID",
        "DATA_NASCITA",
        "PLURI_COD",
        "CIRC_COD",
        "VOTI_CANDIDATO"
      ))
    )
  )
}

load("dati/debug_scrutinio.RData")
source("R/politiche/genera_voti.R")

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "dati/debug_scrutinio.RData",
    purpose = paste(
      "Golden fixture for the deterministic politics vote-preparation boundary",
      "implemented by prepara_dts(). Synthetic astensione and invalid-list rows",
      "are appended to exercise filtering behavior."
    )
  ),
  rami = list(
    camera = prepare_ramo_fixture("camera"),
    senato = prepare_ramo_fixture("senato")
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

cat("Wrote politics vote-preparation fixture:", output_path, "\n")
