#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/politiche/debug_scrutinio.json"

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required")
}

source_all_r <- function() {
  files <- list.files("R", full.names = TRUE, recursive = TRUE)
  invisible(lapply(files, source))
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

normalize_value <- function(value) {
  if (data.table::is.data.table(value) || is.data.frame(value)) {
    normalize_frame(value)
  } else if (is.list(value)) {
    lapply(value, normalize_value)
  } else {
    value
  }
}

capture_conditions <- function(expr) {
  warnings <- character()
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, warnings = as.list(warnings), messages = as.list(messages))
}

prepare_ramo <- function(ramo) {
  uni_liste_sim <- data.table::copy(voti[[ramo]]$uni_liste_sim)
  candidati_uni_sim <- data.table::copy(voti[[ramo]]$candidati_uni_sim)
  candidati_pluri_sim <- data.table::copy(candidati[[ramo]]$candidati_pluri_sim)
  pluri <- data.table::copy(dati_collegi[[ramo]]$pluri)
  uni <- data.table::copy(dati_collegi[[ramo]]$uni)
  liste <- data.table::copy(parametri_input$liste)
  candidati_pluri <- data.table::copy(dati_candidati[[ramo]]$candidati_pluri)

  totale_seggi <- ifelse(ramo == "camera", 392, 196)

  candidati_uni_sim[
    uni,
    on = .(UNI_COD),
    `:=`(
      PLURI_COD = i.PLURI_COD,
      CIRC_COD = i.CIRC_COD
    )
  ]

  candidati_pluri_sim[
    pluri,
    on = .(PLURI_COD),
    CIRC_COD := i.CIRC_COD
  ]

  if (ramo == "camera") {
    candidati_pluri[, REG_COD := substr(CIRC_COD, 1, nchar(CIRC_COD) - 2)]
  } else {
    candidati_pluri[, REG_COD := CIRC_COD]
  }

  liste_minoranza <- candidati_pluri[
    ,
    .(
      REGIONI = length(unique(REG_COD)),
      MINORANZA = sum(MINORANZA)
    ),
    by = .(LISTA)
  ][REGIONI == 1 & MINORANZA > 0, LISTA]

  liste[, MINORANZA := LISTA %in% liste_minoranza]

  uni_liste_sim_dt <- as.data.frame(uni_liste_sim[, .(
    CIRCOSCRIZIONE = CIRC_COD,
    COLLEGIOPLURINOMINALE = PLURI_COD,
    COLLEGIOUNINOMINALE = UNI_COD,
    CANDIDATO = CANDIDATO_ID,
    CAND_MINORANZA,
    LISTA,
    MINORANZA,
    VOTI_LISTA = VOTI_LISTA_SIM,
    SIM
  )])

  candidati_uni_sim_dt <- as.data.frame(candidati_uni_sim[, .(
    CIRCOSCRIZIONE = CIRC_COD,
    COLLEGIOPLURINOMINALE = PLURI_COD,
    COLLEGIOUNINOMINALE = UNI_COD,
    CANDIDATO = CANDIDATO_ID,
    DATA_NASCITA,
    VOTI_CANDIDATO,
    SIM
  )])

  candidati_pluri_sim_dt <- as.data.frame(candidati_pluri_sim[, .(
    CIRCOSCRIZIONE = CIRC_COD,
    COLLEGIOPLURINOMINALE = PLURI_COD,
    LISTA,
    NUMERO = NUMERO_CANDIDATO,
    CANDIDATO = CANDIDATO_ID,
    SIM
  )])

  list(
    totali_pluri = as.data.frame(pluri[, .(
      CIRCOSCRIZIONE = CIRC_COD,
      COLLEGIOPLURINOMINALE = PLURI_COD,
      SEGGI = SEGGI_PLURI
    )]),
    liste_naz = as.data.frame(liste[, .(
      LISTA,
      COALIZIONE,
      MINORANZA
    )]),
    totale_seggi = totale_seggi,
    simulations = unname(Map(
      function(liste_uni, candidati_uni, candidati_pluri) {
        sim <- unique(liste_uni$SIM)
        stopifnot(length(sim) == 1)
        list(
          sim = sim,
          input = list(
            liste_uni = liste_uni,
            candidati_uni = candidati_uni,
            candidati_pluri = candidati_pluri
          )
        )
      },
      split(uni_liste_sim_dt, uni_liste_sim_dt$SIM),
      split(candidati_uni_sim_dt, candidati_uni_sim_dt$SIM),
      split(candidati_pluri_sim_dt, candidati_pluri_sim_dt$SIM)
    ))
  )
}

run_simulation <- function(ramo, ramo_fixture, sim_fixture) {
  capture_conditions(scrutinio_politiche(
    sim_fixture$input$liste_uni,
    sim_fixture$input$candidati_uni,
    sim_fixture$input$candidati_pluri,
    totali_pluri = ramo_fixture$totali_pluri,
    liste_naz = ramo_fixture$liste_naz,
    totale_seggi = ramo_fixture$totale_seggi,
    ramo = ramo
  ))
}

load("dati/debug_scrutinio.RData")
source_all_r()
set.seed(20260601)

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "dati/debug_scrutinio.RData",
    purpose = "Golden-master fixture for the politics scrutiny TypeScript port.",
    random_seed = 20260601,
    warning_policy = "Warnings are expected output until Luca approves a business or legal correction."
  ),
  rami = list()
)

for (ramo in c("camera", "senato")) {
  ramo_fixture <- prepare_ramo(ramo)
  ramo_fixture$simulations <- lapply(ramo_fixture$simulations, function(sim_fixture) {
    result <- run_simulation(ramo, ramo_fixture, sim_fixture)
    sim_fixture$expected <- result$value
    sim_fixture$warnings <- result$warnings
    sim_fixture$messages <- result$messages
    sim_fixture
  })
  fixture$rami[[ramo]] <- ramo_fixture
}

fixture <- normalize_value(fixture)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  fixture,
  path = output_path,
  dataframe = "rows",
  null = "null",
  na = "null",
  auto_unbox = TRUE,
  digits = NA,
  pretty = TRUE
)

cat("Wrote politics golden fixture:", output_path, "\n")
