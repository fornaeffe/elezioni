#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/regionali-er/debug_scrutiny.json"
scenario_path <- if (length(args) >= 2) args[[2]] else "scenari/BO_insieme.xlsx"
cache_path <- if (length(args) >= 3) args[[3]] else "dati/dati.RData"
data_elezione <- if (length(args) >= 4) as.POSIXct(args[[4]]) else as.POSIXct("2027-03-01")
simulazioni <- if (length(args) >= 5) as.integer(args[[5]]) else 10
random_seed <- if (length(args) >= 6) as.integer(args[[6]]) else 20260606

required_packages <- c("data.table", "jsonlite", "readxl", "formattable")
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required")
  }
}

library(data.table)

source("R/funzioni_allocazione_seggi.R")
source("R/caricamento_dati.R")
source("R/calcolo_parametri_input.R")
source("R/generazione_voti.R")
source("R/Emilia-Romagna/scrutinio_ER.R", encoding = "UTF-8")

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
  missing_columns <- setdiff(columns, names(dt))
  if (length(missing_columns) > 0) {
    stop("Missing columns: ", paste(missing_columns, collapse = ", "))
  }
  normalize_frame(data.table::as.data.table(dt)[, ..columns])
}

message("Loading Emilia-Romagna source data from ", cache_path)
dati <- carica_dati(
  cache_path = cache_path,
  filtro = list(REGIONE = "Emilia-Romagna")
)

message("Calculating regional input parameters from ", scenario_path)
parametri_input <- calcola_parametri_input(dati, scenario_path)

set.seed(random_seed)
message("Generating ", simulazioni, " regional vote simulations")
comuni_liste_sim <- genera_voti(
  parametri_input$comuni_liste,
  parametri_input$liste,
  data_elezione,
  simulazioni
)

message("Running R regional scrutiny")
risultato <- esegui_scrutini_ER(
  comuni_liste_sim,
  parametri_input$liste,
  dati$pop_legale
)

simulations <- lapply(seq_len(simulazioni), function(sim) {
  list(
    sim = sim,
    input = list(
      comuni_liste = select_frame(
        comuni_liste_sim[SIM == sim],
        c("CODICE_COMUNE", "COMUNE", "LISTA", "CODICE_PROVINCIA", "PROVINCIA", "VOTI_LISTA_SIM")
      )
    ),
    expected = list(
      coalizioni = select_frame(
        risultato$coalizioni[SIM == sim],
        c("COALIZIONE", "PRESIDENTE", "MIGLIOR_PERDENTE", "VOTI_LISTA_ITER", "PERCENTUALE", "ELETTI", "ELETTI_TOT")
      ),
      liste = select_frame(
        risultato$liste[SIM == sim],
        c("LISTA", "COALIZIONE", "VOTI_LISTA_ITER", "PERCENTUALE", "ELETTI")
      ),
      prov_lista = select_frame(
        risultato$prov_lista[SIM == sim],
        c("PROVINCIA", "LISTA", "ELETTI", "VOTI_LISTA_ITER", "PERCENTUALE")
      )
    )
  )
})

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "R/Emilia-Romagna/scrutinio_ER.R",
    purpose = "Golden fixture for the Emilia-Romagna regional scrutiny TypeScript migration.",
    cache_path = cache_path,
    scenario_path = scenario_path,
    random_seed = random_seed,
    simulations = simulazioni,
    data_elezione = format(data_elezione, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ),
  context = list(
    pop_legale = select_frame(
      dati$pop_legale,
      c("CODICE_COMUNE", "POPOLAZIONE", "COMUNE", "CODICE_PROVINCIA", "PROVINCIA", "CODICE_REGIONE", "REGIONE")
    ),
    liste = select_frame(
      parametri_input$liste,
      c("LISTA", "COALIZIONE", "COLORE", "PERCENTUALE", "DATA", "LOGIT_P", "SIGMA_GLOBAL", "SIGMA_DELTA")
    )
  ),
  simulations = simulations
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

cat("Wrote regional Emilia-Romagna golden fixture:", output_path, "\n")
