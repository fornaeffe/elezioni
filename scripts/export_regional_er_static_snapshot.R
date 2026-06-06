#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "web/static/data/v1/regional-er-static.json"
scenario_path <- if (length(args) >= 2) args[[2]] else "scenari/BO_insieme.xlsx"
cache_path <- if (length(args) >= 3) args[[3]] else "dati/dati.RData"
data_elezione <- if (length(args) >= 4) as.POSIXct(args[[4]]) else as.POSIXct("2027-03-01")

required_packages <- c("data.table", "jsonlite", "readxl", "formattable")
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required")
  }
}

library(data.table)

source("R/caricamento_dati.R")
source("R/calcolo_parametri_input.R")

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

municipalities <- unique(
  data.table::as.data.table(parametri_input$comuni_liste)[
    ,
    .(
      CODICE_COMUNE,
      COMUNE,
      CODICE_PROVINCIA,
      PROVINCIA,
      CODICE_REGIONE,
      REGIONE,
      ELETTORI
    )
  ]
)
data.table::setorder(municipalities, CODICE_COMUNE)

snapshot <- list(
  metadata = list(
    schema_version = 1,
    source = "current R Emilia-Romagna regional preparation pipeline",
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    purpose = paste(
      "Production Emilia-Romagna regional static snapshot exported from the",
      "current R data cache and scenario workbook."
    ),
    cache_path = cache_path,
    scenario_path = scenario_path,
    region = "Emilia-Romagna"
  ),
  data = list(
    municipalities = select_frame(municipalities, c(
      "CODICE_COMUNE",
      "COMUNE",
      "CODICE_PROVINCIA",
      "PROVINCIA",
      "CODICE_REGIONE",
      "REGIONE",
      "ELETTORI"
    )),
    comuni_liste_elezioni = select_frame(dati$comuni_liste_elezioni, c(
      "DATA",
      "ELEZIONE",
      "CODICE_COMUNE",
      "LISTA",
      "VOTI"
    )),
    pop_legale = select_frame(dati$pop_legale, c(
      "CODICE_COMUNE",
      "POPOLAZIONE",
      "COMUNE",
      "CODICE_PROVINCIA",
      "PROVINCIA",
      "CODICE_REGIONE",
      "REGIONE"
    ))
  ),
  default_scenario = list(
    id = "regionali-er-2027",
    name = "Regionali Emilia-Romagna 2027",
    data_elezione = format(data_elezione, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    liste = select_frame(parametri_input$liste, c(
      "LISTA",
      "COALIZIONE",
      "COLORE",
      "PERCENTUALE",
      "DATA",
      "LOGIT_P",
      "SIGMA_GLOBAL",
      "SIGMA_DELTA"
    )),
    comuni_liste = select_frame(parametri_input$comuni_liste, c(
      "CODICE_COMUNE",
      "COMUNE",
      "CODICE_PROVINCIA",
      "PROVINCIA",
      "CODICE_REGIONE",
      "REGIONE",
      "LISTA",
      "DATA",
      "DELTA",
      "ELETTORI",
      "SIGMA_DELTA"
    )),
    coalizioni = select_frame(parametri_input$coalizioni, c(
      "COALIZIONE",
      "COLORE"
    )),
    liste_elezioni = select_frame(parametri_input$liste_elezioni, c(
      "DATA",
      "ELEZIONE",
      "LISTA",
      "VOTI",
      "PERCENTUALE",
      "LOGIT_P"
    )),
    corrispondenza_liste = select_frame(parametri_input$corrispondenza_liste, c(
      "DATA",
      "ELEZIONE",
      "LISTA_ORIGINALE",
      "LISTA",
      "FATTORE"
    ))
  )
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(
  snapshot,
  path = output_path,
  dataframe = "rows",
  null = "null",
  na = "null",
  auto_unbox = TRUE,
  digits = NA,
  pretty = FALSE
)

cat("Wrote regional Emilia-Romagna static snapshot:", output_path, "\n")
