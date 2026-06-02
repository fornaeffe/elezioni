#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "web/static/data/v1/politics-static.json"
scenario_path <- if (length(args) >= 2) args[[2]] else "scenari/politiche_2027.xlsx"
cache_path <- if (length(args) >= 3) args[[3]] else "dati/dati.RData"
data_elezione <- if (length(args) >= 4) as.POSIXct(args[[4]]) else as.POSIXct("2027-03-01")

required_packages <- c("data.table", "jsonlite", "readxl", "formattable")
for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required")
  }
}

source("R/funzioni_allocazione_seggi.R")
source("R/caricamento_dati.R")
source("R/calcolo_parametri_input.R")
source("R/politiche/calcolo_collegi_ramo.R")
source("R/politiche/calcolo_collegi.R")
source("R/politiche/carica_candidati.R")

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

default_data_nascita <- normalize_frame(data.frame(
  DATA_NASCITA = as.POSIXct("2000-01-01")
))$DATA_NASCITA[[1]]

message("Loading cached politics source data from ", cache_path)
dati <- carica_dati(
  cache_path = cache_path,
  filtro = list(ELEZIONE = c(
    "camera 2018",
    "europee 2019",
    "camera 2022",
    "europee 2024"
  ))
)

message("Calculating politics colleges")
dati_collegi <- calcola_collegi(dati)

message("Calculating politics input parameters from ", scenario_path)
parametri_input <- calcola_parametri_input(dati, scenario_path, "europee")

message("Loading politics candidate templates")
dati_candidati <- carica_candidati(dati_collegi, scenario_path, parametri_input)

snapshot <- list(
  metadata = list(
    schema_version = 1,
    source = "current R politics preparation pipeline",
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    purpose = paste(
      "Production politics static snapshot bridge exported from the current R",
      "data cache and scenario workbook. Reusable election data is split from",
      "the default scenario for the SvelteKit worker."
    ),
    cache_path = cache_path,
    scenario_path = scenario_path,
    elections = c("camera 2018", "europee 2019", "camera 2022", "europee 2024")
  ),
  data = list(
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
      ))
    )
  ),
  default_scenario = list(
    id = "politiche-2027",
    name = "Politiche 2027",
    data_elezione = format(data_elezione, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    frazione_uni_in_pluri = 0,
    frazioni_pluricandidature = as.list(c(1, 0, 0, 0, 0)),
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
    )),
    camera = list(
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

cat("Wrote politics static snapshot:", output_path, "\n")
