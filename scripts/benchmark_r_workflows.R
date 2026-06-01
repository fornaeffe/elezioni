#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(name, default) {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0) return(default)
  sub(prefix, "", match[[1]], fixed = TRUE)
}

output_path <- get_arg("output", "")
politics_sims <- as.integer(get_arg("politics-sims", "100"))
municipal_sims <- as.integer(get_arg("municipal-sims", "1000"))
regional_sims <- as.integer(get_arg("regional-sims", "1000"))

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required")
}

if (nzchar(output_path) && !requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required when --output is set")
}

files <- list.files("R", full.names = TRUE, recursive = TRUE)
invisible(lapply(files, source))

run_quietly <- function(expr) {
  withCallingHandlers(
    force(expr),
    message = function(m) {
      invokeRestart("muffleMessage")
    }
  )
}

benchmark <- function(label, expr) {
  gc()
  value <- NULL
  elapsed <- system.time({
    value <- run_quietly(expr)
  })

  list(
    label = label,
    elapsed_seconds = unname(elapsed[["elapsed"]]),
    user_seconds = unname(elapsed[["user.self"]]),
    system_seconds = unname(elapsed[["sys.self"]]),
    output_size_mb = round(as.numeric(object.size(value)) / 1024^2, 2)
  )
}

set.seed(20260601)

results <- list(
  metadata = list(
    r_version = R.version.string,
    detected_cores = parallel::detectCores(),
    politics_sims = politics_sims,
    municipal_sims = municipal_sims,
    regional_sims = regional_sims
  ),
  benchmarks = list()
)

results$benchmarks$municipal_bologna <- benchmark(
  sprintf("Municipal Bologna (%s simulations)", municipal_sims),
  simula_comunali(
    "Bologna",
    "scenari/BO_soli_fuori_coal.xlsx",
    as.POSIXct("2027-03-01"),
    municipal_sims
  )
)

results$benchmarks$regional_er <- benchmark(
  sprintf("Emilia-Romagna regional (%s simulations)", regional_sims),
  {
    dati <- carica_dati(
      cache_path = "dati/dati.RData",
      filtro = list(REGIONE = "Emilia-Romagna")
    )
    parametri_input <- calcola_parametri_input(dati, "scenari/BO_insieme.xlsx")
    comuni_liste_sim <- genera_voti(
      parametri_input$comuni_liste,
      parametri_input$liste,
      as.POSIXct("2027-03-01"),
      regional_sims
    )
    esegui_scrutini_ER(
      comuni_liste_sim,
      parametri_input$liste,
      dati$pop_legale
    )
  }
)

results$benchmarks$politics <- benchmark(
  sprintf("Politics (%s simulations)", politics_sims),
  simula_politiche(
    "scenari/politiche_2027.xlsx",
    as.POSIXct("2027-03-01"),
    0.35,
    c(0.8, 0.1, 0.05, 0.03, 0.02),
    politics_sims
  )
)

print(results)

if (nzchar(output_path)) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    results,
    path = output_path,
    auto_unbox = TRUE,
    digits = NA,
    pretty = TRUE
  )
  cat("Wrote benchmark results:", output_path, "\n")
}
