#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/core/vote_generation.json"

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

trace_genera_voti <- function(
    comuni_liste,
    liste,
    data_elezione,
    simulazioni,
    colonna_localita
) {
  comuni_liste <- data.table::copy(comuni_liste)
  liste <- data.table::copy(liste)

  liste[, DT := unclass(data_elezione - DATA)^0.5]
  comuni_liste[, DT := unclass(data_elezione - DATA)^0.5]

  comuni_liste_sim <- comuni_liste[rep(seq_len(.N), times = simulazioni)]
  comuni_liste_sim[, SIM := as.integer(rep(seq_len(simulazioni), each = .N / simulazioni))]

  global_draws <- vector("list", nrow(liste) * simulazioni)
  draw_index <- 1
  liste_sim <- data.table::rbindlist(lapply(seq_len(nrow(liste)), function(i) {
    values <- rnorm(
      simulazioni,
      mean = liste$LOGIT_P[[i]],
      sd = liste$SIGMA_GLOBAL[[i]] * liste$DT[[i]]
    )
    rows <- data.table::data.table(
      phase = "global",
      LISTA = liste$LISTA[[i]],
      SIM = seq_len(simulazioni),
      LOCALITA = NA,
      mean = liste$LOGIT_P[[i]],
      sd = liste$SIGMA_GLOBAL[[i]] * liste$DT[[i]],
      value = values
    )
    for (j in seq_len(nrow(rows))) {
      global_draws[[draw_index]] <<- rows[j]
      draw_index <<- draw_index + 1
    }
    data.table::data.table(
      LISTA = liste$LISTA[[i]],
      SIM = seq_len(simulazioni),
      LOGIT_P_SIM_GLOBAL = values
    )
  }))

  comuni_liste_sim <- comuni_liste_sim[liste_sim, on = .(LISTA, SIM)]

  delta_mean <- comuni_liste_sim$DELTA
  delta_sd <- comuni_liste_sim$SIGMA_DELTA * comuni_liste_sim$DT
  delta_values <- rnorm(nrow(comuni_liste_sim), mean = delta_mean, sd = delta_sd)
  delta_draws <- data.table::data.table(
    phase = "local",
    LISTA = comuni_liste_sim$LISTA,
    SIM = comuni_liste_sim$SIM,
    LOCALITA = comuni_liste_sim[[colonna_localita]],
    mean = delta_mean,
    sd = delta_sd,
    value = delta_values
  )

  comuni_liste_sim[, DELTA_SIM := delta_values]
  comuni_liste_sim[, LOGIT_P_SIM := LOGIT_P_SIM_GLOBAL + DELTA_SIM]
  comuni_liste_sim[, p := plogis(LOGIT_P_SIM)]
  comuni_liste_sim[, PERCENTUALE_SIM := p / sum(p), by = .(SIM, get(colonna_localita))]
  comuni_liste_sim[, VOTI_LISTA_SIM := round(PERCENTUALE_SIM * ELETTORI)]
  comuni_liste_sim[, c("DT", "LOGIT_P_SIM_GLOBAL", "DELTA_SIM", "LOGIT_P_SIM", "p") := NULL]
  data.table::setcolorder(comuni_liste_sim, c("SIM"))

  list(
    normal_draws = normalize_frame(data.table::rbindlist(c(global_draws, list(delta_draws)))),
    expected = normalize_frame(comuni_liste_sim)
  )
}

data_elezione <- as.POSIXct("2027-03-01 00:00:00", tz = "UTC")
simulazioni <- 3

liste <- data.table::data.table(
  LISTA = c("Lista A", "Lista B", "Lista C"),
  DATA = as.POSIXct(
    c("2024-06-08 00:00:00", "2024-07-15 00:00:00", "2024-10-01 12:00:00"),
    tz = "UTC"
  ),
  LOGIT_P = c(-0.8, -1.25, -2.1),
  SIGMA_GLOBAL = c(0.012, 0.02, 0.018)
)

comuni_liste <- data.table::data.table(
  CODICE_COMUNE = c(1001, 1001, 1001, 1002, 1002, 1002),
  AREA = c("nord", "nord", "nord", "sud", "sud", "sud"),
  LISTA = rep(c("Lista A", "Lista B", "Lista C"), 2),
  DATA = as.POSIXct(
    c(
      "2024-06-08 00:00:00",
      "2024-06-08 00:00:00",
      "2024-06-08 00:00:00",
      "2024-07-15 00:00:00",
      "2024-07-15 00:00:00",
      "2024-07-15 00:00:00"
    ),
    tz = "UTC"
  ),
  DELTA = c(0.05, -0.1, -0.35, -0.15, 0.08, -0.22),
  SIGMA_DELTA = c(0.03, 0.025, 0.018, 0.02, 0.03, 0.015),
  ELETTORI = c(1200, 1200, 1200, 900, 900, 900)
)

set.seed(20260601)
trace <- trace_genera_voti(
  comuni_liste,
  liste,
  data_elezione,
  simulazioni,
  colonna_localita = "CODICE_COMUNE"
)

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "synthetic trace of R/generazione_voti.R",
    random_seed = 20260601,
    purpose = paste(
      "Golden fixture for the generic vote-generation math and table order.",
      "Normal draws are exported from R and injected into the TypeScript test;",
      "browser production runs use the TypeScript seeded RNG instead."
    )
  ),
  input = list(
    data_elezione = format(data_elezione, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    simulazioni = simulazioni,
    colonna_localita = "CODICE_COMUNE",
    liste = normalize_frame(liste),
    comuni_liste = normalize_frame(comuni_liste)
  ),
  normal_draws = trace$normal_draws,
  expected = trace$expected
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
  pretty = TRUE
)

cat("Wrote vote-generation fixture:", output_path, "\n")
