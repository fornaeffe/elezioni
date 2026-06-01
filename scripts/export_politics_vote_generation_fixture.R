#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/politiche/vote_generation.json"

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
    expected = comuni_liste_sim
  )
}

source("R/politiche/genera_voti.R")

data_elezione <- as.POSIXct("2027-03-01 00:00:00", tz = "UTC")
simulazioni <- 3

liste <- data.table::data.table(
  LISTA = c("Lista A", "Lista B", "Lista C", "astensione"),
  COALIZIONE = c("Coalizione 1", "Coalizione 1", "Coalizione 2", "astensione"),
  DATA = as.POSIXct(
    c("2024-06-08 00:00:00", "2024-07-15 00:00:00", "2024-10-01 12:00:00", "2024-06-08 00:00:00"),
    tz = "UTC"
  ),
  LOGIT_P = c(-0.8, -1.25, -2.1, -1.6),
  SIGMA_GLOBAL = c(0.012, 0.02, 0.018, 0.01)
)

comuni_liste <- data.table::data.table(
  CODICE_COMUNE = rep(c(10, 20, 30), each = 4),
  LISTA = rep(c("Lista A", "Lista B", "Lista C", "astensione"), 3),
  DATA = as.POSIXct(
    c(
      "2024-06-08 00:00:00",
      "2024-06-08 00:00:00",
      "2024-06-08 00:00:00",
      "2024-06-08 00:00:00",
      "2024-07-15 00:00:00",
      "2024-07-15 00:00:00",
      "2024-07-15 00:00:00",
      "2024-07-15 00:00:00",
      "2024-10-01 12:00:00",
      "2024-10-01 12:00:00",
      "2024-10-01 12:00:00",
      "2024-10-01 12:00:00"
    ),
    tz = "UTC"
  ),
  DELTA = c(0.05, -0.1, -0.35, -0.2, -0.15, 0.08, -0.22, -0.3, 0.12, -0.05, -0.28, -0.25),
  SIGMA_DELTA = c(0.03, 0.025, 0.018, 0.02, 0.02, 0.03, 0.015, 0.02, 0.018, 0.022, 0.017, 0.02)
)

base_dati <- data.table::data.table(
  CODICE_COMUNE = c(10, 20, 30),
  CODITA_20N = c(10001, 20001, 30001),
  ELETTORI = c(600, 700, 500),
  CU20_COD = c(101, 201, 201),
  SU20_COD = c(1001, 1001, 2001)
)

camera_uni <- data.table::data.table(
  CIRC_COD = c(1, 2),
  PLURI_COD = c(11, 21),
  UNI_COD = c(101, 201)
)

senato_uni <- data.table::data.table(
  CIRC_COD = c(10, 20),
  PLURI_COD = c(110, 210),
  UNI_COD = c(1001, 2001)
)

make_candidates <- function(uni, ramo) {
  data.table::rbindlist(lapply(seq_len(simulazioni), function(sim) {
    data.table::rbindlist(lapply(seq_len(nrow(uni)), function(i) {
      data.table::data.table(
        SIM = sim,
        COALIZIONE = c("Coalizione 1", "Coalizione 2"),
        UNI_COD = uni$UNI_COD[[i]],
        LISTA_MINORANZA = NA,
        CANDIDATO_ID = c(
          paste0(ramo, "_", sim, "_", uni$UNI_COD[[i]], "_C1"),
          paste0(ramo, "_", sim, "_", uni$UNI_COD[[i]], "_C2")
        ),
        DATA_NASCITA = as.POSIXct(c("1975-01-01 00:00:00", "1980-01-01 00:00:00"), tz = "UTC"),
        PLURI_COD = uni$PLURI_COD[[i]],
        CIRC_COD = uni$CIRC_COD[[i]]
      )
    }))
  }))
}

camera_candidati_pluri <- data.table::data.table(
  CIRC_COD = c(1, 1, 1, 2, 2),
  PLURI_COD = c(11, 11, 11, 21, 21),
  LISTA = c("Lista A", "Lista B", "Lista C", "Lista A", "Lista B"),
  MINORANZA = c(FALSE, FALSE, TRUE, FALSE, FALSE)
)

senato_candidati_pluri <- data.table::data.table(
  CIRC_COD = c(10, 10, 10, 20, 20),
  PLURI_COD = c(110, 110, 110, 210, 210),
  LISTA = c("Lista A", "Lista B", "Lista C", "Lista A", "Lista B"),
  MINORANZA = c(FALSE, FALSE, TRUE, FALSE, FALSE)
)

parametri_input <- list(
  liste = liste,
  comuni_liste = comuni_liste
)
dati_collegi <- list(
  base_dati = base_dati,
  camera = list(uni = camera_uni),
  senato = list(uni = senato_uni)
)
dati_candidati <- list(
  camera = list(candidati_pluri = camera_candidati_pluri),
  senato = list(candidati_pluri = senato_candidati_pluri)
)
candidati <- list(
  camera = list(candidati_uni_sim = make_candidates(camera_uni, "camera")),
  senato = list(candidati_uni_sim = make_candidates(senato_uni, "senato"))
)

unita_liste <- parametri_input$comuni_liste[, .(
  CODICE_COMUNE,
  LISTA,
  DATA,
  DELTA,
  SIGMA_DELTA
)][
  dati_collegi$base_dati[, .(
    CODICE_COMUNE,
    CODITA_20N,
    ELETTORI,
    CU20_COD,
    SU20_COD
  )],
  on = .(CODICE_COMUNE)
]

set.seed(20260601)
trace <- trace_genera_voti(
  unita_liste[, .(
    CODITA_20N,
    LISTA,
    DATA,
    DELTA,
    SIGMA_DELTA,
    ELETTORI,
    CU20_COD,
    SU20_COD
  )],
  parametri_input$liste,
  data_elezione,
  simulazioni,
  colonna_localita = "CODITA_20N"
)

unita_liste_sim <- trace$expected
unicam_liste_sim <- unita_liste_sim[
  ,
  .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)),
  by = .(SIM, UNI_COD = CU20_COD, LISTA)
]
unisen_liste_sim <- unita_liste_sim[
  ,
  .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)),
  by = .(SIM, UNI_COD = SU20_COD, LISTA)
]

camera <- prepara_dts(
  unicam_liste_sim,
  "camera",
  dati_collegi,
  dati_candidati,
  candidati,
  parametri_input
)
senato <- prepara_dts(
  unisen_liste_sim,
  "senato",
  dati_collegi,
  dati_candidati,
  candidati,
  parametri_input
)

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "synthetic trace of R/politiche/genera_voti.R",
    random_seed = 20260601,
    purpose = paste(
      "Golden fixture for politics-specific vote generation orchestration.",
      "Normal draws are exported from R and injected into the TypeScript test;",
      "browser production runs use the TypeScript seeded RNG instead."
    )
  ),
  input = list(
    data_elezione = format(data_elezione, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    simulazioni = simulazioni,
    liste = select_frame(liste, c("LISTA", "COALIZIONE", "DATA", "LOGIT_P", "SIGMA_GLOBAL")),
    comuni_liste = select_frame(comuni_liste, c("CODICE_COMUNE", "LISTA", "DATA", "DELTA", "SIGMA_DELTA")),
    base_dati = select_frame(base_dati, c("CODICE_COMUNE", "CODITA_20N", "ELETTORI", "CU20_COD", "SU20_COD")),
    camera = list(
      uni = select_frame(camera_uni, c("CIRC_COD", "PLURI_COD", "UNI_COD")),
      candidati_uni_sim = select_frame(candidati$camera$candidati_uni_sim, c(
        "SIM", "COALIZIONE", "UNI_COD", "LISTA_MINORANZA", "CANDIDATO_ID", "DATA_NASCITA", "PLURI_COD", "CIRC_COD"
      )),
      candidati_pluri_template = select_frame(camera_candidati_pluri, c("CIRC_COD", "PLURI_COD", "LISTA", "MINORANZA"))
    ),
    senato = list(
      uni = select_frame(senato_uni, c("CIRC_COD", "PLURI_COD", "UNI_COD")),
      candidati_uni_sim = select_frame(candidati$senato$candidati_uni_sim, c(
        "SIM", "COALIZIONE", "UNI_COD", "LISTA_MINORANZA", "CANDIDATO_ID", "DATA_NASCITA", "PLURI_COD", "CIRC_COD"
      )),
      candidati_pluri_template = select_frame(senato_candidati_pluri, c("CIRC_COD", "PLURI_COD", "LISTA", "MINORANZA"))
    )
  ),
  normal_draws = trace$normal_draws,
  expected = list(
    camera = list(
      uni_liste_sim = select_frame(camera$uni_liste_sim, c(
        "SIM", "UNI_COD", "LISTA", "VOTI_LISTA_SIM", "PLURI_COD", "CIRC_COD", "COALIZIONE", "CANDIDATO_ID", "CAND_MINORANZA", "MINORANZA"
      )),
      candidati_uni_sim = select_frame(camera$candidati_uni_sim, c(
        "SIM", "COALIZIONE", "UNI_COD", "LISTA_MINORANZA", "CANDIDATO_ID", "DATA_NASCITA", "PLURI_COD", "CIRC_COD", "VOTI_CANDIDATO"
      ))
    ),
    senato = list(
      uni_liste_sim = select_frame(senato$uni_liste_sim, c(
        "SIM", "UNI_COD", "LISTA", "VOTI_LISTA_SIM", "PLURI_COD", "CIRC_COD", "COALIZIONE", "CANDIDATO_ID", "CAND_MINORANZA", "MINORANZA"
      )),
      candidati_uni_sim = select_frame(senato$candidati_uni_sim, c(
        "SIM", "COALIZIONE", "UNI_COD", "LISTA_MINORANZA", "CANDIDATO_ID", "DATA_NASCITA", "PLURI_COD", "CIRC_COD", "VOTI_CANDIDATO"
      ))
    )
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
  pretty = TRUE
)

cat("Wrote politics vote-generation fixture:", output_path, "\n")
