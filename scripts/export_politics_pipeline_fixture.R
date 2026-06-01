#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/politiche/pipeline.json"

if (!requireNamespace("data.table", quietly = TRUE)) stop("Package 'data.table' is required")
if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required")

source("R/funzioni_allocazione_seggi.R")
source("R/politiche/genera_voti.R")

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

select_frame <- function(dt, columns) {
  normalize_frame(data.table::as.data.table(dt)[, ..columns])
}

json_array <- function(value) unname(as.list(value))

trace_genera_candidati_ramo <- function(
    ramo,
    candidati_uni,
    candidati_pluri,
    liste,
    frazione_uni_in_pluri,
    frazioni_pluricandidature,
    simulazioni
) {
  sample_draws <- list()
  record_sample <- function(values, size, replace, context) {
    result <- if (size > 0) sample(values, size = size, replace = replace) else values[integer()]
    sample_draws[[length(sample_draws) + 1]] <<- list(
      context = context,
      values = json_array(values),
      size = size,
      replace = replace,
      result = json_array(result)
    )
    result
  }

  liste <- data.table::copy(liste[LISTA != "astensione"])
  liste[, PERC_NORM := PERCENTUALE / sum(PERCENTUALE), by = COALIZIONE]
  out_uni <- vector("list", simulazioni)
  out_pluri <- vector("list", simulazioni)

  for (sim in seq_len(simulazioni)) {
    uni <- data.table::copy(candidati_uni)
    na_uni <- which(is.na(uni$CANDIDATO_ID))
    n_na_uni <- length(na_uni)
    if (n_na_uni > 0) uni[na_uni, CANDIDATO_ID := paste0("UNI_", sim, "_", seq_len(.N))]

    n_uni_pluri <- floor(n_na_uni * frazione_uni_in_pluri)
    idx_uni_pluri <- if (n_uni_pluri > 0) {
      record_sample(
        na_uni,
        n_uni_pluri,
        FALSE,
        list(phase = "select_uni_for_pluri", ramo = ramo, sim = sim)
      )
    } else integer()

    uni_pluri <- uni[idx_uni_pluri]
    joined <- uni_pluri[liste, on = .(COALIZIONE), allow.cartesian = TRUE]
    uni_pluri_liste <- joined[
      ,
      {
        selected <- record_sample(
          seq_len(.N),
          round(.N * PERC_NORM[1]),
          FALSE,
          list(
            phase = "assign_uni_to_list",
            ramo = ramo,
            sim = sim,
            coalizione = COALIZIONE[1],
            lista = LISTA[1]
          )
        )
        .SD[selected]
      },
      by = .(COALIZIONE, LISTA)
    ]

    pluri <- data.table::copy(candidati_pluri)
    na_pluri <- pluri[is.na(CANDIDATO_ID)]
    pluri_filled <- vector("list", length = length(unique(pluri$LISTA)))
    i <- 1

    for (lista in unique(pluri$LISTA)) {
      posti_lista <- na_pluri[LISTA == lista]
      n_posti <- nrow(posti_lista)
      if (n_posti == 0) next

      n_fraz <- Hare.Niemeyer(frazioni_pluricandidature, n_posti)
      uni_disp <- uni_pluri_liste[LISTA == lista, CANDIDATO_ID]
      candidati_lista <- character()
      n1 <- n_fraz[1]
      use_uni <- min(length(uni_disp), n1)

      if (use_uni > 0) {
        candidati_lista <- record_sample(
          uni_disp,
          use_uni,
          FALSE,
          list(phase = "fill_first_fraction_from_uni", ramo = ramo, sim = sim, lista = lista)
        )
      }

      if (use_uni < n1) {
        candidati_lista <- c(candidati_lista, paste0("PLURI_", sim, "_", lista, "_", seq_len(n1 - use_uni)))
      }

      prev <- candidati_lista
      if (length(prev) == 0) prev <- paste0("PLURI_", sim, "_", lista, "_base")

      for (f in 2:5) {
        if (n_fraz[f] == 0) next
        prev <- record_sample(
          prev,
          n_fraz[f],
          length(prev) < n_fraz[f],
          list(phase = "fill_repeated_fraction", ramo = ramo, sim = sim, lista = lista, fraction = f)
        )
        candidati_lista <- c(candidati_lista, prev)
      }

      posti_lista[, CANDIDATO_ID := candidati_lista[seq_len(.N)]]
      pluri_filled[[i]] <- posti_lista
      i <- i + 1
    }

    pluri <- data.table::rbindlist(list(pluri[!is.na(CANDIDATO_ID)], data.table::rbindlist(pluri_filled, use.names = TRUE)))
    out_uni[[sim]] <- uni[, .(SIM = sim, COALIZIONE, UNI_COD, LISTA_MINORANZA, CANDIDATO_ID, DATA_NASCITA)]
    out_pluri[[sim]] <- pluri[, .(SIM = sim, LISTA, PLURI_COD, NUMERO_CANDIDATO, MINORANZA, CANDIDATO_ID, DATA_NASCITA)]
  }

  candidati_uni_sim <- data.table::rbindlist(out_uni)
  candidati_pluri_sim <- data.table::rbindlist(out_pluri)
  candidati_uni_sim[is.na(DATA_NASCITA), DATA_NASCITA := as.POSIXct("2000-01-01")]
  candidati_pluri_sim[is.na(DATA_NASCITA), DATA_NASCITA := as.POSIXct("2000-01-01")]

  list(
    sample_draws = sample_draws,
    output = list(
      candidati_uni_sim = candidati_uni_sim,
      candidati_pluri_sim = candidati_pluri_sim
    )
  )
}

trace_genera_voti <- function(comuni_liste, liste, data_elezione, simulazioni, colonna_localita) {
  comuni_liste <- data.table::copy(comuni_liste)
  liste <- data.table::copy(liste)
  liste[, DT := unclass(data_elezione - DATA)^0.5]
  comuni_liste[, DT := unclass(data_elezione - DATA)^0.5]
  comuni_liste_sim <- comuni_liste[rep(seq_len(.N), times = simulazioni)]
  comuni_liste_sim[, SIM := as.integer(rep(seq_len(simulazioni), each = .N / simulazioni))]

  global_draws <- vector("list", nrow(liste) * simulazioni)
  draw_index <- 1
  liste_sim <- data.table::rbindlist(lapply(seq_len(nrow(liste)), function(i) {
    values <- rnorm(simulazioni, mean = liste$LOGIT_P[[i]], sd = liste$SIGMA_GLOBAL[[i]] * liste$DT[[i]])
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
    data.table::data.table(LISTA = liste$LISTA[[i]], SIM = seq_len(simulazioni), LOGIT_P_SIM_GLOBAL = values)
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

make_uninominal <- function(prefix, codes) {
  data.table::rbindlist(lapply(codes, function(code) {
    data.table::data.table(
      COALIZIONE = c("Coalizione 1", "Coalizione 2"),
      UNI_COD = code,
      LISTA_MINORANZA = NA,
      CANDIDATO_ID = c(NA, paste0(prefix, "_fixed_", code, "_C2")),
      DATA_NASCITA = as.POSIXct(c(NA, "1970-01-01 00:00:00"), tz = "UTC")
    )
  }))
}

make_plurinominal <- function(prefix, rows) {
  data.table::rbindlist(lapply(seq_len(nrow(rows)), function(i) {
    list_rows <- data.table::data.table(
      CIRC_COD = rows$CIRC_COD[[i]],
      LISTA = rows$LISTA[[i]],
      PLURI_COD = rows$PLURI_COD[[i]],
      NUMERO_CANDIDATO = seq_len(rows$N[[i]]),
      MINORANZA = rows$MINORANZA[[i]],
      CANDIDATO_ID = NA_character_,
      DATA_NASCITA = as.POSIXct(NA, tz = "UTC")
    )
    if (rows$FIXED[[i]]) {
      list_rows[1, `:=`(
        CANDIDATO_ID = paste0(prefix, "_", LISTA, "_fixed"),
        DATA_NASCITA = as.POSIXct("1960-02-02 00:00:00", tz = "UTC")
      )]
    }
    list_rows
  }))
}

prepare_direct_ramo <- function(ramo, prepared_votes, generated_candidates, uni, pluri, liste, candidati_pluri) {
  candidati_pluri_sim <- data.table::as.data.table(data.table::copy(generated_candidates$candidati_pluri_sim))
  candidati_pluri_sim[pluri, on = .(PLURI_COD), CIRC_COD := i.CIRC_COD]
  candidati_uni_sim <- data.table::as.data.table(data.table::copy(prepared_votes$candidati_uni_sim))
  candidati_uni_sim[
    uni,
    on = .(UNI_COD),
    `:=`(
      PLURI_COD = i.PLURI_COD,
      CIRC_COD = i.CIRC_COD
    )
  ]

  if (ramo == "camera") {
    candidati_pluri[, REG_COD := substr(CIRC_COD, 1, nchar(CIRC_COD) - 2)]
  } else {
    candidati_pluri[, REG_COD := CIRC_COD]
  }

  liste_minoranza <- candidati_pluri[
    ,
    .(REGIONI = length(unique(REG_COD)), MINORANZA = sum(MINORANZA)),
    by = .(LISTA)
  ][REGIONI == 1 & MINORANZA > 0, LISTA]
  liste_naz <- data.table::copy(liste)[, MINORANZA := LISTA %in% liste_minoranza]

  uni_liste_sim_dt <- as.data.frame(prepared_votes$uni_liste_sim[, .(
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
    liste_naz = as.data.frame(liste_naz[, .(LISTA, COALIZIONE, MINORANZA)]),
    totale_seggi = ifelse(ramo == "camera", 392, 196),
    simulations = unname(Map(
      function(liste_uni, candidati_uni, candidati_pluri) {
        sim <- unique(liste_uni$SIM)
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

data_elezione <- as.POSIXct("2027-03-01 00:00:00", tz = "UTC")
simulazioni <- 2
frazione_uni_in_pluri <- 0.75
frazioni_pluricandidature <- c(42, 27, 18, 9, 4) / 100
default_data_nascita <- normalize_frame(data.table::data.table(DATA_NASCITA = as.POSIXct("2000-01-01")))$DATA_NASCITA[[1]]

liste <- data.table::data.table(
  LISTA = c("Lista A", "Lista B", "Lista C", "astensione"),
  COALIZIONE = c("Coalizione 1", "Coalizione 1", "Coalizione 2", "astensione"),
  PERCENTUALE = c(0.6, 0.4, 0.25, 0.2),
  DATA = as.POSIXct(c("2024-06-08 00:00:00", "2024-07-15 00:00:00", "2024-10-01 12:00:00", "2024-06-08 00:00:00"), tz = "UTC"),
  LOGIT_P = c(-0.8, -1.25, -2.1, -1.6),
  SIGMA_GLOBAL = c(0.012, 0.02, 0.018, 0.01)
)

comuni_liste <- data.table::data.table(
  CODICE_COMUNE = rep(c(10, 20, 30), each = 4),
  LISTA = rep(c("Lista A", "Lista B", "Lista C", "astensione"), 3),
  DATA = as.POSIXct(rep(c("2024-06-08 00:00:00", "2024-07-15 00:00:00", "2024-10-01 12:00:00"), each = 4), tz = "UTC"),
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

camera_uni <- data.table::data.table(CIRC_COD = c(1, 2), PLURI_COD = c(11, 21), UNI_COD = c(101, 201))
senato_uni <- data.table::data.table(CIRC_COD = c(10, 20), PLURI_COD = c(110, 210), UNI_COD = c(1001, 2001))
camera_pluri <- data.table::data.table(CIRC_COD = c(1, 2), PLURI_COD = c(11, 21), SEGGI_PLURI = c(3, 2))
senato_pluri <- data.table::data.table(CIRC_COD = c(10, 20), PLURI_COD = c(110, 210), SEGGI_PLURI = c(3, 2))

camera_candidati_pluri <- make_plurinominal("camera", data.table::data.table(
  CIRC_COD = c(1, 1, 2), PLURI_COD = c(11, 11, 21), LISTA = c("Lista A", "Lista B", "Lista C"),
  N = c(5, 3, 3), MINORANZA = c(FALSE, FALSE, TRUE), FIXED = c(TRUE, FALSE, FALSE)
))
senato_candidati_pluri <- make_plurinominal("senato", data.table::data.table(
  CIRC_COD = c(10, 10, 20), PLURI_COD = c(110, 110, 210), LISTA = c("Lista A", "Lista B", "Lista C"),
  N = c(5, 3, 3), MINORANZA = c(FALSE, FALSE, TRUE), FIXED = c(TRUE, FALSE, FALSE)
))

set.seed(20260601)
camera_candidates <- trace_genera_candidati_ramo(
  "camera", make_uninominal("camera", c(101, 201)), camera_candidati_pluri, liste,
  frazione_uni_in_pluri, frazioni_pluricandidature, simulazioni
)
senato_candidates <- trace_genera_candidati_ramo(
  "senato", make_uninominal("senato", c(1001, 2001)), senato_candidati_pluri, liste,
  frazione_uni_in_pluri, frazioni_pluricandidature, simulazioni
)

unita_liste <- comuni_liste[, .(CODICE_COMUNE, LISTA, DATA, DELTA, SIGMA_DELTA)][
  base_dati[, .(CODICE_COMUNE, CODITA_20N, ELETTORI, CU20_COD, SU20_COD)],
  on = .(CODICE_COMUNE)
]
vote_trace <- trace_genera_voti(
  unita_liste[, .(CODITA_20N, LISTA, DATA, DELTA, SIGMA_DELTA, ELETTORI, CU20_COD, SU20_COD)],
  liste,
  data_elezione,
  simulazioni,
  "CODITA_20N"
)

unita_liste_sim <- vote_trace$expected
unicam_liste_sim <- unita_liste_sim[, .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)), by = .(SIM, UNI_COD = CU20_COD, LISTA)]
unisen_liste_sim <- unita_liste_sim[, .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)), by = .(SIM, UNI_COD = SU20_COD, LISTA)]

parametri_input <- list(liste = liste, comuni_liste = comuni_liste)
dati_collegi <- list(base_dati = base_dati, camera = list(uni = camera_uni), senato = list(uni = senato_uni))
dati_candidati <- list(
  camera = list(candidati_pluri = camera_candidati_pluri),
  senato = list(candidati_pluri = senato_candidati_pluri)
)
candidati <- list(camera = camera_candidates$output, senato = senato_candidates$output)
camera_votes <- prepara_dts(unicam_liste_sim, "camera", dati_collegi, dati_candidati, candidati, parametri_input)
senato_votes <- prepara_dts(unisen_liste_sim, "senato", dati_collegi, dati_candidati, candidati, parametri_input)

expected_snapshot <- list(
  schema_version = 1,
  source_schema_version = 1,
  source = "generated politics TypeScript pipeline",
  purpose = "Direct-scrutiny snapshot produced by the composed politics generation pipeline.",
  rami = list(
    camera = prepare_direct_ramo(
      "camera", camera_votes, camera_candidates$output, camera_uni, camera_pluri, liste[, .(LISTA, COALIZIONE)], data.table::copy(camera_candidati_pluri)
    ),
    senato = prepare_direct_ramo(
      "senato", senato_votes, senato_candidates$output, senato_uni, senato_pluri, liste[, .(LISTA, COALIZIONE)], data.table::copy(senato_candidati_pluri)
    )
  )
)

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "synthetic composed politics pipeline trace",
    random_seed = 20260601,
    purpose = "Golden fixture for candidate generation plus vote generation plus direct-scrutiny input adaptation."
  ),
  input = list(
    data_elezione = format(data_elezione, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    simulazioni = simulazioni,
    frazione_uni_in_pluri = frazione_uni_in_pluri,
    frazioni_pluricandidature = as.list(frazioni_pluricandidature),
    default_data_nascita = default_data_nascita,
    liste = select_frame(liste, c("LISTA", "COALIZIONE", "PERCENTUALE", "DATA", "LOGIT_P", "SIGMA_GLOBAL")),
    comuni_liste = select_frame(comuni_liste, c("CODICE_COMUNE", "LISTA", "DATA", "DELTA", "SIGMA_DELTA")),
    base_dati = select_frame(base_dati, c("CODICE_COMUNE", "CODITA_20N", "ELETTORI", "CU20_COD", "SU20_COD")),
    camera = list(
      uni = select_frame(camera_uni, c("CIRC_COD", "PLURI_COD", "UNI_COD")),
      pluri = select_frame(camera_pluri, c("CIRC_COD", "PLURI_COD", "SEGGI_PLURI")),
      candidati_uni = select_frame(make_uninominal("camera", c(101, 201)), c("COALIZIONE", "UNI_COD", "LISTA_MINORANZA", "CANDIDATO_ID", "DATA_NASCITA")),
      candidati_pluri = select_frame(camera_candidati_pluri, c("CIRC_COD", "LISTA", "PLURI_COD", "NUMERO_CANDIDATO", "MINORANZA", "CANDIDATO_ID", "DATA_NASCITA"))
    ),
    senato = list(
      uni = select_frame(senato_uni, c("CIRC_COD", "PLURI_COD", "UNI_COD")),
      pluri = select_frame(senato_pluri, c("CIRC_COD", "PLURI_COD", "SEGGI_PLURI")),
      candidati_uni = select_frame(make_uninominal("senato", c(1001, 2001)), c("COALIZIONE", "UNI_COD", "LISTA_MINORANZA", "CANDIDATO_ID", "DATA_NASCITA")),
      candidati_pluri = select_frame(senato_candidati_pluri, c("CIRC_COD", "LISTA", "PLURI_COD", "NUMERO_CANDIDATO", "MINORANZA", "CANDIDATO_ID", "DATA_NASCITA"))
    )
  ),
  sample_draws = c(camera_candidates$sample_draws, senato_candidates$sample_draws),
  normal_draws = vote_trace$normal_draws,
  expected = normalize_value(expected_snapshot)
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

cat("Wrote politics pipeline fixture:", output_path, "\n")
