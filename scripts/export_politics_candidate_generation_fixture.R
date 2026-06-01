#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
output_path <- if (length(args) >= 1) args[[1]] else "test/fixtures/politiche/candidate_generation.json"

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Package 'data.table' is required")
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required")
}

source("R/funzioni_allocazione_seggi.R")

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

json_array <- function(value) {
  unname(as.list(value))
}

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
    result <- if (size > 0) {
      sample(values, size = size, replace = replace)
    } else {
      values[integer()]
    }

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

    if (n_na_uni > 0) {
      uni[na_uni, CANDIDATO_ID := paste0("UNI_", sim, "_", seq_len(.N))]
    }

    n_uni_pluri <- floor(n_na_uni * frazione_uni_in_pluri)
    idx_uni_pluri <- if (n_uni_pluri > 0) {
      record_sample(
        na_uni,
        n_uni_pluri,
        FALSE,
        list(phase = "select_uni_for_pluri", ramo = ramo, sim = sim)
      )
    } else {
      integer()
    }

    uni_pluri <- uni[idx_uni_pluri]

    joined <- uni_pluri[
      liste,
      on = .(COALIZIONE),
      allow.cartesian = TRUE
    ]

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
        nuovi <- paste0("PLURI_", sim, "_", lista, "_", seq_len(n1 - use_uni))
        candidati_lista <- c(candidati_lista, nuovi)
      }

      prev <- candidati_lista
      if (length(prev) == 0) {
        prev <- paste0("PLURI_", sim, "_", lista, "_base")
      }

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

    pluri <- data.table::rbindlist(list(
      pluri[!is.na(CANDIDATO_ID)],
      data.table::rbindlist(pluri_filled, use.names = TRUE)
    ))

    out_uni[[sim]] <- uni[, .(
      SIM = sim,
      COALIZIONE,
      UNI_COD,
      LISTA_MINORANZA,
      CANDIDATO_ID,
      DATA_NASCITA
    )]

    out_pluri[[sim]] <- pluri[, .(
      SIM = sim,
      LISTA,
      PLURI_COD,
      NUMERO_CANDIDATO,
      MINORANZA,
      CANDIDATO_ID,
      DATA_NASCITA
    )]
  }

  candidati_uni_sim <- data.table::rbindlist(out_uni)
  candidati_pluri_sim <- data.table::rbindlist(out_pluri)

  candidati_uni_sim[is.na(DATA_NASCITA), DATA_NASCITA := as.POSIXct("2000-01-01")]
  candidati_pluri_sim[is.na(DATA_NASCITA), DATA_NASCITA := as.POSIXct("2000-01-01")]

  list(
    sample_draws = sample_draws,
    output = list(
      candidati_uni_sim = normalize_frame(candidati_uni_sim),
      candidati_pluri_sim = normalize_frame(candidati_pluri_sim)
    )
  )
}

make_uninominal <- function(prefix, codes) {
  data.table::data.table(
    COALIZIONE = rep(c("Coalizione 1", "Coalizione 2"), length(codes)),
    UNI_COD = rep(codes, each = 2),
    LISTA_MINORANZA = NA,
    CANDIDATO_ID = c(
      NA,
      paste0(prefix, "_fixed_", codes[[1]], "_C2"),
      NA,
      NA,
      paste0(prefix, "_fixed_", codes[[3]], "_C1"),
      NA
    ),
    DATA_NASCITA = as.POSIXct(
      c(
        NA,
        "1970-01-01 00:00:00",
        NA,
        NA,
        "1965-05-05 00:00:00",
        NA
      ),
      tz = "UTC"
    )
  )
}

make_plurinominal <- function(prefix, pluri_codes) {
  data.table::rbindlist(list(
    data.table::data.table(
      LISTA = "Lista A",
      PLURI_COD = pluri_codes[[1]],
      NUMERO_CANDIDATO = 1:6,
      MINORANZA = FALSE,
      CANDIDATO_ID = c(paste0(prefix, "_A_fixed"), rep(NA, 5)),
      DATA_NASCITA = as.POSIXct(c("1960-02-02 00:00:00", rep(NA, 5)), tz = "UTC")
    ),
    data.table::data.table(
      LISTA = "Lista B",
      PLURI_COD = pluri_codes[[1]],
      NUMERO_CANDIDATO = 1:4,
      MINORANZA = FALSE,
      CANDIDATO_ID = rep(NA, 4),
      DATA_NASCITA = as.POSIXct(rep(NA, 4), tz = "UTC")
    ),
    data.table::data.table(
      LISTA = "Lista C",
      PLURI_COD = pluri_codes[[2]],
      NUMERO_CANDIDATO = 1:3,
      MINORANZA = TRUE,
      CANDIDATO_ID = rep(NA, 3),
      DATA_NASCITA = as.POSIXct(rep(NA, 3), tz = "UTC")
    )
  ))
}

liste <- data.table::data.table(
  LISTA = c("Lista A", "Lista B", "Lista C", "astensione"),
  COALIZIONE = c("Coalizione 1", "Coalizione 1", "Coalizione 2", "astensione"),
  PERCENTUALE = c(0.6, 0.4, 0.25, 0.2)
)

camera_source <- list(
  candidati_uni = make_uninominal("camera", c(101, 102, 103)),
  candidati_pluri = make_plurinominal("camera", c(11, 21))
)
senato_source <- list(
  candidati_uni = make_uninominal("senato", c(1001, 1002, 1003)),
  candidati_pluri = make_plurinominal("senato", c(110, 210))
)

frazione_uni_in_pluri <- 0.75
frazioni_pluricandidature <- c(42, 27, 18, 9, 4) / 100
simulazioni <- 2
default_data_nascita <- normalize_frame(data.table::data.table(
  DATA_NASCITA = as.POSIXct("2000-01-01")
))$DATA_NASCITA[[1]]

set.seed(20260601)
camera_trace <- trace_genera_candidati_ramo(
  "camera",
  camera_source$candidati_uni,
  camera_source$candidati_pluri,
  liste,
  frazione_uni_in_pluri,
  frazioni_pluricandidature,
  simulazioni
)
senato_trace <- trace_genera_candidati_ramo(
  "senato",
  senato_source$candidati_uni,
  senato_source$candidati_pluri,
  liste,
  frazione_uni_in_pluri,
  frazioni_pluricandidature,
  simulazioni
)

fixture <- list(
  metadata = list(
    schema_version = 1,
    source = "synthetic trace of R/politiche/genera_candidati.R",
    random_seed = 20260601,
    purpose = paste(
      "Golden fixture for politics candidate generation.",
      "R sample() outputs are exported and replayed in TypeScript tests;",
      "browser production runs use the TypeScript seeded sampler."
    )
  ),
  input = list(
    simulazioni = simulazioni,
    frazione_uni_in_pluri = frazione_uni_in_pluri,
    frazioni_pluricandidature = as.list(frazioni_pluricandidature),
    default_data_nascita = default_data_nascita,
    liste = normalize_frame(liste),
    camera = list(
      candidati_uni = normalize_frame(camera_source$candidati_uni),
      candidati_pluri = normalize_frame(camera_source$candidati_pluri)
    ),
    senato = list(
      candidati_uni = normalize_frame(senato_source$candidati_uni),
      candidati_pluri = normalize_frame(senato_source$candidati_pluri)
    )
  ),
  sample_draws = c(camera_trace$sample_draws, senato_trace$sample_draws),
  expected = list(
    camera = camera_trace$output,
    senato = senato_trace$output
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

cat("Wrote politics candidate-generation fixture:", output_path, "\n")
