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

empty_trace_frame <- function(columns) {
  as.data.frame(
    stats::setNames(rep(list(logical()), length(columns)), columns),
    stringsAsFactors = FALSE
  )
}

compute_early_trace <- function(
  liste_uni,
  candidati_uni,
  liste_naz_input,
  totali_pluri,
  totale_seggi,
  ramo
) {
  liste_naz <- as.data.frame(liste_naz_input, stringsAsFactors = FALSE)

  candidati_uni <- candidati_uni[order(
    candidati_uni$CIRCOSCRIZIONE,
    candidati_uni$COLLEGIOPLURINOMINALE,
    candidati_uni$COLLEGIOUNINOMINALE,
    candidati_uni$VOTI_CANDIDATO,
    candidati_uni$DATA_NASCITA,
    decreasing = c("FALSE", "FALSE", "FALSE", "TRUE", "TRUE"),
    method = "radix"
  ), ]

  candidati_uni$ELETTO <- !duplicated(candidati_uni$COLLEGIOUNINOMINALE)

  candidati_uni_elezione <- candidati_uni[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "CANDIDATO",
    "ELETTO"
  )]

  if (nrow(liste_uni) == 0) stop("Errore alla riga 144")

  candidati_uni <- merge(
    candidati_uni,
    aggregate(
      VOTI_LISTA ~ COLLEGIOUNINOMINALE + CANDIDATO,
      liste_uni,
      sum
    ),
    all.x = TRUE
  )

  candidati_uni$VOTI_LISTA[is.na(candidati_uni$VOTI_LISTA)] <- 0

  candidati_uni$VOTI_SOLO_CANDIDATO <-
    candidati_uni$VOTI_CANDIDATO - candidati_uni$VOTI_LISTA

  candidati_uni$QUOZIENTE <-
    candidati_uni$VOTI_LISTA / candidati_uni$VOTI_SOLO_CANDIDATO

  liste_uni <- merge(
    liste_uni,
    candidati_uni[, c(
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "ELETTO",
      "QUOZIENTE"
    )],
    all.x = TRUE
  )

  liste_uni$PARTE_INTERA <-
    liste_uni$VOTI_LISTA %/% liste_uni$QUOZIENTE
  liste_uni$RESTO <-
    liste_uni$VOTI_LISTA %% liste_uni$QUOZIENTE

  liste_uni$PARTE_INTERA[
    liste_uni$PARTE_INTERA < 0 |
      is.na(liste_uni$PARTE_INTERA) |
      is.nan(liste_uni$PARTE_INTERA)
  ] <- 0

  if (nrow(liste_uni) == 0) stop("Errore alla riga 184")

  candidati_uni <- merge(
    candidati_uni,
    aggregate(
      PARTE_INTERA ~ COLLEGIOUNINOMINALE + CANDIDATO,
      liste_uni,
      sum
    ),
    all.x = TRUE
  )

  candidati_uni$PARTE_INTERA[is.na(candidati_uni$PARTE_INTERA)] <- 0

  candidati_uni$DA_ASSEGNARE <-
    candidati_uni$VOTI_SOLO_CANDIDATO - candidati_uni$PARTE_INTERA

  liste_uni <- merge(
    liste_uni,
    candidati_uni[, c("COLLEGIOUNINOMINALE", "CANDIDATO", "DA_ASSEGNARE")],
    all.x = TRUE
  )

  liste_uni <- liste_uni[order(
    liste_uni$CIRCOSCRIZIONE,
    liste_uni$COLLEGIOPLURINOMINALE,
    liste_uni$COLLEGIOUNINOMINALE,
    liste_uni$CANDIDATO,
    liste_uni$RESTO,
    decreasing = c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE"),
    method = "radix"
  ), ]

  liste_uni$ORDINE <- ave(
    seq_along(liste_uni$CIRCOSCRIZIONE),
    paste(liste_uni$COLLEGIOUNINOMINALE, liste_uni$CANDIDATO),
    FUN = seq_along
  )

  liste_uni$VOTO_DA_RESTO <- liste_uni$ORDINE <= liste_uni$DA_ASSEGNARE

  liste_uni$VOTO_DA_RESTO[
    is.na(liste_uni$VOTO_DA_RESTO) | is.nan(liste_uni$VOTO_DA_RESTO)
  ] <- 0

  liste_uni$CIFRA <-
    liste_uni$VOTI_LISTA +
    liste_uni$PARTE_INTERA +
    liste_uni$VOTO_DA_RESTO

  if (nrow(liste_uni) == 0) stop("Errore alla riga 242")

  liste_pluri <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
    liste_uni,
    sum
  )

  if (nrow(liste_pluri) == 0) stop("Errore alla riga 250")

  liste_pluri <- merge(
    liste_pluri,
    aggregate(
      CIFRA ~ COLLEGIOPLURINOMINALE,
      liste_pluri,
      sum
    ),
    by = "COLLEGIOPLURINOMINALE",
    suffixes = c("", "_TOT")
  )

  liste_pluri$CIFRA_PERCENTUALE <-
    liste_pluri$CIFRA / liste_pluri$CIFRA_TOT * 100

  if (nrow(liste_pluri) == 0) stop("Errore alla riga 272")

  liste_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + LISTA,
    liste_pluri,
    sum
  )

  if (nrow(candidati_uni) == 0) stop("Errore alla riga 297")

  candidati_uni <- merge(
    candidati_uni,
    aggregate(
      VOTI_CANDIDATO ~ COLLEGIOUNINOMINALE,
      candidati_uni,
      sum
    ),
    by = "COLLEGIOUNINOMINALE",
    suffixes = c("", "_TOT")
  )

  candidati_uni$CIFRA_PERCENTUALE <-
    candidati_uni$VOTI_CANDIDATO / candidati_uni$VOTI_CANDIDATO_TOT * 100

  totali_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE,
    liste_circ,
    sum
  )

  liste_naz <- merge(
    liste_naz,
    aggregate(
      CIFRA ~ LISTA,
      liste_circ,
      sum
    )
  )

  totale_naz <- sum(liste_naz$CIFRA)

  liste_naz$CIFRA_PERCENTUALE <- liste_naz$CIFRA / totale_naz * 100

  liste_circ <- merge(
    liste_circ,
    totali_circ,
    by = "CIRCOSCRIZIONE",
    suffixes = c("", "_TOT")
  )

  liste_circ$CIFRA_PERCENTUALE <- liste_circ$CIFRA / liste_circ$CIFRA_TOT * 100

  if (sum(liste_uni$CAND_MINORANZA) > 0) {
    liste_circ <- merge(
      liste_circ,
      aggregate(
        ELETTO ~ CIRCOSCRIZIONE + LISTA,
        liste_uni[liste_uni$CAND_MINORANZA, ],
        sum
      ),
      all.x = TRUE
    )
    names(liste_circ)[names(liste_circ) == "ELETTO"] <- "ELETTI_MINORANZA"
  } else {
    liste_circ$ELETTI_MINORANZA <- 0
  }

  liste_circ$ELETTI_MINORANZA[is.na(liste_circ$ELETTI_MINORANZA)] <- 0

  liste_circ <- merge(
    liste_circ,
    aggregate(
      COLLEGIOUNINOMINALE ~ CIRCOSCRIZIONE,
      unique(liste_uni[, c("CIRCOSCRIZIONE", "COLLEGIOUNINOMINALE")]),
      length
    )
  )
  names(liste_circ)[names(liste_circ) == "COLLEGIOUNINOMINALE"] <- "COLLEGI_UNI"

  liste_circ$SOGLIA20 <- liste_circ$CIFRA_PERCENTUALE >= 20
  liste_circ$SOGLIA_MINORANZA <-
    liste_circ$ELETTI_MINORANZA >= ceiling(liste_circ$COLLEGI_UNI / 4)

  liste_naz <- merge(
    liste_naz,
    aggregate(
      SOGLIA20 ~ LISTA,
      liste_circ,
      function(x) Reduce("|", x)
    )
  )

  liste_naz <- merge(
    liste_naz,
    aggregate(
      SOGLIA_MINORANZA ~ LISTA,
      liste_circ,
      function(x) Reduce("|", x)
    )
  )

  liste_naz$SOGLIA1M <-
    liste_naz$CIFRA_PERCENTUALE >= 1 |
    (liste_naz$SOGLIA20 & (liste_naz$MINORANZA | ramo == "senato")) |
    liste_naz$SOGLIA_MINORANZA

  if (nrow(liste_naz[liste_naz$SOGLIA1M, ]) == 0) stop("Errore alla riga 476")

  coal_naz <- aggregate(
    CIFRA ~ COALIZIONE,
    data = liste_naz,
    sum,
    subset = SOGLIA1M
  )

  liste_circ <- merge(
    liste_circ,
    liste_naz[, c("LISTA", "SOGLIA1M", "COALIZIONE", "MINORANZA")]
  )

  if (nrow(liste_circ[liste_circ$SOGLIA1M, ]) == 0) stop("Errore alla riga 494")

  coal_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + COALIZIONE,
    liste_circ,
    sum,
    subset = SOGLIA1M
  )

  coal_naz$CIFRA_PERCENTUALE <-
    coal_naz$CIFRA / totale_naz * 100

  liste_naz$SOGLIA3 <- liste_naz$CIFRA_PERCENTUALE >= 3

  liste_naz$SOGLIA3M <-
    liste_naz$SOGLIA3 |
    (liste_naz$SOGLIA20 & (liste_naz$MINORANZA | ramo == "senato")) |
    liste_naz$SOGLIA_MINORANZA

  coal_naz <- merge(
    coal_naz,
    aggregate(
      SOGLIA3M ~ COALIZIONE,
      data = liste_naz,
      function(x) Reduce("|", x)
    )
  )

  coal_naz$SOGLIA_COALIZIONE <-
    coal_naz$CIFRA_PERCENTUALE >= 10 &
    coal_naz$SOGLIA3M

  liste_naz <- merge(
    liste_naz,
    coal_naz[, c("COALIZIONE", "SOGLIA_COALIZIONE")],
    all.x = TRUE
  )

  liste_naz$SOGLIA_SOLA <-
    (is.na(liste_naz$COALIZIONE) | !liste_naz$SOGLIA_COALIZIONE) &
    liste_naz$SOGLIA3M

  camera_riparto <- list(
    seggi_proporzionale = NA,
    totale_naz_riparto = NA,
    quoziente_elettorale_naz = NA,
    ancora_da_attribuire = NA,
    riparto_naz = empty_trace_frame(c(
      "SOGGETTO_RIPARTO",
      "CIFRA",
      "PARTE_INTERA",
      "RESTO",
      "ORDINE",
      "SEGGIO_DA_RESTO",
      "SEGGI",
      "CIFRA_AMMESSE_AL_RIPARTO",
      "QUOZIENTE",
      "PARTE_INTERA_TOT",
      "DA_ASSEGNARE"
    )),
    ammesse_naz = empty_trace_frame(c(
      "SOGGETTO_RIPARTO",
      "LISTA",
      "CIFRA",
      "QUOZIENTE",
      "PARTE_INTERA",
      "RESTO",
      "DA_ASSEGNARE",
      "ORDINE",
      "SEGGIO_DA_RESTO",
      "SEGGI"
    )),
    liste_naz_riparto = empty_trace_frame(c(
      "LISTA",
      "COALIZIONE",
      "SOGLIA1M",
      "SOGLIA3M",
      "SOGLIA_COALIZIONE",
      "SOGLIA_SOLA",
      "SOGGETTO_RIPARTO"
    ))
  )

  if (ramo == "camera") {
    seggi_proporzionale <- totale_seggi - sum(candidati_uni$ELETTO)

    if (seggi_proporzionale != sum(totali_pluri$SEGGI)) stop(
      "seggi_proporzionale = ",
      seggi_proporzionale,
      " ma sum(totali_pluri$SEGGI) = ",
      sum(totali_pluri$SEGGI)
    )

    liste_naz$SOGGETTO_RIPARTO <- NA

    liste_naz$SOGGETTO_RIPARTO[which(liste_naz$SOGLIA_COALIZIONE)] <-
      as.character(liste_naz$COALIZIONE[which(liste_naz$SOGLIA_COALIZIONE)])

    liste_naz$SOGGETTO_RIPARTO[which(liste_naz$SOGLIA_SOLA)] <-
      as.character(liste_naz$LISTA[which(liste_naz$SOGLIA_SOLA)])

    liste_naz$SOGGETTO_RIPARTO <- as.factor(liste_naz$SOGGETTO_RIPARTO)

    if (nrow(liste_naz[liste_naz$SOGLIA1M, ]) == 0) stop("Errore alla riga 645")

    riparto_naz <- aggregate(
      CIFRA ~ SOGGETTO_RIPARTO,
      liste_naz,
      sum,
      subset = SOGLIA1M
    )

    totale_naz_riparto <- sum(riparto_naz$CIFRA)
    quoziente_elettorale_naz <- totale_naz_riparto %/% seggi_proporzionale

    riparto_naz$PARTE_INTERA <- riparto_naz$CIFRA %/% quoziente_elettorale_naz
    riparto_naz$RESTO <- riparto_naz$CIFRA %% quoziente_elettorale_naz

    ancora_da_attribuire <- seggi_proporzionale - sum(riparto_naz$PARTE_INTERA)

    riparto_naz <- riparto_naz[
      order(riparto_naz$RESTO, riparto_naz$CIFRA, decreasing = TRUE),
    ]

    riparto_naz$ORDINE <- seq_along(riparto_naz$RESTO)

    riparto_naz$SEGGIO_DA_RESTO <- riparto_naz$ORDINE <= ancora_da_attribuire

    riparto_naz$SEGGI <- riparto_naz$PARTE_INTERA + riparto_naz$SEGGIO_DA_RESTO

    ammesse_naz <- liste_naz[
      liste_naz$SOGLIA3M,
      c(
        "SOGGETTO_RIPARTO",
        "LISTA",
        "CIFRA"
      )
    ]

    riparto_naz <- merge(
      riparto_naz,
      aggregate(
        CIFRA ~ SOGGETTO_RIPARTO,
        data = ammesse_naz,
        sum
      ),
      by = "SOGGETTO_RIPARTO",
      suffixes = c("", "_AMMESSE_AL_RIPARTO")
    )

    riparto_naz$QUOZIENTE <-
      riparto_naz$CIFRA_AMMESSE_AL_RIPARTO %/% riparto_naz$SEGGI

    ammesse_naz <- merge(
      ammesse_naz,
      riparto_naz[, c("SOGGETTO_RIPARTO", "QUOZIENTE")]
    )

    ammesse_naz$PARTE_INTERA <- ammesse_naz$CIFRA %/% ammesse_naz$QUOZIENTE
    ammesse_naz$RESTO <- ammesse_naz$CIFRA %% ammesse_naz$QUOZIENTE

    riparto_naz <- merge(
      riparto_naz,
      aggregate(
        PARTE_INTERA ~ SOGGETTO_RIPARTO,
        data = ammesse_naz,
        sum
      ),
      by = "SOGGETTO_RIPARTO",
      suffixes = c("", "_TOT")
    )

    riparto_naz$DA_ASSEGNARE <- riparto_naz$SEGGI - riparto_naz$PARTE_INTERA_TOT

    ammesse_naz <- merge(
      ammesse_naz,
      riparto_naz[, c("SOGGETTO_RIPARTO", "DA_ASSEGNARE")]
    )

    ammesse_naz <- ammesse_naz[order(
      ammesse_naz$SOGGETTO_RIPARTO,
      ammesse_naz$RESTO,
      ammesse_naz$CIFRA,
      decreasing = c(FALSE, TRUE, TRUE),
      method = "radix"
    ), ]

    ammesse_naz$ORDINE <- ave(
      seq_along(ammesse_naz$SOGGETTO_RIPARTO),
      ammesse_naz$SOGGETTO_RIPARTO,
      FUN = seq_along
    )

    ammesse_naz$SEGGIO_DA_RESTO <- ammesse_naz$ORDINE <= ammesse_naz$DA_ASSEGNARE

    ammesse_naz$SEGGI <- ammesse_naz$PARTE_INTERA + ammesse_naz$SEGGIO_DA_RESTO

    camera_riparto <- list(
      seggi_proporzionale = seggi_proporzionale,
      totale_naz_riparto = totale_naz_riparto,
      quoziente_elettorale_naz = quoziente_elettorale_naz,
      ancora_da_attribuire = ancora_da_attribuire,
      riparto_naz = riparto_naz[, c(
        "SOGGETTO_RIPARTO",
        "CIFRA",
        "PARTE_INTERA",
        "RESTO",
        "ORDINE",
        "SEGGIO_DA_RESTO",
        "SEGGI",
        "CIFRA_AMMESSE_AL_RIPARTO",
        "QUOZIENTE",
        "PARTE_INTERA_TOT",
        "DA_ASSEGNARE"
      )],
      ammesse_naz = ammesse_naz[, c(
        "SOGGETTO_RIPARTO",
        "LISTA",
        "CIFRA",
        "QUOZIENTE",
        "PARTE_INTERA",
        "RESTO",
        "DA_ASSEGNARE",
        "ORDINE",
        "SEGGIO_DA_RESTO",
        "SEGGI"
      )],
      liste_naz_riparto = liste_naz[, c(
        "LISTA",
        "COALIZIONE",
        "SOGLIA1M",
        "SOGLIA3M",
        "SOGLIA_COALIZIONE",
        "SOGLIA_SOLA",
        "SOGGETTO_RIPARTO"
      )]
    )
  }

  totali_circ_base <- totali_circ[, c("CIRCOSCRIZIONE", "CIFRA")]

  totali_circ <- merge(
    totali_circ,
    aggregate(
      SEGGI ~ CIRCOSCRIZIONE,
      totali_pluri,
      sum
    )
  )

  if (ramo == "camera") {
    liste_circ <- merge(
      liste_circ,
      liste_naz[
        ,
        c(
          "LISTA",
          "SOGGETTO_RIPARTO",
          "SOGLIA1M",
          "SOGLIA3"
        )
      ]
    )
  } else {
    liste_circ <- merge(
      liste_circ,
      coal_naz[, c("COALIZIONE", "SOGLIA_COALIZIONE")],
      all.x = TRUE
    )

    liste_circ <- merge(
      liste_circ,
      liste_naz[, c("LISTA", "SOGLIA1M", "SOGLIA3")]
    )

    liste_circ$SOGLIA_SOLA <-
      (is.na(liste_circ$COALIZIONE) | !liste_circ$SOGLIA_COALIZIONE) &
      (liste_circ$SOGLIA3 | liste_circ$SOGLIA20 | liste_circ$SOGLIA_MINORANZA)

    liste_circ$SOGGETTO_RIPARTO <- NA

    liste_circ$SOGGETTO_RIPARTO[which(liste_circ$SOGLIA_COALIZIONE)] <-
      as.character(liste_circ$COALIZIONE[which(liste_circ$SOGLIA_COALIZIONE)])

    liste_circ$SOGGETTO_RIPARTO[which(liste_circ$SOGLIA_SOLA)] <-
      as.character(liste_circ$LISTA[which(liste_circ$SOGLIA_SOLA)])
  }

  if (nrow(liste_circ[liste_circ$SOGLIA1M, ]) == 0) stop("Errore alla riga 931")

  riparto_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
    data = liste_circ,
    sum,
    subset = SOGLIA1M
  )

  totali_circ <- merge(
    totali_circ,
    aggregate(
      CIFRA ~ CIRCOSCRIZIONE,
      data = riparto_circ,
      sum
    ),
    by = "CIRCOSCRIZIONE",
    suffixes = c("", "_AMMESSE_AL_RIPARTO")
  )

  totali_circ$QUOZIENTE <-
    totali_circ$CIFRA_AMMESSE_AL_RIPARTO %/% totali_circ$SEGGI

  riparto_circ <- merge(
    riparto_circ,
    totali_circ[, c("CIRCOSCRIZIONE", "QUOZIENTE")]
  )

  riparto_circ$PARTE_INTERA <- riparto_circ$CIFRA %/% riparto_circ$QUOZIENTE

  totali_circ <- merge(
    totali_circ,
    aggregate(
      PARTE_INTERA ~ CIRCOSCRIZIONE,
      data = riparto_circ,
      sum
    )
  )

  totali_circ$DA_ASSEGNARE <- totali_circ$SEGGI - totali_circ$PARTE_INTERA

  riparto_circ <- merge(
    riparto_circ,
    totali_circ[, c("CIRCOSCRIZIONE", "DA_ASSEGNARE")]
  )

  riparto_naz_circ <- empty_trace_frame(c(
    "SOGGETTO_RIPARTO",
    "CIFRA",
    "SEGGI",
    "PARTE_INTERA_CIRC",
    "ESCLUSE",
    "SEGGI_CIRC",
    "SEGGI_ECCEDENTI",
    "SEGGI_ECCEDENTI_CONTATORE"
  ))

  if (ramo == "camera") {
    riparto_circ$DECIMALI <- (riparto_circ$CIFRA / riparto_circ$QUOZIENTE) %% 1

    riparto_naz <- merge(
      riparto_naz,
      aggregate(
        PARTE_INTERA ~ SOGGETTO_RIPARTO,
        data = riparto_circ,
        sum
      ),
      by = "SOGGETTO_RIPARTO",
      suffixes = c("", "_CIRC")
    )

    riparto_naz$ESCLUSE <- riparto_naz$PARTE_INTERA_CIRC >= riparto_naz$SEGGI

    riparto_circ <- merge(
      riparto_circ,
      riparto_naz[, c("SOGGETTO_RIPARTO", "ESCLUSE", "CIFRA")],
      by = "SOGGETTO_RIPARTO",
      suffixes = c("", "_NAZ")
    )

    riparto_circ <- riparto_circ[
      order(
        riparto_circ$CIRCOSCRIZIONE,
        riparto_circ$ESCLUSE,
        riparto_circ$DECIMALI,
        riparto_circ$CIFRA_NAZ,
        decreasing = c(FALSE, FALSE, TRUE, TRUE),
        method = "radix"
      ),
    ]

    riparto_circ$ORDINE[!riparto_circ$ESCLUSE] <- ave(
      seq_along(riparto_circ$CIRCOSCRIZIONE[!riparto_circ$ESCLUSE]),
      riparto_circ$CIRCOSCRIZIONE[!riparto_circ$ESCLUSE],
      FUN = seq_along
    )

    riparto_circ$SEGGIO_DA_DECIMALI <-
      riparto_circ$ORDINE <= riparto_circ$DA_ASSEGNARE
    riparto_circ$SEGGIO_DA_DECIMALI[is.na(riparto_circ$SEGGIO_DA_DECIMALI)] <-
      FALSE

    riparto_circ$SEGGI <-
      riparto_circ$PARTE_INTERA + riparto_circ$SEGGIO_DA_DECIMALI

    riparto_naz <- merge(
      riparto_naz,
      aggregate(
        SEGGI ~ SOGGETTO_RIPARTO,
        data = riparto_circ,
        sum
      ),
      by = "SOGGETTO_RIPARTO",
      suffixes = c("", "_CIRC")
    )

    riparto_naz$SEGGI_ECCEDENTI <- riparto_naz$SEGGI_CIRC - riparto_naz$SEGGI

    riparto_naz <- riparto_naz[
      order(
        riparto_naz$SEGGI_ECCEDENTI,
        riparto_naz$CIFRA,
        decreasing = TRUE
      ),
    ]

    riparto_naz$SEGGI_ECCEDENTI_CONTATORE <- riparto_naz$SEGGI_ECCEDENTI

    riparto_circ$FLIPPER <- 0

    for (i in seq_along(riparto_naz$SOGGETTO_RIPARTO)) {
      if (riparto_naz$SEGGI_ECCEDENTI[i] < 1) break

      s <- riparto_naz$SOGGETTO_RIPARTO[i]
      for (j in 1:riparto_naz$SEGGI_ECCEDENTI[i]) {
        riparto_circ$DEFICIT <-
          riparto_circ$SOGGETTO_RIPARTO %in% riparto_naz$SOGGETTO_RIPARTO[
            riparto_naz$SEGGI_ECCEDENTI_CONTATORE < 0
          ] & !riparto_circ$SEGGIO_DA_DECIMALI & riparto_circ$FLIPPER == 0

        rc <- riparto_circ[
          riparto_circ$SOGGETTO_RIPARTO == s &
            riparto_circ$SEGGIO_DA_DECIMALI &
            riparto_circ$FLIPPER == 0,
        ]

        if (dim(rc)[1] < 1) stop(
          "Devo togliere un seggio eccedente ma non ci sono circoscrizioni dove ",
          "questo sia stato ottenuto con i resti"
        )

        rc$DEFICIT_PRESENTE <- rc$CIRCOSCRIZIONE %in% riparto_circ$CIRCOSCRIZIONE[
          riparto_circ$DEFICIT
        ]

        rc <- rc[
          order(
            rc$DEFICIT_PRESENTE,
            rc$DECIMALI,
            decreasing = c(TRUE, FALSE),
            method = "radix"
          ),
        ]

        c <- rc$CIRCOSCRIZIONE[1]

        if (rc$DEFICIT_PRESENTE[1]) {
          rc2 <- riparto_circ[
            riparto_circ$CIRCOSCRIZIONE == c & riparto_circ$DEFICIT,
          ]
        } else {
          rc2 <- riparto_circ[riparto_circ$DEFICIT, ]
        }

        if (dim(rc2)[1] < 1) stop("Non ho a chi dare il seggio eccedente")

        rc2 <- rc2[order(
          rc2$DECIMALI,
          rc2$CIFRA_NAZ,
          decreasing = TRUE
        ), ]

        s2 <- rc2$SOGGETTO_RIPARTO[1]

        c2 <- rc2$CIRCOSCRIZIONE[1]

        riparto_circ$FLIPPER[
          riparto_circ$SOGGETTO_RIPARTO == s &
            riparto_circ$CIRCOSCRIZIONE == c
        ] <- -1

        riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s] <-
          riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s] - 1

        riparto_circ$FLIPPER[
          riparto_circ$SOGGETTO_RIPARTO == s2 &
            riparto_circ$CIRCOSCRIZIONE == c2
        ] <- 1

        riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s2] <-
          riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s2] + 1
      }
    }

    riparto_circ$SEGGI <- riparto_circ$SEGGI + riparto_circ$FLIPPER

    riparto_naz_circ <- riparto_naz[, c(
      "SOGGETTO_RIPARTO",
      "CIFRA",
      "SEGGI",
      "PARTE_INTERA_CIRC",
      "ESCLUSE",
      "SEGGI_CIRC",
      "SEGGI_ECCEDENTI",
      "SEGGI_ECCEDENTI_CONTATORE"
    )]
  } else {
    riparto_circ$RESTO <- riparto_circ$CIFRA %% riparto_circ$QUOZIENTE

    riparto_circ <- riparto_circ[
      order(
        riparto_circ$CIRCOSCRIZIONE,
        riparto_circ$RESTO,
        riparto_circ$CIFRA,
        decreasing = c(FALSE, TRUE, TRUE),
        method = "radix"
      ),
    ]

    riparto_circ$ORDINE <- ave(
      seq_along(riparto_circ$CIRCOSCRIZIONE),
      riparto_circ$CIRCOSCRIZIONE,
      FUN = seq_along
    )

    riparto_circ$SEGGIO_DA_RESTO <-
      riparto_circ$ORDINE <= riparto_circ$DA_ASSEGNARE

    riparto_circ$SEGGI <-
      riparto_circ$PARTE_INTERA + riparto_circ$SEGGIO_DA_RESTO
  }

  for (name in c(
    "DECIMALI",
    "RESTO",
    "CIFRA_NAZ",
    "ESCLUSE",
    "ORDINE",
    "SEGGIO_DA_DECIMALI",
    "SEGGIO_DA_RESTO",
    "FLIPPER"
  )) {
    if (!name %in% names(riparto_circ)) riparto_circ[[name]] <- NA
  }

  if (!"SOGLIA_SOLA" %in% names(liste_circ)) liste_circ$SOGLIA_SOLA <- NA
  if (!"SOGLIA_COALIZIONE" %in% names(liste_circ)) liste_circ$SOGLIA_COALIZIONE <- NA

  circ_riparto <- list(
    totali_circ = totali_circ[, c(
      "CIRCOSCRIZIONE",
      "CIFRA",
      "SEGGI",
      "CIFRA_AMMESSE_AL_RIPARTO",
      "QUOZIENTE",
      "PARTE_INTERA",
      "DA_ASSEGNARE"
    )],
    liste_circ = liste_circ[, c(
      "CIRCOSCRIZIONE",
      "LISTA",
      "CIFRA",
      "SOGLIA1M",
      "SOGLIA3",
      "SOGLIA20",
      "SOGLIA_MINORANZA",
      "SOGLIA_COALIZIONE",
      "SOGLIA_SOLA",
      "SOGGETTO_RIPARTO"
    )],
    riparto_circ = riparto_circ[, c(
      "CIRCOSCRIZIONE",
      "SOGGETTO_RIPARTO",
      "CIFRA",
      "QUOZIENTE",
      "PARTE_INTERA",
      "DA_ASSEGNARE",
      "DECIMALI",
      "RESTO",
      "CIFRA_NAZ",
      "ESCLUSE",
      "ORDINE",
      "SEGGIO_DA_DECIMALI",
      "SEGGIO_DA_RESTO",
      "FLIPPER",
      "SEGGI"
    )],
    riparto_naz = riparto_naz_circ
  )

  list(
    totale_naz = totale_naz,
    candidati_uni_elezione = candidati_uni_elezione,
    candidati_uni_attribuzione = candidati_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "VOTI_CANDIDATO",
      "VOTI_LISTA",
      "VOTI_SOLO_CANDIDATO",
      "QUOZIENTE",
      "PARTE_INTERA",
      "DA_ASSEGNARE"
    )],
    liste_uni_cifre = liste_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "LISTA",
      "VOTI_LISTA",
      "ELETTO",
      "QUOZIENTE",
      "PARTE_INTERA",
      "RESTO",
      "DA_ASSEGNARE",
      "ORDINE",
      "VOTO_DA_RESTO",
      "CIFRA"
    )],
    liste_pluri_cifre = liste_pluri[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA",
      "CIFRA",
      "CIFRA_TOT",
      "CIFRA_PERCENTUALE"
    )],
    liste_circ_cifre = liste_circ[, c(
      "CIRCOSCRIZIONE",
      "LISTA",
      "CIFRA"
    )],
    candidati_uni_graduatoria = candidati_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "ELETTO",
      "VOTI_CANDIDATO",
      "VOTI_CANDIDATO_TOT",
      "CIFRA_PERCENTUALE"
    )],
    totali_circ = totali_circ_base,
    liste_naz_soglie = liste_naz[, c(
      "LISTA",
      "COALIZIONE",
      "MINORANZA",
      "CIFRA",
      "CIFRA_PERCENTUALE",
      "SOGLIA20",
      "SOGLIA_MINORANZA",
      "SOGLIA1M",
      "SOGLIA3",
      "SOGLIA3M",
      "SOGLIA_COALIZIONE",
      "SOGLIA_SOLA"
    )],
    liste_circ_soglie = liste_circ[, c(
      "CIRCOSCRIZIONE",
      "LISTA",
      "CIFRA",
      "CIFRA_TOT",
      "CIFRA_PERCENTUALE",
      "ELETTI_MINORANZA",
      "COLLEGI_UNI",
      "SOGLIA20",
      "SOGLIA_MINORANZA",
      "SOGLIA1M",
      "COALIZIONE",
      "MINORANZA"
    )],
    coal_naz_soglie = coal_naz[, c(
      "COALIZIONE",
      "CIFRA",
      "CIFRA_PERCENTUALE",
      "SOGLIA3M",
      "SOGLIA_COALIZIONE"
    )],
    coal_circ_cifre = coal_circ[, c(
      "CIRCOSCRIZIONE",
      "COALIZIONE",
      "CIFRA"
    )],
    camera_riparto = camera_riparto,
    circ_riparto = circ_riparto
  )
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
    schema_version = 5,
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
    sim_fixture$trace <- compute_early_trace(
      sim_fixture$input$liste_uni,
      sim_fixture$input$candidati_uni,
      ramo_fixture$liste_naz,
      ramo_fixture$totali_pluri,
      ramo_fixture$totale_seggi,
      ramo
    )
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
