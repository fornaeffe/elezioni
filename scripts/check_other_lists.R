#!/usr/bin/env Rscript
# Golden-input politics and constructed regional checks; run from the repository root. No source data writes.
library(data.table)
source("R/funzioni_allocazione_seggi.R", encoding = "UTF-8")
source("R/politiche/scrutinio.R", encoding = "UTF-8")
source("R/Emilia-Romagna/scrutinio_ER.R", encoding = "UTF-8")
fixture <- jsonlite::read_json("test/fixtures/politiche/debug_scrutinio.json", simplifyVector = TRUE)
outputs <- list()
for (ramo in c("camera", "senato")) {
  r <- fixture$rami[[ramo]]
  input <- lapply(r$simulations$input, function(x) x[[1]])
  input$candidati_uni$DATA_NASCITA <- as.POSIXct(input$candidati_uni$DATA_NASCITA, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  uni <- as.data.frame(input$liste_uni)
  other <- uni[!duplicated(uni$COLLEGIOUNINOMINALE), ]
  other$LISTA <- "Altre liste"
  other$CANDIDATO <- NA_character_
  other$CAND_MINORANZA <- FALSE
  other$MINORANZA <- FALSE
  other$VOTI_LISTA <- 100
  input$liste_uni <- rbind(uni, other)
  lists <- r$liste_naz
  lists$kind <- "list"
  lists <- rbind(lists, data.frame(LISTA = "Altre liste", COALIZIONE = NA_character_, MINORANZA = FALSE, kind = "other"))
  out <- scrutinio_politiche(input$liste_uni, input$candidati_uni, input$candidati_pluri,
    r$totali_pluri, lists, r$totale_seggi, ramo)
  stopifnot(all(out$liste_pluri$ELETTI[out$liste_pluri$LISTA == "Altre liste"] == 0),
    !anyNA(out$candidati_uni$CANDIDATO), !any(out$candidati_pluri$LISTA == "Altre liste"))
  outputs[[ramo]] <- out
}
population <- data.table(CODICE_COMUNE = "1", COMUNE = "Test", CODICE_PROVINCIA = "1", PROVINCIA = "Test", CODICE_REGIONE = "08", REGIONE = "Emilia-Romagna", POPOLAZIONE = 10000)
for (share in c(1, 30, 80)) {
  lists <- data.frame(LISTA = c("A", "B", "Altre liste"), COALIZIONE = c("A", "B", NA), kind = c("list", "list", "other"))
  votes <- data.table(CODICE_COMUNE = "1", COMUNE = "Test", CODICE_PROVINCIA = "1", PROVINCIA = "Test", LISTA = lists$LISTA, VOTI_LISTA_SIM = c((100-share)*60, (100-share)*40, share*100))
  set.seed(1)
  out <- scrutinio_regionali_ER(votes, population, lists)
  stopifnot(out$liste$ELETTI[out$liste$LISTA == "Altre liste"] == 0,
    out$liste$PERCENTUALE[out$liste$LISTA == "Altre liste"] == share/100,
    !anyNA(out$coalizioni$COALIZIONE), sum(out$liste$ELETTI) == 48)
  outputs[[paste0("regional-", share)]] <- out
}
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) jsonlite::write_json(outputs, args[[1]], auto_unbox = TRUE, dataframe = "rows", na = "null", digits = NA)
cat("Other list checks passed: Camera, Senato, and regional 1/30/80 percent.\n")
