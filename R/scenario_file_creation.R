crea_file_scenario <- function(
    dati,
    percorso = "scenario.xlsx",
    sovrascrivi = FALSE
) {
  # --- Controlli preliminari ----------------------------------------
  stopifnot(is.data.frame(dati$comuni_liste_elezioni))
  
  if (file.exists(percorso) && !sovrascrivi) {
    stop(paste("Il file", percorso, "esiste già. Usa sovrascrivi = TRUE per ricrearlo."))
  }
  
  # --- 1. liste_future (vuoto) -------------------------------------
  liste_future <- data.table::data.table(LISTA_FUTURA = character(), COALIZIONE = character(), COLORE = character())
  
  # --- 2. flussi_previsti ------------------------------------------
  colonne_flussi <- c(
    "DATA", "ELEZIONE", "LISTA"
  )
  flussi_previsti <- unique(dati$comuni_liste_elezioni[, ..colonne_flussi])
  flussi_previsti[, `:=`(LISTA_FUTURA = "", FRAZIONE = 1)]
  
  # --- Scrittura file Excel ----------------------------------------
  scrittura <- list(
    liste_future = liste_future,
    flussi_previsti = flussi_previsti
  )
  
  writexl::write_xlsx(scrittura, path = percorso)
  message("✅ File di scenario creato con successo: ", percorso)
  
  invisible(percorso)
}
