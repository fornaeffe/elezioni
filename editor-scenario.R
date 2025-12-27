library(data.table)
library(DataEditR)

# Inserire il nome dello scenario:
scenario <- "base"

file_scenario <- paste0("scenari/", scenario,".RData")

# Carica i dati
load("dati/dati.RData")


setkey(dati, DATA, ELEZIONE, LISTA)

# Elenca le liste nelle passate elezioni
liste_passate <- unique(dati[, c("DATA", "ELEZIONE", "LISTA")])
setkey(liste_passate, DATA, ELEZIONE, LISTA)

# Controlla se è già presente un file per lo scenario indicato
if (file.exists(file_scenario)) {
  
  # Se sì, lo carica
  load(file_scenario)
  
  # Controlla che non ci siano liste passati mancanti
  matrice_liste <- matrice_liste[liste_passate]

  # Riempie le nuove righe
  for (col in names(matrice_liste)[-1:-3]) {
    matrice_liste[is.na(get(col)) & col == LISTA, (col) := 1]  # Sostituisci con 1 se il nome della colonna coincide con LISTA
    matrice_liste[is.na(get(col)) & col != LISTA, (col) := 0]  # Altrimenti sostituisci con 0
  }
} else {
  
  # Se no, crea la tabella delle liste
  liste <- data.table(
    LISTA = "astensione",
    COALIZIONE = as.character(NA),
    COLORE = as.character(NA)
  )
  
  # E inizializza la matrice delle liste
  matrice_liste <- liste_passate
  setkey(matrice_liste, DATA, ELEZIONE, LISTA)
}

# Modifica manualmente le liste
liste <- data_edit(
  liste,
  col_edit = FALSE,
  col_names = FALSE,
  title = "Liste alle prossime elezioni",
  viewer = "browser"
)

# Aggiunge alla matrice delle liste le colonne relative alle liste mancanti
if (length(setdiff(liste$LISTA, colnames(matrice_liste[-1:-3]))) > 0) {
  matrice_liste <- cbind(
    matrice_liste,
    sapply(
      setdiff(liste$LISTA, colnames(matrice_liste[-1:-3])),
      function(lista) {
        (matrice_liste$LISTA == lista) * 1
      }
    )
  )
}


# Rimuove le colonne relative alle liste cancellate
matrice_liste[,setdiff(colnames(matrice_liste)[-1:-3], liste$LISTA)] <- NULL

# Modifica manualmente la matrice
matrice_liste <- data_edit(
  matrice_liste,
  col_edit = FALSE,
  col_names = FALSE,
  col_readonly = c("DATA", "ELEZIONE", "LISTA"),
  row_edit = FALSE,
  title = "Corrispondenza tra liste passate e future",
  viewer = "browser"
)

# TODO: check righe a somma zero

# Fix formato data
matrice_liste$DATA <- as.POSIXct(matrice_liste$DATA)
setkey(matrice_liste, DATA, ELEZIONE, LISTA)

save(liste, matrice_liste, file = file_scenario)
