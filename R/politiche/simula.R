# ATTENZIONE: questa funzione richiede almeno 16 GB di RAM!

simula_politiche <- function(
    scenario,
    data_elezione,
    frazioni_pluricandidature = c(1,0,0,0,0),
    simulazioni = 1000
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData")
  
  # Estraggo i collegi e ne calcolo i seggi e gli elettori
  dati_politiche <- calcola_collegi(dati)
  
  # Calcolo i parametri di input per la generazione dei voti
  parametri_input <- calcola_parametri_input(dati, scenario)
  
  # Carico i candidati
  dati_politiche <- carica_candidati(dati_politiche, scenario, parametri_input)
  
  # Genero casualmente i candidati mancanti
  candidati <- genera_candidati(
    dati_politiche,
    parametri_input,
    frazioni_pluricandidature,
    simulazioni
  )
  
  
  
}