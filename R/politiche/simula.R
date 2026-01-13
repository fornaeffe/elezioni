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
  dati_collegi <- calcola_collegi(dati)
  
  # Calcolo i parametri di input per la generazione dei voti
  parametri_input <- calcola_parametri_input(dati, scenario)
  
  # Carico i candidati
  dati_candidati <- carica_candidati(dati_collegi, scenario, parametri_input)
  
  # Genero casualmente i candidati mancanti
  candidati <- genera_candidati(
    dati_candidati,
    dati_collegi,
    parametri_input,
    frazioni_pluricandidature,
    simulazioni
  )
  
  # Genero i voti
  voti <- genera_voti_politiche(
    parametri_input,
    dati_collegi,
    dati_candidati,
    data_elezione,
    simulazioni
  )
  
  
  
}