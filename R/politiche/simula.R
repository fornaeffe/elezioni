# ATTENZIONE: questa funzione richiede almeno 16 GB di RAM!

simula_politiche <- function(
    scenario,
    data_elezione,
    simulazioni = 1000
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData")
  
  # Estraggo i collegi e ne calcolo i seggi e gli elettori
  dati_politiche <- calcola_collegi(dati)
  
  # Calcolo i parametri di input per la generazione dei voti
  parametri_input <- calcola_parametri_input(dati, scenario)
  
  
  # Passo i parametri di input, generati su base comunale, alle singole
  # unità territoriali della base dati
  unita_liste <- parametri_input$comuni_liste[,.(
    CODICE_COMUNE,
    LISTA,
    DATA,
    DELTA,
    SIGMA_DELTA
  )][
    base_dati[,.(
      CODICE_COMUNE,
      CODITA_20N,
      ELETTORI,
      CU20_COD,
      SU20_COD
    )],
    on = .(CODICE_COMUNE)
  ]
  
  unita_liste_sim <- genera_voti(
    unita_liste[,.(
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
  
  unisen_liste_sim <- unita_liste_sim[
    ,
    .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)),
    by = .(SIM, SU20_COD, LISTA)
  ]
  
  unicam_liste_sim <- unita_liste_sim[
    ,
    .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)),
    by = .(SIM, CU20_COD, LISTA)
  ]
  
}