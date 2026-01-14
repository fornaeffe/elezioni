# Dati necessari:
# parametri_input$liste
# parametri_input$comuni_liste
# dati_collegi$base_dati
# dati_collegi[[ramo]]$uni
# dati_candidati[[ramo]]$candidati_pluri
# candidati[[ramo]]$candidati_uni_sim
# data_elezione
# simulazioni

genera_voti_politiche <- function(
    parametri_input,
    dati_collegi,
    dati_candidati,
    candidati,
    data_elezione,
    simulazioni
) {
  # Passo i parametri di input, generati su base comunale, alle singole
  # unità territoriali della base dati
  unita_liste <- parametri_input$comuni_liste[,.(
    CODICE_COMUNE,
    LISTA,
    DATA,
    DELTA,
    SIGMA_DELTA
  )][
    dati_collegi$base_dati[,.(
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
    by = .(SIM, UNI_COD = SU20_COD, LISTA)
  ]
  
  unicam_liste_sim <- unita_liste_sim[
    ,
    .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)),
    by = .(SIM, UNI_COD = CU20_COD, LISTA)
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
  
  return(
    list(
      camera = camera,
      senato = senato
    )
  )
}

prepara_dts <- function(
    uni_liste_sim,
    ramo,
    dati_collegi,
    dati_candidati,
    candidati,
    parametri_input
){
  uni_liste_sim <- uni_liste_sim[LISTA != "astensione"]
  
  uni_liste_sim[
    dati_collegi[[ramo]]$uni,
    on = .(UNI_COD),
    `:=`(
      PLURI_COD = i.PLURI_COD,
      CIRC_COD = i.CIRC_COD
    )
  ]
  # TODO risolvere l'assenza della VdA
  validi <- unique(
    dati_candidati[[ramo]]$candidati_pluri[, .(
      CIRC_COD, 
      PLURI_COD, 
      LISTA,
      MINORANZA
    )]
  )
  
  uni_liste_sim_filtrato <- uni_liste_sim[
    validi,
    on = .(CIRC_COD, PLURI_COD, LISTA),
    nomatch = NULL
  ]
  
  uni_liste_sim_filtrato[
    parametri_input$liste[, .(LISTA, COALIZIONE)],
    on = .(LISTA),
    COALIZIONE := i.COALIZIONE
  ]
  
  # TODO aggiungere la possibilità dei candidati delle minoranze
  uni_liste_sim_filtrato[
    candidati[[ramo]]$candidati_uni_sim,
    on = .(SIM, UNI_COD, COALIZIONE),
    `:=`(
      CANDIDATO_ID = i.CANDIDATO_ID,
      CAND_MINORANZA = FALSE
    )
  ]
  
  voti_candidato <- uni_liste_sim_filtrato[
    ,
    .(VOTI_CANDIDATO = sum(VOTI_LISTA_SIM)),
    by = .(SIM, UNI_COD, COALIZIONE, CANDIDATO_ID)
  ]
  
  candidati_uni_sim <- candidati[[ramo]]$candidati_uni_sim[
    voti_candidato,
    on = .(SIM, UNI_COD, COALIZIONE, CANDIDATO_ID),
    nomatch = NULL
  ]
  
  return(
    list(
      uni_liste_sim = uni_liste_sim_filtrato,
      candidati_uni_sim = candidati_uni_sim
    )
  )
}