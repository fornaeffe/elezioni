# ATTENZIONE: questa funzione richiede almeno 16 GB di RAM!

simula_politiche <- function(
    scenario,
    data_elezione,
    frazione_uni_in_pluri = 0,
    frazioni_pluricandidature = c(1,0,0,0,0),
    simulazioni = 1000
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData")
  
  # Estraggo i collegi e ne calcolo i seggi e gli elettori
  dati_collegi <- calcola_collegi(dati)
  
  # Calcolo i parametri di input per la generazione dei voti
  parametri_input <- calcola_parametri_input(dati, scenario, "europee")
  
  # Carico i candidati
  dati_candidati <- carica_candidati(dati_collegi, scenario, parametri_input)
  
  # Genero casualmente i candidati mancanti
  candidati <- genera_candidati(
    dati_candidati,
    dati_collegi,
    parametri_input,
    frazione_uni_in_pluri,
    frazioni_pluricandidature,
    simulazioni
  )
  
  # Genero i voti
  voti <- genera_voti_politiche(
    parametri_input,
    dati_collegi,
    dati_candidati,
    candidati,
    data_elezione,
    simulazioni
  )
  
  scrutinio <- esegui_scrutini_politiche(
    dati_collegi,
    parametri_input,
    dati_candidati,
    candidati,
    voti
  )
  
  prepara_output_ramo <- function(ramo){
    # TODO verificare quali di questi copy è davvero necessario
    pluri <- data.table::copy(dati_collegi[[ramo]]$pluri)
    uni <- data.table::copy(dati_collegi[[ramo]]$uni)
    liste <- data.table::copy(parametri_input$liste)
    candidati_uni <- data.table::copy(dati_candidati[[ramo]]$candidati_uni)
    candidati_pluri <- data.table::copy(dati_candidati[[ramo]]$candidati_pluri)
    candidati_uni_sim <- data.table::copy(candidati[[ramo]]$candidati_uni_sim)
    candidati_uni_sim_voti <- data.table::copy(voti[[ramo]]$candidati_uni_sim)
    uni_liste_sim <- data.table::copy(voti[[ramo]]$uni_liste_sim)
    pluri_liste_sim_scrutinio <- 
      data.table::copy(scrutinio[[ramo]]$pluri_liste_sim)
    candidati_uni_sim_scrutinio <- 
      data.table::copy(scrutinio[[ramo]]$candidati_uni_sim)
    candidati_pluri_sim_scrutinio <- 
      data.table::copy(scrutinio[[ramo]]$candidati_pluri_sim)
    
    
    
    # Costruisco il data.table pluri_liste_sim con le informazioni dai
    # diversi step della simulazione
    pluri_liste_sim <- uni_liste_sim[
      ,
      .(
        VOTI_LISTA = sum(VOTI_LISTA_SIM)
      ),
      by = .(
        SIM,
        PLURI_COD,
        LISTA
      )
    ][
      pluri,
      on = .(PLURI_COD),
      `:=`(
        CIRC_COD = i.CIRC_COD,
        CIRC_DEN = i.CIRC_DEN,
        PLURI_DEN = i.PLURI_DEN,
        SEGGI_PLURI = i.SEGGI_PLURI,
        MAX_CANDIDATI = i.MAX_CANDIDATI
      )
    ][
      pluri_liste_sim_scrutinio,
      on = .(SIM, PLURI_COD, LISTA),
      `:=`(
        ELETTI = i.ELETTI,
        NUMERO_MAX = i.NUMERO_MAX,
        SEGGI_PRE_SUBENTRI = i.SEGGI_PRE_SUBENTRI
      )
    ][
      liste,
      on = .(LISTA),
      `:=`(
        COALIZIONE = i.COALIZIONE
      )
    ]
    
    # Calcolo la percentuale sui voti validi per ogni lista in ogni
    # collegio plurinominale
    pluri_liste_sim[
      ,
      PERCENTUALE := formattable::percent(VOTI_LISTA / sum(VOTI_LISTA), 2),
      by = .(SIM, PLURI_COD)
    ]
    
    # Costruisco il data.table liste_sim con le informazioni dai
    # diversi step della simulazione
    liste_sim <- pluri_liste_sim[
      ,
      .(
        VOTI_LISTA = sum(VOTI_LISTA),
        ELETTI = sum(ELETTI),
        SEGGI_PRE_SUBENTRI = sum(SEGGI_PRE_SUBENTRI)
      ),
      by = .(
        SIM,
        COALIZIONE,
        LISTA
      )
    ]
    
    # Calcolo le percentuali nazionali
    liste_sim[
      ,
      PERCENTUALE := formattable::percent(VOTI_LISTA / sum(VOTI_LISTA), 2),
      by = .(SIM)
    ]
    
    # Costruisco il data.table candidati_uni_sim con le informazioni dai
    # diversi step della simulazione
    candidati_uni_sim[
      uni,
      on = .(UNI_COD),
      `:=`(
        CIRC_COD = i.CIRC_COD,
        CIRC_DEN = i.CIRC_DEN,
        PLURI_COD = i.PLURI_COD,
        PLURI_DEN = i.PLURI_DEN,
        UNI_DEN = i.UNI_DEN
      )
    ][
      candidati_uni,
      on = .(CANDIDATO_ID),
      `:=`(
        COGNOME_CAND = i.COGNOME_CAND_UNI,
        NOME_CAND = i.NOME_CAND_UNI
      )
    ][
      candidati_uni_sim_voti,
      on = .(
        SIM,
        UNI_COD,
        CANDIDATO_ID
      ),
      VOTI_CANDIDATO := i.VOTI_CANDIDATO
    ][
      candidati_uni_sim_scrutinio,
      on = .(
        SIM,
        UNI_COD,
        CANDIDATO_ID
      ),
      ELETTO := i.ELETTO
    ]
    
    # Calcolo la percentuale nei collegi uninominali
    candidati_uni_sim[
      ,
      PERCENTUALE := formattable::percent(VOTI_CANDIDATO / sum(VOTI_CANDIDATO), 2),
      by = .(SIM, UNI_COD)
    ]
    
    # FIX: rimuovo VdA
    # TODO: generazione candidati e scrutinio per VdA
    # candidati_uni_sim <- candidati_uni_sim[CIRC_COD != "201"]
    # candidati_uni_sim <- candidati_uni_sim[CIRC_COD != "2"]
    # 
    # FIX: rimuovo TAA al Senato
    
    # Creo il data.table delle coalizioni
    coalizioni_sim <- candidati_uni_sim[
      ,
      .(
        VOTI_CANDIDATO = sum(VOTI_CANDIDATO),
        ELETTI_UNI = sum(ELETTO)
      ),
      by = .(SIM, COALIZIONE)
    ]
    
    # Aggiungo le info relative ai collegi plurinominali
    coalizioni_sim[
      liste_sim[
        ,
        .(
          VOTI_LISTA = sum(VOTI_LISTA),
          ELETTI_PLURI = sum(ELETTI)
        ),
        by = .(SIM, COALIZIONE)
      ],
      on = .(SIM, COALIZIONE),
      `:=`(
        VOTI_LISTA = i.VOTI_LISTA,
        ELETTI_PLURI = i.ELETTI_PLURI
      )
    ][
      ,
      ELETTI := ELETTI_UNI + ELETTI_PLURI
    ]
    
    # Calcolo le percentuali
    coalizioni_sim[
      ,
      `:=`(
        PERCENTUALE_CANDIDATO = formattable::percent(VOTI_CANDIDATO / sum(VOTI_CANDIDATO), 2),
        PERCENTUALE_LISTA = formattable::percent(VOTI_LISTA / sum(VOTI_LISTA), 2)
      ),
      by = .(SIM)
    ]
    
    
    # Costruisco il data.table candidati_pluri_sim con le informazioni dai
    # diversi step della simulazione
    candidati_pluri_sim <- candidati_pluri_sim_scrutinio[
      pluri,
      on = .(PLURI_COD),
      `:=`(
        CIRC_COD = i.CIRC_COD,
        CIRC_DEN = i.CIRC_DEN,
        PLURI_DEN = i.PLURI_DEN,
        SEGGI_PLURI = i.SEGGI_PLURI,
        MAX_CANDIDATI = i.MAX_CANDIDATI
      )
    ][
      candidati_pluri,
      on = .(CANDIDATO_ID),
      `:=`(
        COGNOME_CAND = i.COGNOME_CAND_PLURI,
        NOME_CAND = i.NOME_CAND_PLURI,
        DATA_NASCITA = i.DATA_NASCITA
      )
    ][
      liste,
      on = .(LISTA),
      `:=`(
        COALIZIONE = i.COALIZIONE
      )
    ]
    
    return(
      list(
        uni = uni,
        pluri = pluri,
        coalizioni = parametri_input$coalizioni,
        liste = liste,
        liste_elezioni = parametri_input$liste_elezioni,
        comuni_liste = parametri_input$comuni_liste,
        corrispondenza_liste = parametri_input$corrispondenza_liste,
        coalizioni_sim = coalizioni_sim,
        liste_sim = liste_sim,
        pluri_liste_sim = pluri_liste_sim,
        candidati_uni_sim = candidati_uni_sim,
        candidati_pluri_sim = candidati_pluri_sim
      )
    )
    
    
  }
  
  camera <- prepara_output_ramo("camera")
  senato <- prepara_output_ramo("senato")
  
  return(
    list(
      camera = camera,
      senato = senato
    )
  )
  
  
  
}