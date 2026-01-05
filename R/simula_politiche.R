# ATTENZIONE: questa funzione richiede almeno 16 GB di RAM!

simula_politiche <- function(
    scenario,
    data_elezione,
    simulazioni = 1000
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData")
  
  # Carico i collegi
  base_dati <- data.table::fread(
    "dati/BaseDati_Proposta_Commissione.csv",
    na.strings = ""
  )
  
  # Aggiungo manualmente la modifica fatta dal governo alla proposta della 
  # commissione, perché il file della proposta della commissione era più
  # semplice da importare.
  # TODO: importare direttamente e automaticamente i file definitivi
  # dalla pagina https://www.riformeistituzionali.gov.it/it/i-nuovi-collegi-elettorali/
  base_dati[
    CU20_DEN == "Lazio 1 - U03",
    `:=`(
      CP20_DEN = "Lazio 1 - P01",
      CP20_COD = 120101,
      CP20_COD1 = "P01"
    )
  ]
  base_dati[
    CU20_DEN == "Lazio 1 - U08",
    `:=`(
      CP20_DEN = "Lazio 1 - P02",
      CP20_COD = 120102,
      CP20_COD1 = "P02"
    )
  ]
  base_dati[
    CU20_DEN == "Lazio 1 - U07",
    `:=`(
      CP20_DEN = "Lazio 1 - P03",
      CP20_COD = 120103,
      CP20_COD1 = "P03"
    )
  ]
  
  # Aggiorno i codici comune delle unità territoriali della base dati collegi:
  # Converto i codici comune nella tabella ISTAT di traslazione (perché in 
  # base_dati sono numeric)
  ISTAT_traslazione <- data.table::copy(dati$ISTAT_traslazione)
  ISTAT_traslazione[
    ,
    PRO_COM_T_numeric := as.numeric(PRO_COM_T)
  ]
  
  # Verifico che per tutti ci sia una corrispondenza
  stopifnot(length(setdiff(base_dati$PRO_COM20, ISTAT_traslazione$PRO_COM_T_numeric)) == 0)
  
  # Aggiorno i codici comune
  base_dati[
    ISTAT_traslazione,
    on = .(PRO_COM20 = PRO_COM_T_numeric),
    `:=`(
      CODICE_COMUNE = i.PRO_COM_T_DT_FI
    )
  ]
  
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
      CODITA_20N,
      POP_2011,
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
      ELETTORI = POP_2011,
      CU20_COD,
      SU20_COD
    )],
    parametri_input$liste,
    data_elezione,
    simulazioni,
    colonna_localita = "CODITA_20N"
  )
  
  SU_liste_sim <- unita_liste_sim[
    ,
    .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)),
    by = .(SIM, SU20_COD, LISTA)
  ]
  
  CU_liste_sim <- unita_liste_sim[
    ,
    .(VOTI_LISTA_SIM = sum(VOTI_LISTA_SIM)),
    by = .(SIM, CU20_COD, LISTA)
  ]
  
}