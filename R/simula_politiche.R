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
  
  # Completo con i nomi e i codici aggiornati di provincia e regione
  # base_dati[
  #   dati$ISTAT,
  #   on = .(CODICE_COMUNE = PRO_COM_T),
  #   `:=`(
  #     COMUNE = i.COMUNE,
  #     CODICE_PROVINCIA = i.COD_UTS,
  #     PROVINCIA = i.DEN_UTS,
  #     CODICE_REGIONE = i.COD_REG,
  #     REGIONE = i.DEN_REG
  #   )
  # ]
  
  # Calcolo i parametri di input per la generazione dei voti
  parametri_input <- calcola_parametri_input(dati, scenario)
  
  
  
}