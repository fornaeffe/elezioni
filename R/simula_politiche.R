# ATTENZIONE: questa funzione richiede almeno 16 GB di RAM!

simula_politiche <- function(
    scenario,
    data_elezione,
    simulazioni = 1000
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData")
  
  # DECRETO DEL PRESIDENTE DELLA REPUBBLICA 30 marzo 1957, n. 361
  # Art. 1
  # 2. Il territorio nazionale è diviso nelle circoscrizioni elettorali indicate
  # nella tabella A allegata al presente testo unico. Salvi i seggi assegnati
  # alla circoscrizione Estero e fermo restando quanto disposto dall'articolo 2,
  # nelle circoscrizioni del territorio nazionale sono costituiti ((un numero di
  # collegi uninominali pari ai tre ottavi del totale dei seggi da eleggere
  # nelle circoscrizioni elettorali di cui alla tabella A allegata al presente
  # testo unico, con arrotondamento all'unità inferiore,)) ripartiti in ciascuna
  # circoscrizione sulla base della popolazione; ((la circoscrizione
  # Trentino-Alto Adige/Südtirol è ripartita in un numero di collegi uninominali
  # pari alla metà dei seggi assegnati alla circoscrizione medesima, con
  # arrotondamento all'unità pari superiore. Le circoscrizioni cui sono
  # assegnati tre deputati sono ripartite in due collegi uninominali; le
  # circoscrizioni cui sono assegnati due deputati sono costituite in un
  # collegio uninominale)). 3. Per l'assegnazione degli altri seggi ciascuna
  # circoscrizione è ripartita in collegi plurinominali costituiti, di norma,
  # dall'aggregazione del territorio di collegi uninominali contigui e tali che
  # a ciascuno di essi sia assegnato, di norma, un numero di seggi non inferiore
  # a tre e non superiore a otto.
  
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
  
  # Aggiorno la popoplazione
  comuni <- base_dati[
    ,
    .(POP_2011 = sum(POP_2011)),
    by = CODICE_COMUNE
  ]
  
  comuni[
    dati$pop_legale,
    on = .(CODICE_COMUNE),
    POP_LEGALE := i.POPOLAZIONE
  ]
  
  comuni[
    ,
    FATTORE_CRESCITA_POP := POP_LEGALE / POP_2011
  ]
  
  base_dati[
    comuni,
    on = .(CODICE_COMUNE),
    POP_LEGALE := POP_2011 * i.FATTORE_CRESCITA_POP
  ]
  
  # TODO urgente: disambiguare i comuni omonimi di caricamento_dati.R
  
  
  # Calcolo i parametri di input per la generazione dei voti
  parametri_input <- calcola_parametri_input(dati, scenario)
  
  # setdiff(comuni$CODICE_COMUNE, parametri_input$comuni_liste$CODICE_COMUNE)
  
  # print(comuni[
  #   !parametri_input$comuni_liste,
  #   on = .(CODICE_COMUNE)
  # ][
  #   dati$ISTAT,
  #   on = .(CODICE_COMUNE = PRO_COM_T),
  #   COMUNE := i.COMUNE
  # ])
  
  
  # Art. 3.
  
  # 1. L'assegnazione del numero dei seggi alle singole circoscrizioni di cui
  # alla tabella A allegata al presente testo unico, è effettuata, sulla base
  # dei risultati dell'ultimo censimento generale della popolazione, riportati
  # dalla più recente pubblicazione ufficiale dell'Istituto nazionale di
  # statistica, con decreto del Presidente della Repubblica, su proposta del
  # Ministro dell'interno, da emanare contestualmente al decreto di convocazione
  # dei comizi.
  
  
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