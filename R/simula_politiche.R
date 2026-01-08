# ATTENZIONE: questa funzione richiede almeno 16 GB di RAM!

simula_politiche <- function(
    scenario,
    data_elezione,
    simulazioni = 1000
){
  # Carico i dati
  dati <- carica_dati(cache_path = "dati/dati.RData")
  
  # Calcolo i parametri di input per la generazione dei voti
  parametri_input <- calcola_parametri_input(dati, scenario)
  
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
  
  # DECRETO LEGISLATIVO 20 dicembre 1993, n. 533 
  # Art. 1 
  # 2. ((Il territorio
  # nazionale è suddiviso in un numero di collegi uninominali pari ai tre ottavi
  # del totale dei seggi da eleggere nelle circoscrizioni regionali, con
  # arrotondamento all'unità più prossima, assicurandone uno per ogni
  # circoscrizione. Fatti salvi i collegi uninominali delle regioni che eleggono
  # un solo senatore e quelli del Trentino-Alto Adige/Südtirol, i)) restanti
  # collegi uninominali sono ripartiti nelle altre regioni proporzionalmente
  # alla rispettiva popolazione. In tali collegi uninominali risulta eletto il
  # candidato che ha riportato il maggior numero di voti validi.
  
  # 2-bis. Per la assegnazione degli altri seggi ciascuna circoscrizione
  # regionale è ripartita in collegi plurinominali costituiti, di norma,
  # dall'aggregazione del territorio di collegi uninominali contigui e tali che
  # a ciascuno di essi sia assegnato, di norma, un numero di seggi non inferiore
  # a due e non superiore a otto. [...]
  
  # 3. ((Le regioni che eleggono un solo senatore sono costituite)) in unico
  # collegio uninominale.
  
  # 4. La regione Trentino-Alto Adige è costituita in sei collegi uninominali
  # definiti ai sensi della legge 30 dicembre 1991, n. 422 ((, ovvero in un
  # numero di collegi uninominali individuato nel numero pari più alto nel
  # limite dei seggi assegnati alla regione)). La restante quota di seggi
  # spettanti alla regione è attribuita con metodo del recupero proporzionale.
  
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
  
  # Aggiorno la popoplazione e gli elettori
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
  
  # Controllo che ci siano dati di elezione per ogni comune
  stopifnot(
    length(
      setdiff(
        comuni$CODICE_COMUNE, 
        parametri_input$comuni_liste$CODICE_COMUNE
      )
    ) == 0
  )
  
  comuni[
    parametri_input$comuni_liste,
    on = .(CODICE_COMUNE),
    ELETTORI := i.ELETTORI
  ]
  
  comuni[
    ,
    `:=`(
      FATTORE_CRESCITA_POP = POP_LEGALE / POP_2011,
      FATTORE_ELETTORI = ELETTORI / POP_LEGALE
    )
  ]
  
  base_dati[
    comuni,
    on = .(CODICE_COMUNE),
    `:=`(
      POP_LEGALE = round(POP_2011 * i.FATTORE_CRESCITA_POP),
      ELETTORI = round(POP_2011 * i.FATTORE_CRESCITA_POP * i.FATTORE_ELETTORI)
    )
  ]
  
  # COSTITUZIONE
  # Art. 56
  # La Camera dei deputati è eletta a suffragio universale e diretto. Il numero
  # dei deputati è di ((quattrocento)), ((otto)) dei quali eletti nella
  # circoscrizione Estero. [...]
  # La ripartizione dei seggi tra le circoscrizioni, fatto salvo il numero dei
  # seggi assegnati alla circoscrizione Estero, si effettua dividendo il numero
  # degli abitanti della Repubblica, quale risulta dall'ultimo censimento
  # generale della popolazione, per ((trecentonovantadue)) e distribuendo i
  # seggi in proporzione alla popolazione di ogni circoscrizione, sulla base dei
  # quozienti interi e dei più alti resti.
  
  # DECRETO DEL PRESIDENTE DELLA REPUBBLICA 30 marzo 1957, n. 361
  # Art. 3.
  # 1. L'assegnazione del numero dei seggi alle singole circoscrizioni di cui
  # alla tabella A allegata al presente testo unico, è effettuata, sulla base
  # dei risultati dell'ultimo censimento generale della popolazione, riportati
  # dalla più recente pubblicazione ufficiale dell'Istituto nazionale di
  # statistica, con decreto del Presidente della Repubblica, su proposta del
  # Ministro dell'interno, da emanare contestualmente al decreto di convocazione
  # dei comizi.
  
  # Ottengo i dati per ogni circoscrizione
  circocam <- base_dati[
    ,
    .(
      POP_LEGALE = sum(POP_LEGALE),
      CU = length(unique(CU20_COD))
    ),
    by = .(CIRCOCAM_20_COD, CIRCOCAM_20_DEN)
  ]
  
  # Distribuisco i seggi
  circocam[
    ,
    SEGGI := Hare.Niemeyer(POP_LEGALE, 392)
  ]
  
  # Calcolo i seggi da distribuire nei collegi plurinominali
  circocam[
    ,
    SEGGI_PLURI := SEGGI - CU
  ]
  
  # COSTITUZIONE
  
  # Art. 57
  # Il Senato della Repubblica è eletto a base regionale, salvi i seggi
  # assegnati alla circoscrizione Estero. Il numero dei senatori elettivi è di
  # ((duecento)), ((quattro)) dei quali eletti nella circoscrizione Estero.
  # ((20)) Nessuna Regione ((o Provincia autonoma)) può avere un numero di
  # senatori inferiore a ((tre)); il Molise ne ha due, la Valle d'Aosta uno.
  # ((20)). ((La ripartizione dei seggi tra le Regioni o le Province autonome,
  # previa applicazione delle disposizioni del precedente comma, si effettua in
  # proporzione alla loro popolazione, quale risulta dall'ultimo censimento
  # generale, sulla base dei quozienti interi e dei più alti resti)).
  
  # DECRETO LEGISLATIVO 20 dicembre 1993, n. 533
  
  # Art. 1
  
  # 1. Il Senato della Repubblica è eletto su base regionale. Salvo i seggi
  # assegnati alla circoscrizione Estero, i seggi sono ripartiti tra le regioni
  # a norma dell'articolo 57 della Costituzione sulla base dei risultati
  # dell'ultimo censimento generale della popolazione, riportati dalla più
  # recente pubblicazione ufficiale dell'Istituto nazionale di statistica, con
  # decreto del Presidente della Repubblica, da emanare, su proposta del
  # Ministro dell'interno, previa deliberazione del Consiglio dei ministri,
  # contemporaneamente al decreto di convocazione dei comizi.
  
  seggi_senato <- 200
  seggi_senato_estero <- 4
  seggi_senato_italia <- seggi_senato - seggi_senato_estero
  regioni_seggi_fissi <- c("Molise", "Valle d'Aosta")
  
  circosen <- base_dati[
    ,
    .(
      POP_LEGALE = sum(POP_LEGALE),
      SU = length(unique(SU20_COD))
    ),
    by = .(COD_REG20, DEN_REG20)
  ]
  
  # Assegno i seggi minimi per ciascuna regione
  circosen[, SEGGI_FISSI := 3]
  circosen[DEN_REG20 == "Molise", SEGGI_FISSI := 2]
  circosen[DEN_REG20 == "Valle d'Aosta", SEGGI_FISSI := 1]
  circosen[DEN_REG20 == "Trentino-Alto Adige", SEGGI_FISSI := 6]
  
  # Trovo la popolazione totale delle regioni i cui seggi non sono fissi:
  pop_seggi_variabili <- circosen[
    !(DEN_REG20 %in% regioni_seggi_fissi),
    sum(POP_LEGALE)
  ]
  
  # Trovo i seggi da assegnare alle regioni i cui seggi non sono fissi:
  seggi_regioni_non_fisse <- seggi_senato_italia - 
    circosen[
      DEN_REG20 %in% regioni_seggi_fissi,
      sum(SEGGI_FISSI)
    ]
  
  # Trovo il quoziente
  quoziente <- pop_seggi_variabili / seggi_regioni_non_fisse
  
  # Tolgo dalla popolazione legale quella necessaria ad attribuire i
  # seggi minimi, impostando il risultato a zero se negativo.
  circosen[
    !(DEN_REG20 %in% regioni_seggi_fissi), 
    pop_per_hn := pmax(POP_LEGALE - quoziente * SEGGI_FISSI, 0)
  ]
  
  # Imposto a zero la popolazione così modificata per le regioni a 
  # seggi fissi
  circosen[
    DEN_REG20 %in% regioni_seggi_fissi, 
    pop_per_hn := 0
  ]
  
  # Calcolo i seggi ancora da assegnare:
  seggi_variabili <- seggi_senato_italia - sum(circosen$SEGGI_FISSI)
  
  # Li distribuisco tra la popolazione che non ha ancora dato origine
  # a seggi
  circosen[
    ,
    SEGGI_VARIABILI := Hare.Niemeyer(pop_per_hn, seggi_variabili)
  ]
  
  # Sommo i seggi
  circosen[
    ,
    SEGGI := SEGGI_FISSI + SEGGI_VARIABILI
  ]
  
  # Calcolo i seggi da distribuire nei collegi plurinominali
  circosen[
    ,
    SEGGI_PLURI := SEGGI - SU
  ]
  
  
  
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