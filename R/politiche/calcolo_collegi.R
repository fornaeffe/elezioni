calcola_collegi <- function(
    dati
) {
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
  
  # TODO: passare più esplicitamente gli elettori da carica_dati
  
  data.table::setorder(dati$comuni_liste_elezioni, -DATA)
  ultime_elezioni <- dati$comuni_liste_elezioni[
    ,
    .(ELETTORI = sum(VOTI)),
    by = .(CODICE_COMUNE, ELEZIONE)
  ][
    ,
    .(ELETTORI = ELETTORI[1]),
    by = .(CODICE_COMUNE)
  ]
  
  # Controllo che ci siano dati di elezione per ogni comune
  stopifnot(
    length(
      setdiff(
        comuni$CODICE_COMUNE, 
        ultime_elezioni$CODICE_COMUNE
      )
    ) == 0
  )
  
  comuni[
    ultime_elezioni,
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
  
  # Calcolo i seggi plurinominali per ogni collegio
  camera <- calcolo_collegi_ramo("camera", base_dati)
  senato <- calcolo_collegi_ramo("senato", base_dati)
  
  return(
    list(
      base_dati = base_dati,
      camera = camera,
      senato = senato
    )
  )
}