calcolo_collegi_ramo <- function(
  ramo,
  base_dati
) {
  if (!(ramo %in% c("camera", "senato"))) {
      stop("Argomento ramo non valido: ", ramo)
  }
      
  if (!data.table::is.data.table(base_dati)) {
    stop("base_dati deve essere un data.table")
  }
  
  bd <- data.table::copy(base_dati)
  
  nomi_colonne <- c(
    "CIRC_COD",
    "CIRC_DEN",
    "PLURI_COD",
    "PLURI_DEN",
    "UNI_COD",
    "UNI_DEN"
  )
  
  if (ramo == "camera") {
    data.table::setnames(
      bd,
      c(
        "CIRCOCAM_20_COD",
        "CIRCOCAM_20_DEN",
        "CP20_COD",
        "CP20_DEN",
        "CU20_COD",
        "CU20_DEN"
      ),
      nomi_colonne
    )
  }
  
  if (ramo == "senato") {
    data.table::setnames(
      bd,
      c(
        "COD_REG20",
        "DEN_REG20",
        "SP20_COD",
        "SP20_DEN",
        "SU20_COD",
        "SU20_DEN"
      ),
      nomi_colonne
    )
  }
  
  uni <- bd[
    ,
    .(POP_LEGALE = sum(POP_LEGALE)),
    by = .(
      CIRC_COD,
      CIRC_DEN,
      PLURI_COD,
      PLURI_DEN,
      UNI_COD,
      UNI_DEN
    )
  ]
  
  pluri <- uni[
    ,
    .(
      POP_LEGALE = sum(POP_LEGALE),
      NUM_UNI = length(unique(UNI_COD))
    ),
    by = .(
      CIRC_COD,
      CIRC_DEN,
      PLURI_COD,
      PLURI_DEN
    )
  ]
  
  circ <- pluri[
    ,
    .(
      POP_LEGALE = sum(POP_LEGALE),
      NUM_UNI = sum(NUM_UNI)
    ),
    by = .(
      CIRC_COD,
      CIRC_DEN
    )
  ]
  
  ### CAMERA
  
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
  
  if (ramo == "camera") {
    seggi <- 400
    seggi_estero <- 8
    seggi_italia <- seggi - seggi_estero
    
    circ[
      ,
      SEGGI := Hare.Niemeyer(POP_LEGALE, seggi_italia)
    ]
  }
  
  ### SENATO
  
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
  
  if (ramo == "senato") {
    seggi <- 200
    seggi_estero <- 4
    seggi_italia <- seggi - seggi_estero
    regioni_seggi_fissi <- c("Molise", "Valle d'Aosta")
    
    # Assegno i seggi fissi
    circ[CIRC_DEN == "Molise", SEGGI := 2]
    circ[CIRC_DEN == "Valle d'Aosta", SEGGI := 1]
    
    # Assegno i seggi minimi per ciascuna regione
    circ[
      !(CIRC_DEN %in% regioni_seggi_fissi),
      SEGGI_MINIMI := 3
    ]
    circ[CIRC_DEN == "Trentino-Alto Adige", SEGGI_MINIMI := 6]
    
    # Trovo i seggi da assegnare alle regioni i cui seggi non sono fissi:
    seggi_regioni_non_fisse <- seggi_italia - 
      circ[
        CIRC_DEN %in% regioni_seggi_fissi,
        sum(SEGGI)
      ]
    
    # Assegno i seggi alle regioni
    circ[
      !(CIRC_DEN %in% regioni_seggi_fissi),
      SEGGI := Hare.Niemeyer_con_minimo(POP_LEGALE, seggi_regioni_non_fisse, SEGGI_MINIMI)
    ]
    
    circ[,SEGGI_MINIMI := NULL]
  }
  
  # CAMERA
  # ((2. Con il medesimo decreto del Presidente della Repubblica di cui al comma
  # 1, sulla base dei risultati dell'ultimo censimento generale della
  # popolazione, riportati dalla più recente pubblicazione ufficiale
  # dell'Istituto nazionale di statistica, è determinato il numero complessivo
  # di seggi da attribuire in ciascuna circoscrizione nei collegi plurinominali,
  # compresi i seggi spettanti ai collegi uninominali)).
  
  # SENATO
  # 2-ter. Con il medesimo decreto del Presidente della Repubblica di cui al
  # comma 1, sulla base dei risultati dell'ultimo censimento generale della
  # popolazione, riportati dalla più recente pubblicazione ufficiale
  # dell'Istituto nazionale di statistica, è determinato il numero complessivo
  # di seggi da attribuire in ciascuna circoscrizione regionale nei collegi
  # plurinominali, compresi i seggi spettanti ai collegi uninominali.
  
  pluri[
    circ,
    on = .(CIRC_COD),
    SEGGI_CIRC := i.SEGGI
  ]
  
  pluri[
    ,
    SEGGI := Hare.Niemeyer_con_minimo(
      POP_LEGALE,
      SEGGI_CIRC[1L],
      NUM_UNI
    ),
    by = CIRC_COD
  ]
  
  pluri[
    ,
    SEGGI_PLURI := SEGGI - NUM_UNI
  ]
  
  # CAMERA
  
  # 3. In ogni collegio plurinominale ciascuna lista, all'atto della
  # presentazione, è composta da un elenco di candidati presentati secondo un
  # ordine numerico. Il numero dei candidati non può essere inferiore alla metà,
  # con arrotondamento all'unità superiore, dei seggi assegnati al collegio
  # plurinominale e non può essere superiore al limite massimo di seggi
  # assegnati al collegio plurinominale; in ogni caso, il numero dei candidati
  # non può essere inferiore a due né superiore a quattro. A pena di
  # inammissibilità, nella successione interna delle liste nei collegi
  # plurinominali, i candidati sono collocati secondo un ordine alternato di
  # genere.
  
  if (ramo == "camera") {
    pluri[
      ,
      MAX_CANDIDATI := ifelse(SEGGI_PLURI == 0, 0, pmax(2, pmin(4, SEGGI_PLURI)))
    ]
  }
  
  # SENATO
  
  # 4. In ogni collegio plurinominale ciascuna lista, all'atto della
  # presentazione, è composta da un elenco di candidati presentati secondo un
  # ordine numerico. Il numero dei candidati non può essere inferiore alla metà,
  # con arrotondamento all'unità superiore, dei seggi assegnati al collegio
  # plurinominale e non può essere superiore al numero dei seggi assegnati al
  # collegio plurinominale. In ogni caso il numero dei candidati non può essere
  # inferiore a due né superiore a quattro; nei collegi plurinominali in cui è
  # assegnato un solo seggio, la lista è composta da un solo candidato. A pena
  # di inammissibilità, nella successione interna delle liste nei collegi
  # plurinominali, i candidati sono collocati secondo un ordine alternato di
  # genere.
  
  if (ramo == "senato") {
    pluri[
      ,
      MAX_CANDIDATI := pmin(4, SEGGI_PLURI)
    ]
  }
  
  
  
  return(
    list(
      circ = circ,
      pluri = pluri,
      uni = uni
    )
  )
}
