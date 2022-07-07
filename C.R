C_scrutinio <- function(
    
  liste_uni,
  # CIRCOSCRIZIONE (factor)
  # COLLEGIOPLURINOMINALE (factor)
  # COLLEGIOUNINOMINALE (factor)
  # CANDIDATO (factor)
  # CAND_MINORANZA (logical)
  # LISTA (factor)
  # MINORANZA (logical)
  # VOTI_LISTA (integer)
  
  liste_naz,
  # LISTA (factor)
  # COALIZIONE (character) cambiare in factor?
  # MINORANZA (logical)
  
  candidati_uni,
  # CIRCOSCRIZIONE (factor)
  # COLLEGIOPLURINOMINALE (factor)
  # COLLEGIOUNINOMINALE (factor)
  # CANDIDATO (factor)
  # DATA_NASCITA (POSIXct)
  # VOTI_CANDIDATO (integer)
  
  candidati_pluri,
  # CIRCOSCRIZIONE (factor)
  # COLLEGIOPLURINOMINALE (factor)
  # LISTA (factor)
  # NUMERO (integer)
  # CANDIDATO (factor)
  
  totali_pluri,
  # CIRCOSCRIZIONE (factor)
  # COLLEGIOPLURINOMINALE (factor)
  # SEGGI (numeric)
  
  totale_seggi
  
) {
  
  source("condivisi.R", local = TRUE)
  
  ##### Art. 77 comma 1 lettera a - cifra candidati uninominali ####
  
  # Art. 77.
  # ((1. L'Ufficio centrale circoscrizionale, compiute le operazioni di
  # cui all'articolo 76, facendosi assistere, ove lo ritenga opportuno,
  #   da uno o piu' esperti scelti dal presidente:
  # a) determina la cifra elettorale individuale di ciascun candidato
  # nel collegio uninominale; tale cifra e' data dalla somma dei voti
  #   validi conseguiti dal candidato nelle singole sezioni elettorali del
  #   collegio uninominale;
  
  
  
  ##### Art. 77 comma 1 lettera b - elezione candidati uninominali ####
  
  # b) proclama eletto in ciascun collegio uninominale il candidato
  # che ha ottenuto il maggior numero di voti validi; in caso di parita',
  # e' eletto il candidato piu' giovane di eta';
  
  candidati_uni <- candidati_uni[order(
    candidati_uni$CIRCOSCRIZIONE, 
    candidati_uni$COLLEGIOPLURINOMINALE, 
    candidati_uni$COLLEGIOUNINOMINALE, 
    candidati_uni$VOTI_CANDIDATO,
    candidati_uni$DATA_NASCITA,
    decreasing = c("FALSE", "FALSE", "FALSE", "TRUE", "TRUE"),
    method = "radix"
  ), ]
  
  candidati_uni$ELETTO <- !duplicated(candidati_uni$COLLEGIOUNINOMINALE)
  
  ##### Art. 77 comma 1 lettera c - liste cifra uninominale ####
  
  # c) determina la cifra elettorale di collegio uninominale di
  # ciascuna lista. Tale cifra e' data dalla somma dei voti validi
  # conseguiti dalla lista stessa nelle singole sezioni elettorali del
  # collegio uninominale e dei voti espressi a favore dei soli candidati
  # nei collegi uninominali collegati a piu' liste in coalizione di cui
  # all'articolo 58, terzo comma, ultimo periodo, attribuiti alla lista a
  # seguito delle seguenti operazioni: l'Ufficio divide il totale dei
  # voti validi conseguiti da tutte le liste della coalizione nel
  # collegio uninominale per il numero dei voti espressi a favore dei
  # soli candidati nei collegi uninominali, ottenendo il quoziente di
  # ripartizione. Divide poi il totale dei voti validi conseguiti da
  # ciascuna lista per tale quoziente. La parte intera del quoziente
  # cosi' ottenuto rappresenta il numero dei voti da assegnare a ciascuna
  # lista; i voti che rimangono ancora da attribuire sono rispettivamente
  # assegnati alle liste per le quali queste ultime divisioni abbiano
  # dato i maggiori resti, secondo l'ordine decrescente dei resti
  # medesimi. Nella ripartizione dei voti espressi in favore dei soli
  # candidati nei collegi uninominali collegati a piu' liste in
  # coalizione, l'Ufficio esclude dal computo i voti espressi in favore
  # della lista rappresentativa di minoranze linguistiche riconosciute
  # nei collegi uninominali dove questa ha presentato proprie candidature
  # ai sensi dell'articolo 18-bis, comma 1-bis;
  
  candidati_uni <- merge(
    candidati_uni,
    aggregate(
      VOTI_LISTA ~ COLLEGIOUNINOMINALE + CANDIDATO, 
      liste_uni,
      sum
    ),
  )
  
  candidati_uni$VOTI_SOLO_CANDIDATO <-
    candidati_uni$VOTI_CANDIDATO - candidati_uni$VOTI_LISTA
  
  candidati_uni$QUOZIENTE <-
    candidati_uni$VOTI_LISTA / candidati_uni$VOTI_SOLO_CANDIDATO
  
  
  liste_uni <- merge(
    liste_uni, 
    candidati_uni[, c(
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "ELETTO",
      "QUOZIENTE"
    )],
    all.x = TRUE
  )
  
  liste_uni$PARTE_INTERA <- 
    liste_uni$VOTI_LISTA %/% liste_uni$QUOZIENTE
  liste_uni$RESTO <- 
    liste_uni$VOTI_LISTA %% liste_uni$QUOZIENTE
  
  liste_uni$PARTE_INTERA[
    liste_uni$PARTE_INTERA < 0 |
      is.na(liste_uni$PARTE_INTERA) | 
      is.nan(liste_uni$PARTE_INTERA)
  ] <- 0
  
  candidati_uni <- merge(
    candidati_uni,
    aggregate(
      PARTE_INTERA ~ COLLEGIOUNINOMINALE + CANDIDATO,
      liste_uni,
      sum
    )
  )
  candidati_uni$DA_ASSEGNARE <-
    candidati_uni$VOTI_SOLO_CANDIDATO - candidati_uni$PARTE_INTERA
  
  liste_uni <- merge(
    liste_uni, 
    candidati_uni[, c("COLLEGIOUNINOMINALE", "CANDIDATO", "DA_ASSEGNARE")],
    all.x = TRUE
  )
  
  liste_uni <- liste_uni[order(
    liste_uni$CIRCOSCRIZIONE, 
    liste_uni$COLLEGIOPLURINOMINALE, 
    liste_uni$COLLEGIOUNINOMINALE,
    liste_uni$CANDIDATO,  
    liste_uni$RESTO,
    decreasing = c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE"),
    method = "radix"
  ), ]
  
  liste_uni$ORDINE <- ave(
    seq_along(liste_uni$CIRCOSCRIZIONE),
    paste(liste_uni$COLLEGIOUNINOMINALE, liste_uni$CANDIDATO),
    FUN = seq_along
  )
  
  liste_uni$VOTO_DA_RESTO <- liste_uni$ORDINE <= liste_uni$DA_ASSEGNARE
  
  liste_uni$VOTO_DA_RESTO[
    is.na(liste_uni$VOTO_DA_RESTO) | is.nan(liste_uni$VOTO_DA_RESTO)
  ] <- 0
  
  liste_uni$CIFRA <- 
    liste_uni$VOTI_LISTA + 
    liste_uni$PARTE_INTERA + 
    liste_uni$VOTO_DA_RESTO
  
  ##### Art. 77 comma 1 lettera d - liste cifra plurinominale ####
  
  # d) determina la cifra elettorale di collegio plurinominale di
  # ciascuna lista. Tale cifra e' data dalla somma delle cifre elettorali
  # di collegio uninominale di ciascuna lista;
  
  liste_pluri <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
    liste_uni,
    sum
  )
  
  ##### Art. 77 comma 1 lettera e - liste cifra % uninominale ####
  
  # e) determina la cifra elettorale percentuale di collegio
  # plurinominale di ciascuna lista. Tale cifra e' data dal quoziente
  # risultante dalla divisione della cifra elettorale di collegio
  # plurinominale di ciascuna lista per il totale dei voti validi del
  # rispettivo collegio plurinominale, moltiplicato per cento;
  
  liste_pluri <- merge(
    liste_pluri,
    aggregate(
      CIFRA ~ COLLEGIOPLURINOMINALE,
      liste_pluri,
      sum
    ),
    by = "COLLEGIOPLURINOMINALE",
    suffixes = c("", "_TOT")
  )
  
  liste_pluri$CIFRA_PERCENTUALE <- 
    liste_pluri$CIFRA / liste_pluri$CIFRA_TOT * 100
  
  ##### Art. 77 comma 1 lettera f - liste cifra circoscrizionale ####
  
  # f) determina la cifra elettorale circoscrizionale di ciascuna
  # lista. Tale cifra e' data dalla somma delle cifre elettorali di
  # collegio plurinominale della lista stessa;
  
  liste_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + LISTA,
    liste_pluri, 
    sum
  )
  
  ##### Art. 77 comma 1 lettera g - cifra % candidato uninominale ####
  
  # g) determina la cifra elettorale percentuale di ciascun candidato
  # nel collegio uninominale. Tale cifra e' data dal quoziente risultante
  # dalla divisione della cifra elettorale individuale di ciascun
  # candidato per il totale dei voti validi del rispettivo collegio
  # uninominale, moltiplicato per cento;
  
  candidati_uni <- merge(
    candidati_uni,
    aggregate(
      VOTI_CANDIDATO ~ COLLEGIOUNINOMINALE,
      candidati_uni,
      sum
    ),
    by = "COLLEGIOUNINOMINALE",
    suffixes = c("", "_TOT")
  )
  
  candidati_uni$CIFRA_PERCENTUALE <- 
    candidati_uni$VOTI_CANDIDATO / candidati_uni$VOTI_CANDIDATO_TOT * 100
  
  ##### Art. 77 comma 1 lettera h - graduatoria candidati uninominale ####
  
  # h) determina, per ciascuna lista, la graduatoria dei candidati
  # nei collegi uninominali della circoscrizione non proclamati eletti,
  # disponendoli nell'ordine delle rispettive cifre elettorali
  # individuali percentuali. A parita' di cifre individuali percentuali,
  # prevale il piu' giovane di eta'. In caso di collegamento dei
  # candidati con piu' liste, i candidati entrano a far parte della
  # graduatoria relativa a ciascuna delle liste con cui e' stato
  # dichiarato il collegamento;
  
  # Operazioni non svolte qui, ma nell'art. 84 comma 3
  
  ##### Art. 77 comma 1 lettera i - totali circoscrizione ####
  
  # i) determina il totale dei voti validi della circoscrizione. Tale
  # totale e' dato dalla somma delle cifre elettorali circoscrizionali di
  # tutte le liste;
  
  totali_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE,
    liste_circ,
    sum
  )
  
  # l) comunica all'Ufficio centrale nazionale, a mezzo di estratto
  # del verbale, la cifra elettorale circoscrizionale di ciascuna lista
  # nonche' il totale dei voti validi della circoscrizione)).
  
  ##### Art. 83 comma 1 lettera a - cifra naz liste ####
  
  # Art. 83.
  # 1. L'Ufficio centrale nazionale, ricevuti gli estratti dei verbali
  # da tutti gli Uffici centrali circoscrizionali, facendosi assistere,
  # ove lo ritenga opportuno, da uno o piu' esperti scelti dal
  # presidente:
  #   a) determina la cifra elettorale nazionale di ciascuna lista.
  # Tale cifra e' data dalla somma delle cifre elettorali
  # circoscrizionali conseguite nelle singole circoscrizioni dalle liste
  # aventi il medesimo contrassegno;
  
  liste_naz <- merge(
    liste_naz,
    aggregate(
      CIFRA ~ LISTA,
      liste_circ,
      sum
    )
  )
  
  ##### Art. 83 comma 1 lettera b - totale naz ####
  
  # b) determina il totale nazionale dei voti validi. Esso e' dato
  # dalla somma delle cifre elettorali circoscrizionali di tutte le
  # liste;
  totale_naz <- sum(liste_naz$CIFRA)
  
  ##### Art. 83 comma 1 lettera c - cifra naz coalizioni ####
  
  # c) determina la cifra elettorale nazionale di ciascuna coalizione
  # di liste. Tale cifra e' data dalla somma delle cifre elettorali
  # nazionali delle liste collegate in coalizione. Non concorrono alla
  # determinazione della cifra elettorale nazionale di coalizione i voti
  # espressi a favore delle liste collegate che abbiano conseguito sul
  # piano nazionale un numero di voti validi inferiore all'1 per cento
  # del totale, fatto salvo, per le liste rappresentative di minoranze
  # linguistiche riconosciute, quanto previsto alla lettera e);
  
  liste_naz$CIFRA_PERCENTUALE <- liste_naz$CIFRA / totale_naz * 100
  
  liste_naz$SOGLIA1 <- liste_naz$CIFRA_PERCENTUALE >= 1
  
  liste_circ <- merge(
    liste_circ,
    totali_circ,
    by = "CIRCOSCRIZIONE",
    suffixes = c("","_TOT")
  )
  
  liste_circ$CIFRA_PERCENTUALE <- liste_circ$CIFRA / liste_circ$CIFRA_TOT * 100
  
  if (sum(liste_uni$CAND_MINORANZA) > 0) {
    liste_circ <- merge(
      liste_circ,
      aggregate(
        ELETTO ~ CIRCOSCRIZIONE + LISTA,
        liste_uni[liste_uni$CAND_MINORANZA,],
        sum
      ),
      all.x = TRUE
    )
    names(liste_circ)[names(liste_circ) == "ELETTO"] <- "ELETTI_MINORANZA"
  } else {
    liste_circ$ELETTI_MINORANZA <- 0
  }
  
  
  liste_circ$ELETTI_MINORANZA[is.na(liste_circ$ELETTI_MINORANZA)] <- 0
  
  
  liste_circ <- merge(
    liste_circ,
    aggregate(
      COLLEGIOUNINOMINALE ~ CIRCOSCRIZIONE,
      unique(liste_uni[, c("CIRCOSCRIZIONE", "COLLEGIOUNINOMINALE")]),
      length
    )
  )
  names(liste_circ)[names(liste_circ) == "COLLEGIOUNINOMINALE"] <- "COLLEGI_UNI"
  
  
  liste_naz$SOGLIA_MINORANZA <- 
    liste_naz$MINORANZA &
    liste_naz$LISTA %in% liste_circ$LISTA[
      liste_circ$CIFRA_PERCENTUALE >= 20 |
        liste_circ$ELETTI_MINORANZA >= ceiling(liste_circ$COLLEGI_UNI / 4)
    ]
  
  liste_naz$SOGLIA1M <- liste_naz$SOGLIA1 | liste_naz$SOGLIA_MINORANZA
  
  coal_naz <- aggregate(
    CIFRA ~ COALIZIONE,
    data = liste_naz,
    sum,
    subset = SOGLIA1M
  )
  
  ##### Art. 83 comma 1 lettera d - cifra circoscrizionale coalizioni ####
  
  # d) determina la cifra elettorale circoscrizionale di ciascuna
  # coalizione di liste. Tale cifra e' data dalla somma delle cifre
  # elettorali circoscrizionali delle liste collegate tra loro in
  # coalizione, individuate ai sensi dell'ultimo periodo della lettera
  # c);
  
  # Inutile
  
  ##### Art. 83 comma 1 lettera e - soglie di sbarramento ####
  
  # e) individua quindi:
  #   1) le coalizioni di liste che abbiano conseguito sul piano
  # nazionale almeno il 10 per cento dei voti validi espressi e che
  # comprendano almeno una lista collegata che abbia conseguito sul piano
  # nazionale almeno il 3 per cento dei voti validi espressi ovvero una
  # lista collegata rappresentativa di minoranze linguistiche
  # riconosciute, presentata esclusivamente in una regione ad autonomia
  # speciale il cui statuto o le relative norme di attuazione prevedano
  # una particolare tutela di tali minoranze linguistiche, che abbia
  # conseguito almeno il 20 per cento dei voti validi espressi nella
  # regione medesima o i cui candidati siano stati proclamati eletti in
  # almeno ((un quarto dei collegi uninominali della circoscrizione ai
  #          sensi dell'articolo 77, con arrotondamento all'unita' superiore));
  
  coal_naz$CIFRA_PERCENTUALE <- 
    coal_naz$CIFRA / totale_naz * 100
  
  coal_naz$SOGLIA10 <- coal_naz$CIFRA_PERCENTUALE >= 10
  
  liste_naz$SOGLIA3M <- 
    liste_naz$CIFRA_PERCENTUALE >= 3 | liste_naz$SOGLIA_MINORANZA
  
  coal_naz <- merge(
    coal_naz,
    aggregate(
      SOGLIA3M ~ COALIZIONE,
      data = liste_naz,
      function(x) Reduce("|", x)
    )
  )
  
  coal_naz$SOGLIA_COALIZIONE <- 
    coal_naz$SOGLIA10 & 
    coal_naz$SOGLIA3M
  
  
  # 2) le singole liste non collegate, o collegate in coalizioni
  # che non abbiano raggiunto la percentuale di cui al numero 1), che
  # abbiano conseguito sul piano nazionale almeno il 3 per cento dei voti
  # validi espressi, nonche' le singole liste non collegate e le liste
  #          collegate in coalizioni che non abbiano raggiunto la percentuale di
  #          cui al numero 1), rappresentative di minoranze linguistiche
  #         riconosciute, presentate esclusivamente in una regione ad autonomia
  #         speciale il cui statuto o le relative norme di attuazione prevedano
  #         una particolare tutela di tali minoranze linguistiche, che abbiano
  #         conseguito almeno il 20 per cento dei voti validi espressi nella
  #         regione medesima o i cui candidati siano stati proclamati eletti in
  #         almeno ((un quarto dei collegi uninominali della circoscrizione ai
  #          sensi dell'articolo 77, con arrotondamento all'unita' superiore));
  
  liste_naz <- merge(
    liste_naz,
    coal_naz[, c("COALIZIONE", "SOGLIA_COALIZIONE")],
    all.x = TRUE
  )
  
  liste_naz$SOGLIA_SOLA <- 
    (is.na(liste_naz$COALIZIONE) | !liste_naz$SOGLIA_COALIZIONE) &
    liste_naz$SOGLIA3M
  
  ##### Art. 83 comma 1 lettera f - riparto nazionale ####
  
  # f) procede al riparto ((dei seggi assegnati nelle circoscrizioni
  # del territorio nazionale, con esclusione del seggio assegnato alla
  # circoscrizione Valle d'Aosta)); a tale fine, detrae i ((...)) seggi
  #         gia' attribuiti ai candidati proclamati eletti nei collegi
  # uninominali ai sensi dell'articolo 77, comma 1, lettera b), e procede
  # al riparto dei restanti seggi tra le coalizioni di liste e le singole
  # liste di cui alla lettera e) del presente comma in base alla cifra
  # elettorale nazionale di ciascuna di esse, fatto salvo quanto previsto
  # all'articolo 92, primo comma. A tale fine divide il totale delle
  # cifre elettorali nazionali delle coalizioni di liste e delle singole
  # liste di cui alla lettera e) del presente comma per il numero dei
  # seggi da attribuire, ottenendo cosi' il quoziente elettorale
  # nazionale. Nell'effettuare tale divisione non tiene conto
  # dell'eventuale parte frazionaria del quoziente. Divide poi la cifra
  # elettorale nazionale di ciascuna coalizione di liste o singola lista
  # per tale quoziente. La parte intera del quoziente cosi' ottenuto
  # rappresenta il numero dei seggi da assegnare a ciascuna coalizione di
  # liste o singola lista. I seggi che rimangono ancora da attribuire
  # sono rispettivamente assegnati alle coalizioni di liste o singole
  # liste per le quali queste ultime divisioni abbiano dato i maggiori
  # resti, secondo l'ordine decrescente dei resti medesimi, e, in caso di
  # parita' di resti, a quelle che abbiano conseguito la maggiore cifra
  # elettorale nazionale; a parita' di quest'ultima si procede a
  # sorteggio;
  
  seggi_proporzionale <- totale_seggi - sum(candidati_uni$ELETTO) - 1
  
  liste_naz$SOGGETTO_RIPARTO <- NA
  
  liste_naz$SOGGETTO_RIPARTO[which(liste_naz$SOGLIA_COALIZIONE)] <- 
    as.character(liste_naz$COALIZIONE[which(liste_naz$SOGLIA_COALIZIONE)])
  
  liste_naz$SOGGETTO_RIPARTO[which(liste_naz$SOGLIA_SOLA)] <- 
    as.character(liste_naz$LISTA[which(liste_naz$SOGLIA_SOLA)])
  
  liste_naz$SOGGETTO_RIPARTO <- as.factor(liste_naz$SOGGETTO_RIPARTO)
  
  riparto_naz <- aggregate(
    CIFRA ~ SOGGETTO_RIPARTO,
    liste_naz,
    sum,
    subset = SOGLIA1M
  )
  
  totale_naz_riparto <- sum(riparto_naz$CIFRA)
  quoziente_elettorale_naz <-totale_naz_riparto %/% seggi_proporzionale
  
  riparto_naz$PARTE_INTERA <- riparto_naz$CIFRA %/% quoziente_elettorale_naz
  riparto_naz$RESTO <- riparto_naz$CIFRA %% quoziente_elettorale_naz
  
  ancora_da_attribuire <- seggi_proporzionale - sum(riparto_naz$PARTE_INTERA)
  
  riparto_naz <- riparto_naz[
    order(riparto_naz$RESTO, riparto_naz$CIFRA, decreasing = TRUE),
  ]
  
  riparto_naz$ORDINE <- seq_along(riparto_naz$RESTO)
  
  riparto_naz$SEGGIO_DA_RESTO <- riparto_naz$ORDINE <= ancora_da_attribuire
  
  riparto_naz$SEGGI <- riparto_naz$PARTE_INTERA + riparto_naz$SEGGIO_DA_RESTO
  
  ##### Art. 83 comma 1 lettera g - riparto interno alle coalizioni ####
  
  # g) procede, per ciascuna coalizione di liste, al riparto dei
  # seggi fra le liste collegate che abbiano conseguito sul piano
  # nazionale almeno il 3 per cento dei voti validi espressi nonche' fra
  # le liste collegate rappresentative di minoranze linguistiche
  # riconosciute, presentate esclusivamente in una regione ad autonomia
  # speciale il cui statuto o le relative norme di attuazione prevedano
  # una particolare tutela di tali minoranze linguistiche, che abbiano
  # conseguito almeno il 20 per cento dei voti validi espressi nella
  # regione medesima o i cui candidati siano stati proclamati eletti in
  # almeno ((un quarto dei collegi uninominali della circoscrizione ai
  # sensi dell'articolo 77, con arrotondamento all'unita' superiore)). A
  # tale fine, divide la somma delle cifre elettorali delle liste ammesse
  # al riparto per il numero di seggi gia' individuato ai sensi della
  #          lettera f) del presente comma. Nell'effettuare tale divisione non
  # tiene conto dell'eventuale parte frazionaria del quoziente cosi'
  # ottenuto.
  
  ammesse_naz <- liste_naz[
    liste_naz$SOGLIA3M,
    c(
      "SOGGETTO_RIPARTO",
      "LISTA",
      "CIFRA"
    )
  ]
  
  riparto_naz <- merge(
    riparto_naz,
    aggregate(
      CIFRA ~ SOGGETTO_RIPARTO,
      data = ammesse_naz,
      sum
    ),
    by = "SOGGETTO_RIPARTO",
    suffixes = c("","_AMMESSE_AL_RIPARTO")
  )
  
  riparto_naz$QUOZIENTE <- 
    riparto_naz$CIFRA_AMMESSE_AL_RIPARTO %/% riparto_naz$SEGGI
  
  
  # Divide poi la cifra elettorale nazionale di ciascuna lista
  # ammessa al riparto per tale quoziente. La parte intera del quoziente
  # cosi' ottenuto rappresenta il numero dei seggi da assegnare a
  #         ciascuna lista. I seggi che rimangono ancora da attribuire sono
  #         rispettivamente assegnati alle liste per le quali queste ultime
  #         divisioni abbiano dato i maggiori resti e, in caso di parita' di
  # resti, alle liste che abbiano conseguito la maggiore cifra elettorale
  # nazionale; a parita' di quest'ultima si procede a sorteggio;
  
  ammesse_naz <- merge(
    ammesse_naz,
    riparto_naz[, c("SOGGETTO_RIPARTO", "QUOZIENTE")]
  )
  
  ammesse_naz$PARTE_INTERA <- ammesse_naz$CIFRA %/% ammesse_naz$QUOZIENTE
  ammesse_naz$RESTO <- ammesse_naz$CIFRA %% ammesse_naz$QUOZIENTE
  
  riparto_naz <- merge(
    riparto_naz,
    aggregate(
      PARTE_INTERA ~ SOGGETTO_RIPARTO,
      data = ammesse_naz,
      sum
    ),
    by = "SOGGETTO_RIPARTO",
    suffixes = c("", "_TOT")
  )
  
  riparto_naz$DA_ASSEGNARE <- riparto_naz$SEGGI - riparto_naz$PARTE_INTERA_TOT
  
  ammesse_naz <- merge(
    ammesse_naz,
    riparto_naz[, c("SOGGETTO_RIPARTO", "DA_ASSEGNARE")]
  )
  
  ammesse_naz <- ammesse_naz[order(
    ammesse_naz$SOGGETTO_RIPARTO,
    ammesse_naz$RESTO,
    ammesse_naz$CIFRA,
    decreasing = c(FALSE, TRUE, TRUE),
    method = "radix"
  ), ]
  
  ammesse_naz$ORDINE <- ave(
    seq_along(ammesse_naz$SOGGETTO_RIPARTO),
    ammesse_naz$SOGGETTO_RIPARTO,
    FUN = seq_along
  )
  
  ammesse_naz$SEGGIO_DA_RESTO <- ammesse_naz$ORDINE <= ammesse_naz$DA_ASSEGNARE
  
  ammesse_naz$SEGGI <- ammesse_naz$PARTE_INTERA + ammesse_naz$SEGGIO_DA_RESTO
  
  ##### Art. 83 comma 1 lettera h - riparto circoscrizionale ####
  
  # h) procede quindi alla distribuzione nelle singole circoscrizioni
  # dei seggi assegnati alle coalizioni di liste o singole liste di cui
  # alla lettera e). A tale fine determina il numero di seggi da
  # attribuire in ciascuna circoscrizione sottraendo dal numero dei seggi
  # spettanti alla circoscrizione stessa ai sensi dell'articolo 3, comma
  # 1, il numero dei collegi uninominali costituiti nella circoscrizione.
  totali_circ <- merge(
    totali_circ,
    aggregate(
      SEGGI ~ CIRCOSCRIZIONE,
      totali_pluri,
      sum
    )
  )
  
  #         Divide quindi la somma delle cifre elettorali circoscrizionali delle
  #         coalizioni di liste e delle singole liste ammesse al riparto per il
  #         numero di seggi da attribuire nella circoscrizione, ottenendo cosi'
  # il quoziente elettorale circoscrizionale. Nell'effettuare tale
  #         divisione non tiene conto dell'eventuale parte frazionaria del
  # quoziente cosi' ottenuto.
  liste_circ <- merge(
    liste_circ,
    liste_naz[
      ,
      c(
        "LISTA",
        "SOGGETTO_RIPARTO",
        "SOGLIA1M",
        "SOGLIA3M"
      )
    ]
  )
  
  riparto_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
    data = liste_circ,
    sum,
    subset = SOGLIA1M
  )
  
  totali_circ <- merge(
    totali_circ,
    aggregate(
      CIFRA ~ CIRCOSCRIZIONE,
      data = riparto_circ,
      sum
    ),
    by = "CIRCOSCRIZIONE",
    suffixes = c("","_AMMESSE_AL_RIPARTO")
  )
  
  totali_circ$QUOZIENTE <- 
    totali_circ$CIFRA_AMMESSE_AL_RIPARTO %/% totali_circ$SEGGI
  
  # Divide poi la cifra elettorale
  #         circoscrizionale di ciascuna coalizione di liste o singola lista per
  #         il quoziente elettorale circoscrizionale, ottenendo cosi' il
  # quoziente di attribuzione. La parte intera del quoziente di
  # attribuzione rappresenta il numero dei seggi da assegnare a ciascuna
  # coalizione di liste o singola lista.
  
  riparto_circ <- merge(
    riparto_circ,
    totali_circ[,c("CIRCOSCRIZIONE", "QUOZIENTE")]
  )
  
  riparto_circ$PARTE_INTERA <- riparto_circ$CIFRA %/% riparto_circ$QUOZIENTE
  riparto_circ$DECIMALI <- ( riparto_circ$CIFRA / riparto_circ$QUOZIENTE ) %% 1
  
  # I seggi che rimangono ancora da
  # attribuire sono rispettivamente assegnati alle coalizioni di liste o
  # singole liste per le quali queste ultime divisioni hanno dato le
  # maggiori parti decimali e, in caso di parita', alle coalizioni di
  #         liste o singole liste che hanno conseguito la maggiore cifra
  #         elettorale nazionale; a parita' di quest'ultima si procede a
  #         sorteggio. Esclude dall'attribuzione di cui al periodo precedente le
  # coalizioni di liste o singole liste alle quali e' stato gia'
  # attribuito il numero di seggi ad esse assegnato a seguito delle
  # operazioni di cui alla lettera f).
  
  riparto_naz <- merge(
    riparto_naz,
    aggregate(
      PARTE_INTERA ~ SOGGETTO_RIPARTO,
      data = riparto_circ,
      sum
    ),
    by = "SOGGETTO_RIPARTO",
    suffixes = c("","_CIRC")
  )
  
  riparto_naz$ESCLUSE <- riparto_naz$PARTE_INTERA_CIRC >= riparto_naz$SEGGI
  
  riparto_circ <- merge(
    riparto_circ,
    riparto_naz[,c("SOGGETTO_RIPARTO", "ESCLUSE", "CIFRA")],
    by = "SOGGETTO_RIPARTO",
    suffixes = c("", "_NAZ")
  )
  
  totali_circ <- merge(
    totali_circ,
    aggregate(
      PARTE_INTERA ~ CIRCOSCRIZIONE,
      data = riparto_circ,
      sum
    )
  )
  
  totali_circ$DA_ASSEGNARE <- totali_circ$SEGGI - totali_circ$PARTE_INTERA
  
  riparto_circ <- merge(
    riparto_circ,
    totali_circ[,c("CIRCOSCRIZIONE", "DA_ASSEGNARE")]
  )
  
  riparto_circ <- riparto_circ[
    order(
      riparto_circ$CIRCOSCRIZIONE,
      riparto_circ$ESCLUSE,
      riparto_circ$DECIMALI,
      riparto_circ$CIFRA_NAZ,
      decreasing = c(FALSE, FALSE, TRUE, TRUE),
      method = "radix"
    ),
  ]
  
  riparto_circ$ORDINE[!riparto_circ$ESCLUSE] <- ave(
    seq_along(riparto_circ$CIRCOSCRIZIONE[!riparto_circ$ESCLUSE]),
    riparto_circ$CIRCOSCRIZIONE[!riparto_circ$ESCLUSE],
    FUN = seq_along
  )
  
  riparto_circ$SEGGIO_DA_DECIMALI <- 
    riparto_circ$ORDINE <= riparto_circ$DA_ASSEGNARE
  riparto_circ$SEGGIO_DA_DECIMALI[is.na(riparto_circ$SEGGIO_DA_DECIMALI)] <- 
    FALSE
  
  riparto_circ$SEGGI <- 
    riparto_circ$PARTE_INTERA + riparto_circ$SEGGIO_DA_DECIMALI
  
  # Successivamente l'Ufficio accerta
  #   se il numero dei seggi assegnati in tutte le circoscrizioni a
  #   ciascuna coalizione di liste o singola lista corrisponda al numero di
  #   seggi determinato ai sensi della lettera f).
  
  riparto_naz <- merge(
    riparto_naz,
    aggregate(
      SEGGI ~ SOGGETTO_RIPARTO,
      data = riparto_circ,
      sum
    ),
    by = "SOGGETTO_RIPARTO",
    suffixes = c("", "_CIRC")
  )
  
  # In caso negativo,
  # procede alle seguenti operazioni, iniziando dalla coalizione di liste
  # o singola lista che abbia il maggior numero di seggi eccedenti e, in
  # caso di parita' di seggi eccedenti da parte di piu' coalizioni di
  # liste o singole liste, da quella che abbia ottenuto la maggiore cifra
  # elettorale nazionale, proseguendo poi con le altre coalizioni di
  # liste o singole liste in ordine decrescente di seggi eccedenti:
  #   sottrae i seggi eccedenti alla coalizione di liste o singola lista
  # nelle circoscrizioni nelle quali essa li ha ottenuti con le parti
  # decimali dei quozienti di attribuzione, secondo il loro ordine
  # crescente, e nelle quali inoltre le coalizioni di liste o singole
  # liste, che non abbiano ottenuto il numero di seggi spettante, abbiano
  # parti decimali dei quozienti non utilizzate. Conseguentemente,
  # assegna i seggi a tali coalizioni di liste o singole liste. Qualora
  # nella medesima circoscrizione due o piu' coalizioni di liste o
  # singole liste abbiano parti decimali dei quozienti non utilizzate, il
  # seggio e' attribuito alla coalizione di liste o alla singola lista
  # con la piu' alta parte decimale del quoziente non utilizzata o, in
  # caso di parita', a quella con la maggiore cifra elettorale nazionale.
  # Nel caso in cui non sia possibile attribuire il seggio eccedentario
  # nella medesima circoscrizione, in quanto non vi siano coalizioni di
  # liste o singole liste deficitarie con parti decimali di quozienti non
  # utilizzate, l'Ufficio prosegue, per la stessa coalizione di liste o
  # singola lista eccedentaria, nell'ordine dei decimali crescenti, a
  # individuare un'altra circoscrizione, fino a quando non sia possibile
  # sottrarre il seggio eccedentario e attribuirlo ad una coalizione di
  # liste o singola lista deficitaria nella medesima circoscrizione. Nel
  # caso in cui non sia possibile fare riferimento alla medesima
  # circoscrizione ai fini del completamento delle operazioni precedenti,
  # fino a concorrenza dei seggi ancora da cedere, alla coalizione di
  # liste o singola lista eccedentaria vengono sottratti i seggi nelle
  # circoscrizioni nelle quali li ha ottenuti con le minori parti
  # decimali del quoziente di attribuzione e alla coalizione di liste o
  # singola lista deficitaria sono conseguentemente attribuiti seggi
  # nelle altre circoscrizioni nelle quali abbia le maggiori parti
  # decimali del quoziente di attribuzione non utilizzate;
  
  riparto_naz$SEGGI_ECCEDENTI <- riparto_naz$SEGGI_CIRC - riparto_naz$SEGGI
  
  riparto_naz <- riparto_naz[
    order(
      riparto_naz$SEGGI_ECCEDENTI,
      riparto_naz$CIFRA,
      decreasing = TRUE
    ),
  ]
  
  riparto_naz$SEGGI_ECCEDENTI_CONTATORE <- riparto_naz$SEGGI_ECCEDENTI
  
  riparto_circ$FLIPPER <- 0
  
  for (i in seq_along(riparto_naz$SOGGETTO_RIPARTO)) {
    if (riparto_naz$SEGGI_ECCEDENTI[i] < 1) break
    
    s <- riparto_naz$SOGGETTO_RIPARTO[i]
    for (j in 1:riparto_naz$SEGGI_ECCEDENTI[i]) {
      riparto_circ$DEFICIT <- 
        riparto_circ$SOGGETTO_RIPARTO %in% riparto_naz$SOGGETTO_RIPARTO[
          riparto_naz$SEGGI_ECCEDENTI_CONTATORE < 0
        ] & !riparto_circ$SEGGIO_DA_DECIMALI & riparto_circ$FLIPPER == 0
      
      rc <- riparto_circ[
        riparto_circ$SOGGETTO_RIPARTO == s &
          riparto_circ$SEGGIO_DA_DECIMALI & 
          riparto_circ$FLIPPER == 0,
      ]
      
      if (dim(rc)[1] < 1) stop(
        "Devo togliere un seggio eccedente ma non ci sono circoscrizioni dove 
      questo sia stato ottenuto con i resti"
      )
      
      rc$DEFICIT_PRESENTE <- rc$CIRCOSCRIZIONE %in% riparto_circ$CIRCOSCRIZIONE[
        riparto_circ$DEFICIT
      ]
      
      rc <- rc[
        order(
          rc$DEFICIT_PRESENTE,
          rc$DECIMALI,
          decreasing = c(TRUE, FALSE),
          method = "radix"
        ),
      ]
      
      c <- rc$CIRCOSCRIZIONE[1]
      
      if (rc$DEFICIT_PRESENTE[1]) {
        rc2 <- riparto_circ[
          riparto_circ$CIRCOSCRIZIONE == c & riparto_circ$DEFICIT,
        ]
      } else {
        rc2 <- riparto_circ[riparto_circ$DEFICIT,]
      }
      
      if (dim(rc2)[1] < 1) stop("Non ho a chi dare il seggio eccedente")
      
      rc2 <- rc2[order(
        rc2$DECIMALI,
        rc2$CIFRA_NAZ,
        decreasing = TRUE
      ),]
      
      s2 <- rc2$SOGGETTO_RIPARTO[1]
      
      c2 <- rc2$CIRCOSCRIZIONE[1]
      
      riparto_circ$FLIPPER[
        riparto_circ$SOGGETTO_RIPARTO == s &
          riparto_circ$CIRCOSCRIZIONE == c
      ] <- -1
      
      riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s] <-
        riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s] - 1
      
      riparto_circ$FLIPPER[
        riparto_circ$SOGGETTO_RIPARTO == s2 &
          riparto_circ$CIRCOSCRIZIONE == c2
      ] <- 1
      
      riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s2] <-
        riparto_naz$SEGGI_ECCEDENTI_CONTATORE[riparto_naz$SOGGETTO_RIPARTO == s2] + 1
      
      cat(
        "Tolgo un seggio a", as.character(s), "in", as.character(c),
        "per darlo a", as.character(s2), "in", as.character(c2), "\n"
      )
    }
  }
  
  riparto_circ$SEGGI <- riparto_circ$SEGGI + riparto_circ$FLIPPER
  
  ##### Art. 83 comma 1 lettera i - riparto circoscrizionale interno alle coalizioni ####
  
  # i) procede quindi all'attribuzione nelle singole circoscrizioni
  # dei seggi spettanti alle liste di ciascuna coalizione. A tale fine,
  # determina il quoziente circoscrizionale di ciascuna coalizione di
  # liste dividendo il totale delle cifre elettorali circoscrizionali
  # delle liste ammesse alla ripartizione ai sensi della lettera g),
  # primo periodo, per il numero dei seggi assegnati alla coalizione
  # nella circoscrizione ai sensi della lettera h). Nell'effettuare la
  # divisione di cui al periodo precedente non tiene conto dell'eventuale
  # parte frazionaria del quoziente.
  
  ammesse_circ <- liste_circ[
    liste_circ$SOGLIA3M,
    c(
      "CIRCOSCRIZIONE",
      "SOGGETTO_RIPARTO",
      "LISTA",
      "CIFRA"
    )
  ]
  
  riparto_circ <- merge(
    riparto_circ,
    aggregate(
      CIFRA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
      ammesse_circ,
      sum
    ),
    by = c("CIRCOSCRIZIONE", "SOGGETTO_RIPARTO"),
    suffixes = c("","_AMMESSE_AL_RIPARTO")
  )
  
  riparto_circ$QUOZIENTE_COAL <- 
    riparto_circ$CIFRA_AMMESSE_AL_RIPARTO %/% riparto_circ$SEGGI
  
  # Divide quindi la cifra elettorale
  # circoscrizionale di ciascuna lista della coalizione per tale
  # quoziente circoscrizionale. La parte intera del quoziente cosi'
  # ottenuto rappresenta il numero dei seggi da assegnare a ciascuna
  # lista.
  
  ammesse_circ <- merge(
    ammesse_circ,
    riparto_circ[, c("CIRCOSCRIZIONE", "SOGGETTO_RIPARTO", "QUOZIENTE_COAL")]
  )
  
  ammesse_circ$PARTE_INTERA <- ammesse_circ$CIFRA %/% ammesse_circ$QUOZIENTE_COAL
  ammesse_circ$DECIMALI <- ( ammesse_circ$CIFRA / ammesse_circ$QUOZIENTE_COAL ) %% 1
  
  # I seggi che rimangono ancora da attribuire sono assegnati alle
  # liste seguendo la graduatoria decrescente delle parti decimali dei
  # quozienti cosi' ottenuti; in caso di parita', sono attribuiti alle
  # liste con la maggiore cifra elettorale circoscrizionale; a parita' di
  # quest'ultima, si procede a sorteggio. Esclude dall'attribuzione di
  # cui al periodo precedente le liste alle quali e' stato attribuito il
  # numero di seggi ad esse assegnato a seguito delle operazioni di cui
  # alla lettera g).
  
  ammesse_naz <- merge(
    ammesse_naz,
    aggregate(
      PARTE_INTERA ~ LISTA,
      ammesse_circ,
      sum
    ),
    by = "LISTA",
    suffixes = c("", "_CIRC")
  )
  
  ammesse_naz$ESCLUSE <- ammesse_naz$PARTE_INTERA_CIRC >= ammesse_naz$SEGGI
  
  ammesse_circ <- merge(
    ammesse_circ,
    ammesse_naz[,c("LISTA", "ESCLUSE", "CIFRA")],
    by = "LISTA",
    suffixes = c("", "_NAZ")
  )
  
  riparto_circ <- merge(
    riparto_circ,
    aggregate(
      PARTE_INTERA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
      ammesse_circ,
      sum
    ),
    by = c("CIRCOSCRIZIONE", "SOGGETTO_RIPARTO"),
    suffixes = c("", "_TOT")
  )
  
  riparto_circ$DA_ASSEGNARE_COAL <- 
    riparto_circ$SEGGI - riparto_circ$PARTE_INTERA_TOT
  
  ammesse_circ <- merge(
    ammesse_circ,
    riparto_circ[,c(
      "CIRCOSCRIZIONE", 
      "SOGGETTO_RIPARTO",
      "DA_ASSEGNARE_COAL"
    )]
  )
  
  ammesse_circ <- ammesse_circ[
    order(
      ammesse_circ$CIRCOSCRIZIONE,
      ammesse_circ$SOGGETTO_RIPARTO,
      ammesse_circ$ESCLUSE,
      ammesse_circ$DECIMALI,
      ammesse_circ$CIFRA,
      decreasing = c(FALSE, FALSE, FALSE, TRUE, TRUE),
      method = "radix"
    ),
  ]
  
  ammesse_circ$ORDINE[!ammesse_circ$ESCLUSE] <- ave(
    seq_along(ammesse_circ$SOGGETTO_RIPARTO[!ammesse_circ$ESCLUSE]),
    paste(
      ammesse_circ$CIRCOSCRIZIONE[!ammesse_circ$ESCLUSE],
      ammesse_circ$SOGGETTO_RIPARTO[!ammesse_circ$ESCLUSE]
    ),
    FUN = seq_along
  )
  
  ammesse_circ$SEGGIO_DA_DECIMALI <- 
    ammesse_circ$ORDINE <= ammesse_circ$DA_ASSEGNARE_COAL
  ammesse_circ$SEGGIO_DA_DECIMALI[is.na(ammesse_circ$SEGGIO_DA_DECIMALI)] <- FALSE
  
  ammesse_circ$SEGGI <- ammesse_circ$PARTE_INTERA + ammesse_circ$SEGGIO_DA_DECIMALI
  
  # Successivamente l'ufficio accerta se il numero dei
  # seggi assegnati in tutte le circoscrizioni a ciascuna lista
  # corrisponda al numero dei seggi ad essa attribuito ai sensi della
  # lettera g).
  
  ammesse_naz <- merge(
    ammesse_naz,
    aggregate(
      SEGGI ~ LISTA,
      ammesse_circ,
      sum
    ),
    by = "LISTA",
    suffixes = c("", "_CIRC")
  )
  
  ammesse_naz$SEGGI_ECCEDENTI <- ammesse_naz$SEGGI_CIRC - ammesse_naz$SEGGI
  
  # In caso negativo, procede alle seguenti operazioni,
  # iniziando dalla lista che abbia il maggior numero di seggi eccedenti
  # e, in caso di parita' di seggi eccedenti da parte di piu' liste, da
  # quella che abbia ottenuto la maggiore cifra elettorale nazionale,
  # proseguendo poi con le altre liste, in ordine decrescente di seggi
  # eccedenti: sottrae i seggi eccedenti alla lista nelle circoscrizioni
  # nelle quali essa li ha ottenuti con le parti decimali dei quozienti,
  # secondo il loro ordine crescente, e nelle quali inoltre le liste, che
  # non abbiano ottenuto il numero di seggi spettante, abbiano parti
  # decimali dei quozienti non utilizzate. Conseguentemente, assegna i
  # seggi a tali liste. Qualora nella medesima circoscrizione due o piu'
  # liste abbiano parti decimali dei quozienti non utilizzate, il seggio
  # e' attribuito alla lista con la piu' alta parte decimale del
  # quoziente non utilizzata o, in caso di parita', a quella con la
  # maggiore cifra elettorale nazionale. Nel caso in cui non sia
  # possibile attribuire il seggio eccedentario nella medesima
  # circoscrizione, in quanto non vi siano liste deficitarie con parti
  # decimali di quozienti non utilizzate, l'Ufficio prosegue, per la
  # stessa lista eccedentaria, nell'ordine dei decimali crescenti, a
  # individuare un'altra circoscrizione, fino a quando non sia possibile
  # sottrarre il seggio eccedentario e attribuirlo ad una lista
  # deficitaria nella medesima circoscrizione. Nel caso in cui non sia
  # possibile fare riferimento alla medesima circoscrizione ai fini del
  # completamento delle operazioni precedenti, fino a concorrenza dei
  # seggi ancora da cedere, alla lista eccedentaria vengono sottratti i
  # seggi nelle circoscrizioni nelle quali li ha ottenuti con le minori
  # parti decimali del quoziente di attribuzione e alle liste deficitarie
  # sono conseguentemente attribuiti seggi nelle altre circoscrizioni
  # nelle quali abbiano le maggiori parti decimali del quoziente di
  # attribuzione non utilizzate.
  # 2. L'Ufficio centrale nazionale provvede a comunicare ai singoli
  # Uffici centrali circoscrizionali il numero dei seggi assegnati a
  # ciascuna lista.
  # 3. Di tutte le operazioni dell'Ufficio centrale nazionale viene
  # redatto, in duplice esemplare, un apposito verbale: un esemplare e'
  # rimesso alla Segreteria generale della Camera dei deputati, la quale
  # ne rilascia ricevuta; un altro esemplare e' depositato presso la
  # cancelleria della Corte di cassazione.
  
  ammesse_naz <- ammesse_naz[
    order(
      ammesse_naz$SEGGI_ECCEDENTI,
      ammesse_naz$CIFRA,
      decreasing = TRUE
    ),
  ]
  
  ammesse_naz$SEGGI_ECCEDENTI_CONTATORE <- ammesse_naz$SEGGI_ECCEDENTI
  
  ammesse_circ$FLIPPER <- 0
  
  for (i in seq_along(ammesse_naz$LISTA)) {
    if (ammesse_naz$SEGGI_ECCEDENTI[i] < 1) break
    
    s <- ammesse_naz$SOGGETTO_RIPARTO[i]
    l <- ammesse_naz$LISTA[i]
    for (j in 1:ammesse_naz$SEGGI_ECCEDENTI[i]) {
      ammesse_circ$DEFICIT <- 
        ammesse_circ$SOGGETTO_RIPARTO == s &
        ammesse_circ$LISTA %in% ammesse_naz$LISTA[
          ammesse_naz$SEGGI_ECCEDENTI_CONTATORE < 0
        ] &
        !ammesse_circ$SEGGIO_DA_DECIMALI &
        ammesse_circ$FLIPPER == 0
      
      ac <- ammesse_circ[
        ammesse_circ$LISTA == l &
          ammesse_circ$SEGGIO_DA_DECIMALI &
          ammesse_circ$FLIPPER == 0,
      ]
      
      if (dim(ac)[1] < 1) stop(
        "Devo togliere un seggio a ", l, " ma non ci sono circoscrizioni dove ",
        "questo sia stato ottenuto con i resti."
      )
      
      ac$DEFICIT_PRESENTE <- ac$CIRCOSCRIZIONE %in% 
        ammesse_circ$CIRCOSCRIZIONE[ammesse_circ$DEFICIT]
      
      ac <- ac[
        order(
          ac$DEFICIT_PRESENTE,
          ac$DECIMALI,
          decreasing = c(TRUE, FALSE)
        ),
      ]
      
      c <- ac$CIRCOSCRIZIONE[1]
      
      if (ac$DEFICIT_PRESENTE[1]) {
        ac2 <- ammesse_circ[
          ammesse_circ$CIRCOSCRIZIONE == c &
            ammesse_circ$SOGGETTO_RIPARTO == s &
            ammesse_circ$DEFICIT,
        ]
      } else {
        ac2 <- ammesse_circ[
          ammesse_circ$SOGGETTO_RIPARTO == s &
            ammesse_circ$DEFICIT,
        ]
      }
      
      if (dim(ac2)[1] < 1) stop(
        "Devo togliere un seggio a ", l, " ma non ho a chi darlo."
      )
      
      ac2 <- ac2[order(
        ac2$DECIMALI,
        ac2$CIFRA_NAZ,
        decreasing = TRUE
      ),]
      
      l2 <- ac2$LISTA[1]
      c2 <- ac2$CIRCOSCRIZIONE[1]
      
      ammesse_circ$FLIPPER[
        ammesse_circ$LISTA == l &
          ammesse_circ$CIRCOSCRIZIONE == c
      ] <- -1
      
      ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l] <-
        ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l] - 1
      
      ammesse_circ$FLIPPER[
        ammesse_circ$LISTA == l2 &
          ammesse_circ$CIRCOSCRIZIONE == c2
      ] <- 1
      
      ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l2] <-
        ammesse_naz$SEGGI_ECCEDENTI_CONTATORE[ammesse_naz$LISTA == l2] - 1
      
      cat(
        "Tolgo un seggio a", as.character(l), "in", as.character(c),
        "per darlo a", as.character(l2), "in", as.character(c2), "\n"
      )
    }
  }
  
  ammesse_circ$SEGGI <- ammesse_circ$SEGGI + ammesse_circ$FLIPPER
  
  ##### Art. 83 bis - riparto plurinominali ####
  
  # Art. 83-bis.
  # ((1. L'Ufficio centrale circoscrizionale, ricevute da parte
  # dell'Ufficio elettorale centrale nazionale le comunicazioni di cui
  #   all'articolo 83, comma 2, procede all'attribuzione nei singoli
  #   collegi plurinominali dei seggi spettanti alle liste. A tale fine
  #   l'ufficio determina il quoziente elettorale di collegio dividendo la
  # somma delle cifre elettorali di collegio di tutte le liste per il
  # numero dei seggi da attribuire nel collegio stesso. Nell'effettuare
  #   tale divisione non tiene conto dell'eventuale parte frazionaria del
  # quoziente.
  
  liste_pluri <- merge(
    liste_pluri,
    liste_naz[
      ,
      c("LISTA", "SOGLIA3M")
    ]
  )
  
  ammesse_pluri <- liste_pluri[
    liste_pluri$SOGLIA3M,
    c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA",
      "CIFRA",
      "CIFRA_PERCENTUALE"
    )
  ]
  
  totali_pluri <- merge(
    totali_pluri,
    aggregate(
      CIFRA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
      ammesse_pluri,
      sum
    )
  )
  
  totali_pluri$QUOZIENTE <-
    totali_pluri$CIFRA %/% totali_pluri$SEGGI
  
  # Divide quindi la cifra elettorale di collegio di ciascuna
  # lista per tale quoziente di collegio. La parte intera del quoziente
  # cosi' ottenuto rappresenta il numero dei seggi da assegnare a
  #   ciascuna lista.
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    totali_pluri[, c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "QUOZIENTE")]
  )
  
  ammesse_pluri$PARTE_INTERA <- ammesse_pluri$CIFRA %/% ammesse_pluri$QUOZIENTE
  ammesse_pluri$DECIMALI <- ( ammesse_pluri$CIFRA / ammesse_pluri$QUOZIENTE ) %% 1
  
  
  # I seggi che rimangono ancora da attribuire sono
  #   assegnati alle liste seguendo la graduatoria decrescente delle parti
  #   decimali dei quozienti cosi' ottenuti; in caso di parita', sono
  #   attribuiti alle liste con la maggiore cifra elettorale
  #   circoscrizionale; a parita' di quest'ultima, si procede a sorteggio.
  #   L'Ufficio esclude dall'attribuzione di cui al periodo precedente le
  #   liste alle quali e' stato attribuito il numero di seggi ad esse
  # assegnato nella circoscrizione secondo la comunicazione di cui
  # all'articolo 83, comma 2.
  
  ammesse_circ <- merge(
    ammesse_circ,
    aggregate(
      PARTE_INTERA ~ CIRCOSCRIZIONE + LISTA,
      ammesse_pluri,
      sum
    ),
    by = c("CIRCOSCRIZIONE", "LISTA"),
    suffixes = c("", "_PLURI")
  )
  
  ammesse_circ$ESCLUSE_PLURI <- 
    ammesse_circ$PARTE_INTERA_PLURI >= ammesse_circ$SEGGI
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    ammesse_circ[,c("CIRCOSCRIZIONE", "LISTA", "ESCLUSE_PLURI", "CIFRA")],
    by = c("CIRCOSCRIZIONE", "LISTA"),
    suffixes = c("", "_CIRC")
  )
  
  totali_pluri <- merge(
    totali_pluri,
    aggregate(
      PARTE_INTERA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
      ammesse_pluri,
      sum
    )
  )
  
  totali_pluri$DA_ASSEGNARE <- totali_pluri$SEGGI - totali_pluri$PARTE_INTERA
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    totali_pluri[,c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "DA_ASSEGNARE")]
  )
  
  ammesse_pluri <- ammesse_pluri[
    order(
      ammesse_pluri$CIRCOSCRIZIONE,
      ammesse_pluri$COLLEGIOPLURINOMINALE,
      ammesse_pluri$ESCLUSE_PLURI,
      ammesse_pluri$DECIMALI,
      ammesse_pluri$CIFRA_CIRC,
      decreasing = c(FALSE, FALSE, FALSE, TRUE, TRUE),
      method = "radix"
    ),
  ]
  
  ammesse_pluri$ORDINE[!ammesse_pluri$ESCLUSE_PLURI] <- ave(
    seq_along(ammesse_pluri$COLLEGIOPLURINOMINALE[!ammesse_pluri$ESCLUSE_PLURI]),
    paste(
      ammesse_pluri$CIRCOSCRIZIONE[!ammesse_pluri$ESCLUSE_PLURI],
      ammesse_pluri$COLLEGIOPLURINOMINALE[!ammesse_pluri$ESCLUSE_PLURI]
    ),
    FUN = seq_along
  )
  
  ammesse_pluri$SEGGIO_DA_DECIMALI <- 
    ammesse_pluri$ORDINE <= ammesse_pluri$DA_ASSEGNARE
  ammesse_pluri$SEGGIO_DA_DECIMALI[is.na(ammesse_pluri$SEGGIO_DA_DECIMALI)] <- FALSE
  
  ammesse_pluri$SEGGI <- 
    ammesse_pluri$PARTE_INTERA + ammesse_pluri$SEGGIO_DA_DECIMALI
  
  # Successivamente l'ufficio accerta se il
  # numero dei seggi assegnati in tutti i collegi a ciascuna lista
  # corrisponda al numero di seggi ad essa attribuito nella
  # circoscrizione dall'Ufficio elettorale centrale nazionale.
  
  ammesse_circ <- merge(
    ammesse_circ,
    aggregate(
      SEGGI ~ CIRCOSCRIZIONE + LISTA,
      ammesse_pluri,
      sum
    ),
    by = c("CIRCOSCRIZIONE", "LISTA"),
    suffixes = c("", "_PLURI")
  )
  
  ammesse_circ$SEGGI_ECCEDENTI <- 
    ammesse_circ$SEGGI_PLURI - ammesse_circ$SEGGI
  
  # In caso
  #   negativo, determina la lista che ha il maggior numero di seggi
  #   eccedentari e, a parita' di essi, la lista che tra queste ha ottenuto
  # il seggio eccedentario con la minore parte decimale del quoziente;
  # sottrae quindi il seggio a tale lista nel collegio in cui e' stato
  #   ottenuto con la minore parte decimale dei quozienti di attribuzione e
  #   lo assegna alla lista deficitaria che ha il maggior numero di seggi
  #   deficitari e, a parita' di essi, alla lista che tra queste ha la
  # maggiore parte decimale del quoziente che non ha dato luogo
  # all'assegnazione di seggio; il seggio e' assegnato alla lista
  # deficitaria nel collegio plurinominale in cui essa ha la maggiore
  # parte decimale del quoziente di attribuzione non utilizzata; ripete
  # quindi, in successione, tali operazioni sino all'assegnazione di
  #   tutti i seggi eccedentari alle liste deficitarie)).
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    ammesse_circ[
      ,
      c("CIRCOSCRIZIONE", "LISTA", "SEGGI_ECCEDENTI")
    ]
  )
  
  ammesse_pluri$CEDE <- 
    ammesse_pluri$SEGGI_ECCEDENTI > 0 & ammesse_pluri$SEGGIO_DA_DECIMALI
  
  ammesse_pluri$RICEVE <-
    ammesse_pluri$SEGGI_ECCEDENTI < 0 & !ammesse_pluri$SEGGIO_DA_DECIMALI
  
  ammesse_pluri <- ammesse_pluri[order(
    ammesse_pluri$CIRCOSCRIZIONE,
    ammesse_pluri$SEGGIO_DA_DECIMALI,
    ammesse_pluri$SEGGI_ECCEDENTI,
    ammesse_pluri$DECIMALI,
    decreasing = c(FALSE, TRUE, TRUE, FALSE),
    method = "radix"
  ),]
  
  ammesse_pluri$ORDINE_CEDE[ammesse_pluri$CEDE] <- ave(
    seq_along(ammesse_pluri$LISTA[ammesse_pluri$CEDE]),
    paste(
      ammesse_pluri$CIRCOSCRIZIONE,
      ammesse_pluri$LISTA
    )[ammesse_pluri$CEDE],
    FUN = seq_along
  )
  
  ammesse_pluri$CEDUTO <- 
    ammesse_pluri$ORDINE_CEDE <= ammesse_pluri$SEGGI_ECCEDENTI
  
  ammesse_pluri$CEDUTO[is.na(ammesse_pluri$CEDUTO)] <- FALSE
  
  ammesse_pluri <- ammesse_pluri[order(
    ammesse_pluri$CIRCOSCRIZIONE,
    ammesse_pluri$SEGGIO_DA_DECIMALI,
    ammesse_pluri$SEGGI_ECCEDENTI,
    ammesse_pluri$DECIMALI,
    decreasing = c(FALSE, FALSE, FALSE, TRUE),
    method = "radix"
  ),]
  
  ammesse_pluri$ORDINE_RICEVE[ammesse_pluri$RICEVE] <- ave(
    seq_along(ammesse_pluri$LISTA[ammesse_pluri$RICEVE]),
    paste(
      ammesse_pluri$CIRCOSCRIZIONE,
      ammesse_pluri$LISTA
    )[ammesse_pluri$RICEVE],
    FUN = seq_along
  )
  
  ammesse_pluri$RICEVUTO <- 
    ammesse_pluri$ORDINE_RICEVE <= - ammesse_pluri$SEGGI_ECCEDENTI
  
  ammesse_pluri$RICEVUTO[is.na(ammesse_pluri$RICEVUTO)] <- FALSE
  
  ammesse_pluri$SEGGI <- 
    ammesse_pluri$SEGGI - ammesse_pluri$CEDUTO + ammesse_pluri$RICEVUTO
  
  # 2. Di tutte le operazioni dell'Ufficio centrale circoscrizionale
  # viene redatto, in duplice esemplare, apposito verbale: un esemplare
  # e' rimesso alla Segreteria generale della Camera dei deputati, la
  # quale ne rilascia ricevuta; un altro esemplare e' depositato presso
  # la cancelleria della Corte di cassazione.
  
  ##### Art. 84  ####
  
  # Art. 84.
  # ((1. Al termine delle operazioni di cui agli articoli precedenti,
  #   l'Ufficio centrale circoscrizionale proclama eletti in ciascun
  # collegio plurinominale, nei limiti dei seggi ai quali ciascuna lista
  # ha diritto, i candidati compresi nella lista del collegio, secondo
  # l'ordine di presentazione.
  
  candidati_uni$RIPESCATO <- FALSE
  
  candidati_pluri$DISPONIBILE <- 
    !(candidati_pluri$CANDIDATO %in% 
        candidati_uni$CANDIDATO[candidati_uni$ELETTO])
  
  # ammesse_pluri$DECIMALI_USATI <-
  #   ammesse_pluri$SEGGIO_DA_DECIMALI + 
  #   ammesse_pluri$RICEVUTO - 
  #   ammesse_pluri$CEDUTO > 0
  
  ammesse_pluri$DECIMALI_USATI <-
    ammesse_pluri$SEGGIO_DA_DECIMALI
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    aggregate(
      DISPONIBILE ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
      candidati_pluri,
      sum
    ),
    all.x = TRUE
  )
  
  names(ammesse_pluri)[names(ammesse_pluri) == "DISPONIBILE"] <- "CANDIDATI"
  
  ammesse_pluri$CANDIDATI[is.na(ammesse_pluri$CANDIDATI)] <- 0
  
  ammesse_pluri$ELETTI <- 
    pmin(ammesse_pluri$SEGGI, ammesse_pluri$CANDIDATI)
  
  # Schema subentri
  #   livello uni   coal
  # 2
  #   circ    FALSE FALSE
  # 3
  #   pluri   TRUE  FALSE
  #   circ    TRUE  FALSE
  # 4
  #   naz     FALSE FALSE
  # 5
  #   pluri   FALSE TRUE
  #   circ    FALSE TRUE
  # 6
  #   naz     TRUE  FALSE
  # 7
  #   naz     FALSE TRUE
  
    #   2. Qualora una lista abbia esaurito il numero dei candidati
  #   presentati in un collegio plurinominale e non sia quindi possibile
  #   attribuire tutti i seggi a essa spettanti in quel collegio, l'Ufficio
  # centrale circoscrizionale assegna i seggi alla lista negli altri
  # collegi plurinominali della stessa circoscrizione in cui la lista
  # medesima abbia la maggiore parte decimale del quoziente non
  # utilizzata, procedendo secondo l'ordine decrescente. Qualora al
  #   termine di detta operazione residuino ancora seggi da assegnare alla
  #   lista, questi le sono attribuiti negli altri collegi plurinominali
  #   della stessa circoscrizione in cui la lista medesima abbia la
  #   maggiore parte decimale del quoziente gia' utilizzata, procedendo
  # secondo l'ordine decrescente.
  
  subentro()
  
  #   3. Qualora al termine delle operazioni di cui al comma 2 residuino
  #   ancora seggi da assegnare ad una lista, questi sono attribuiti,
  #   nell'ambito del collegio plurinominale originario, ai candidati della
  # lista nei collegi uninominali non proclamati eletti secondo la
  # graduatoria di cui all'articolo 77, comma 1, lettera h).
  
  subentro(livello = "pluri", uni = TRUE)
  
  # Qualora
  #   residuino ancora seggi da assegnare alla lista, questi sono
  #   attribuiti ai candidati della lista nei collegi uninominali non
  #   proclamati eletti nell'ambito della circoscrizione, secondo la
  # graduatoria di cui all'articolo 77, comma 1, lettera h).
  
  subentro(livello = "circ", uni = TRUE)
  
  # 4. Qualora al termine delle operazioni di cui al comma 3 residuino
  # ancora seggi da assegnare alla lista, l'Ufficio centrale nazionale,
  # previa apposita comunicazione dell'Ufficio centrale circoscrizionale,
  # individua la circoscrizione in cui la lista abbia la maggiore parte
  # decimale del quoziente non utilizzata e procede a sua volta ad
  # apposita comunicazione all'Ufficio centrale circoscrizionale
  # competente. L'Ufficio centrale circoscrizionale provvede
  # all'assegnazione dei seggi ai sensi del comma 2.
  # Qualora al termine
  # delle operazioni di cui ai precedenti periodi residuino ancora seggi
  # da assegnare alla lista, questi le sono attribuiti nelle altre
  # circoscrizioni in cui la stessa lista abbia la maggiore parte
  # decimale del quoziente gia' utilizzata, procedendo secondo l'ordine
  # decrescente.
  
  subentro(livello = "naz")
  
  # 5. Qualora al termine delle operazioni di cui al comma 4 residuino
  # ancora seggi da assegnare ad una lista in un collegio plurinominale,
  # questi sono attribuiti, nell'ambito del collegio plurinominale
  # originario, alla lista facente parte della medesima coalizione della
  # lista deficitaria che abbia la maggiore parte decimale del quoziente
  # non utilizzata, procedendo secondo l'ordine decrescente; esaurite le
  # liste con la parte decimale del quoziente non utilizzata, si procede
  # con le liste facenti parte della medesima coalizione, sulla base
  # delle parti decimali del quoziente gia' utilizzate, secondo l'ordine
  # decrescente. 
  
  subentro(livello = "pluri", coal = TRUE)
  
  # Qualora al termine delle operazioni di cui al primo
  # periodo residuino ancora seggi da assegnare alla lista, questi sono
  # attribuiti alle liste facenti parte della medesima coalizione negli
  # altri collegi plurinominali della circoscrizione, partendo da quello
  # in cui la coalizione abbia la maggiore parte decimale del quoziente
  # non utilizzata e procedendo secondo quanto previsto dal primo
  # periodo; si procede successivamente nei collegi plurinominali in cui
  # la coalizione abbia la maggiore parte decimale del quoziente gia'
  # utilizzata, secondo l'ordine decrescente.
  
  subentro(livello = "circ", coal = TRUE)
  
  # 6. Qualora al termine delle operazioni di cui al comma 5 residuino
  # ancora seggi da assegnare ad una lista, questi sono attribuiti ai
  # candidati della lista nei collegi uninominali non proclamati eletti
  # nelle altre circoscrizioni, secondo la graduatoria di cui
  # all'articolo 77, comma 1, lettera h). A tale fine si procede con le
  # modalita' previste dal comma 4.
  
  subentro(livello = "naz", uni = TRUE)
  
  # 7. Qualora al termine delle operazioni di cui al comma 6 residuino
  # ancora seggi da assegnare ad una lista, questi sono attribuiti alle
  # liste facenti parte della medesima coalizione della lista deficitaria
  # nelle altre circoscrizioni. A tale fine si procede con le modalita'
  # previste dai commi 4 e 5.
  
  subentro(livello = "naz", coal = TRUE)
  
  # Art. 85.
  # ((1. Il deputato eletto in piu' collegi plurinominali e' proclamato
  #   nel collegio nel quale la lista cui appartiene ha ottenuto la minore
  #   cifra elettorale percentuale di collegio plurinominale, determinata
  #   ai sensi dell'articolo 77, comma 1, lettera e) )).
  # ((1-bis. Il deputato eletto in un collegio uninominale e in uno o
  # piu' collegi plurinominali si intende eletto nel collegio
  #   uninominale)).
  
  for (i in 1:100) {
    if (sum(ammesse_pluri$SEGGI != ammesse_pluri$ELETTI) > 0) warning(
      "In alcuni collegi il numero di eletti non corrisponde al numero di seggi
  al termine dei subentri"
    )
    
    candidati_pluri$ELETTI <- NULL
    
    candidati_pluri <- merge(
      candidati_pluri,
      ammesse_pluri[,
                    c(
                      "CIRCOSCRIZIONE",
                      "COLLEGIOPLURINOMINALE",
                      "LISTA",
                      "ELETTI",
                      "CIFRA_PERCENTUALE"
                    )
      ]
    )
    
    candidati_pluri <- candidati_pluri[order(
      !candidati_pluri$DISPONIBILE,
      candidati_pluri$CIRCOSCRIZIONE,
      candidati_pluri$COLLEGIOPLURINOMINALE,
      candidati_pluri$LISTA,
      candidati_pluri$NUMERO
    ),]
    
    candidati_pluri$ORDINE <- NA
    candidati_pluri$ORDINE[candidati_pluri$DISPONIBILE] <- ave(
      seq_along(candidati_pluri$CANDIDATO[candidati_pluri$DISPONIBILE]),
      paste(
        candidati_pluri$CIRCOSCRIZIONE,
        candidati_pluri$COLLEGIOPLURINOMINALE,
        candidati_pluri$LISTA
      )[candidati_pluri$DISPONIBILE],
      FUN = seq_along
    )
    
    candidati_pluri$ELETTO <- !is.na(candidati_pluri$ORDINE) &
      candidati_pluri$ORDINE <= candidati_pluri$ELETTI
    

    candidati_pluri <- candidati_pluri[order(
      candidati_pluri$DISPONIBILE,
      candidati_pluri$CANDIDATO,
      candidati_pluri$ELETTO,
      candidati_pluri$CIFRA_PERCENTUALE,
      decreasing = c(TRUE, FALSE, TRUE, FALSE),
      method = "radix"
    ),]
    
    if (sum(candidati_pluri$ELETTO[candidati_pluri$DISPONIBILE] & 
            duplicated(candidati_pluri$CANDIDATO[candidati_pluri$DISPONIBILE])) == 0)
      break
    
    indisponibili <- which(
      candidati_pluri$ELETTO &
        candidati_pluri$DISPONIBILE &
        duplicated(candidati_pluri$CANDIDATO)
    )
    
    candidati_pluri$ELETTO[indisponibili] <- FALSE
    candidati_pluri$DISPONIBILE[indisponibili] <- FALSE
    
    ammesse_pluri$CANDIDATI <- NULL
    
    ammesse_pluri <- merge(
      ammesse_pluri,
      aggregate(
        CANDIDATO ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
        candidati_pluri[candidati_pluri$DISPONIBILE, ],
        length
      ),
      all.x = TRUE
    )
    
    names(ammesse_pluri)[names(ammesse_pluri) == "CANDIDATO"] <- "CANDIDATI"
    ammesse_pluri$CANDIDATI[is.na(ammesse_pluri$CANDIDATI)] <- 0
    
    ammesse_pluri$ELETTI <- 
      pmin(ammesse_pluri$SEGGI, ammesse_pluri$CANDIDATI)
    
    # 1. Il seggio che rimanga vacante per qualsiasi causa, anche
    # sopravvenuta, ((in un collegio plurinominale)) e' attribuito,
    # nell'ambito del medesimo collegio plurinominale, al candidato ((primo
    # dei non eletti, secondo l'ordine di presentazione)). (42)
    # 2. Nel caso in cui una lista abbia gia' esaurito i propri candidati
    # si procede con le modalita' di cui all'articolo 84, commi 2, 3 ((, 4 e 5)).
    
    subentro()
    subentro(livello = "pluri", uni = TRUE)
    subentro(livello = "circ", uni = TRUE)
    subentro(livello = "naz")
    subentro(livello = "pluri", coal = TRUE)
    subentro(livello = "circ", coal = TRUE)
  }
  
  #### Preparazione del risultato ####
  
  # candidati_pluri <- merge(
  #   candidati_pluri_backup,
  #   candidati_pluri[, c(
  #     "CIRCOSCRIZIONE",
  #     "COLLEGIOPLURINOMINALE",
  #     "LISTA",
  #     "NUMERO",
  #     "CANDIDATO",
  #     "ELETTO"
  #   )],
  #   all.x = TRUE
  # )
  
  candidati_pluri$ELETTO[is.na(candidati_pluri$ELETTO)] <- FALSE
  
  candidati_pluri$ELETTO_QUI_O_ALTROVE <-
    candidati_pluri$CANDIDATO %in% candidati_pluri$CANDIDATO[
      candidati_pluri$ELETTO
    ] | candidati_pluri$CANDIDATO %in% candidati_uni$CANDIDATO[
      candidati_uni$ELETTO
    ]
  
  liste_pluri <- merge(
    liste_pluri[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA"
    )],
    ammesse_pluri[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA",
      "ELETTI"
    )],
    all.x = TRUE
  )
  
  liste_pluri$ELETTI[is.na(liste_pluri$ELETTI)] <- 0
  
  liste_pluri <- merge(
    liste_pluri,
    aggregate(
      NUMERO ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
      candidati_pluri[candidati_pluri$ELETTO,],
      max
    ),
    all.x = TRUE
  )
  names(liste_pluri)[names(liste_pluri) == "NUMERO"] <- "NUMERO_MAX"
  
  liste_pluri$NUMERO_MAX[is.na(liste_pluri$NUMERO_MAX)] <- 0
  
  #### Risultato ####
  
  return(list(
    liste_pluri = liste_pluri[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA",
      "ELETTI",
      "NUMERO_MAX"
    )],
    candidati_uni = candidati_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "ELETTO"
    )],
    candidati_pluri = candidati_pluri[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA",
      "NUMERO",
      "CANDIDATO",
      "ELETTO",
      "ELETTO_QUI_O_ALTROVE"
    )]
  ))
  
}