source("condivisi.R")

S_scrutinio <- function(
  
  liste_uni,
  # CIRCOSCRIZIONE (factor)
  # COLLEGIOPLURINOMINALE (factor)
  # COLLEGIOUNINOMINALE (factor)
  # CANDIDATO (factor)
  # CAN_MINORANZA (logical)
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
  
  totali_pluri
  # CIRCOSCRIZIONE (factor)
  # COLLEGIOPLURINOMINALE (factor)
  # SEGGI (numeric)
  
) {
  
#### Art. 16 ####
  
# ((1. L'Ufficio elettorale regionale, compiute le operazioni
# previste dall'articolo 76 del testo unico delle leggi recanti norme
#   per la elezione della Camera dei deputati, di cui al decreto del
#   Presidente della Repubblica 30 marzo 1957, n. 361, facendosi
#   assistere, ove lo ritenga opportuno, da uno o piu' esperti scelti dal
# presidente:
#   a) determina la cifra elettorale individuale di ciascun candidato
# nei collegi uninominali; tale cifra e' data dalla somma dei voti
# validi conseguiti dal candidato nelle singole sezioni elettorali del
# collegio uninominale in conformita' ai risultati accertati;

# Già fatto in candidati_uni
  
#   b) proclama eletto in ciascun collegio uninominale il candidato
# che ha ottenuto il maggior numero di voti validi; in caso di parita',
# e' eletto il candidato piu' giovane di eta';
  
  candidati_uni <- candidati_uni[order(
    candidati_uni$CIRCOSCRIZIONE, 
    candidati_uni$COLLEGIOPLURINOMINALE, 
    candidati_uni$COLLEGIOUNINOMINALE, 
    candidati_uni$VOTI_CANDIDATO,
    candidati_uni$DATA_NASCITA,
    decreasing = c("FALSE", "FALSE", "FALSE", "TRUE", "TRUE")
  ), ]
  
  candidati_uni$ELETTO <- !duplicated(candidati_uni$COLLEGIOUNINOMINALE)

#   c) determina la cifra elettorale di collegio uninominale di
# ciascuna lista. Tale cifra e' data dalla somma dei voti validi
# conseguiti dalla lista stessa nelle singole sezioni elettorali del
# collegio uninominale e dei voti espressi a favore dei soli candidati
# nei collegi uninominali collegati a piu' liste in coalizione di cui
# all'articolo 14, comma 2, secondo periodo, attribuiti alla lista a
# seguito delle seguenti operazioni: l'ufficio divide il totale dei
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
# coalizione, l'ufficio esclude dal computo i voti espressi in favore
# della lista rappresentativa di minoranze linguistiche riconosciute
# nei collegi uninominali dove questa abbia presentato proprie
# candidature ai sensi dell'articolo 18-bis, comma 1-bis, del testo
# unico di cui al decreto del Presidente della Repubblica 30 marzo
# 1957, n. 361;
  
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
    decreasing = c("FALSE", "FALSE", "FALSE", "FALSE", "FALSE", "TRUE")
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
  
#   d) determina la cifra elettorale di collegio plurinominale di
# ciascuna lista. Tale cifra e' data dalla somma delle cifre elettorali
# di collegio uninominale di ciascuna lista;
  
  liste_pluri <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
    liste_uni,
    sum
  )
  
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
  
# f) determina la cifra elettorale regionale di ciascuna lista.
# Tale cifra e' data dalla somma delle cifre elettorali di collegio
# plurinominale della lista stessa;
  
  liste_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + LISTA,
    liste_pluri, 
    sum
  )
  
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
  
# h) determina, per ciascuna lista, la graduatoria dei candidati
# nei collegi uninominali della regione non proclamati eletti,
# disponendoli nell'ordine delle rispettive cifre elettorali
# individuali percentuali. A parita' di cifre individuali percentuali,
# prevale il piu' giovane di eta'. In caso di collegamento dei
# candidati con piu' liste, i candidati entrano a far parte della
# graduatoria relativa a ciascuna delle liste con cui e' stato
# dichiarato il collegamento;
  
  # Operazioni svolte più avanti
  
# i) determina il totale dei voti validi della regione. Tale totale
# e' dato dalla somma delle cifre elettorali regionali di tutte le
# liste;
  
  totali_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE,
    liste_circ,
    sum
  )
  
# l) comunica all'Ufficio elettorale centrale nazionale di cui
# all'articolo 12 del testo unico di cui al decreto del Presidente
# della Repubblica 30 marzo 1957, n. 361, a mezzo di estratto del
# verbale, la cifra elettorale regionale di ciascuna lista nonche' il
# totale dei voti validi della regione.))
  
####   Art. 16-bis. ####
  
#   1. L'Ufficio elettorale centrale nazionale, ricevuti gli estratti
# dei verbali da tutti gli Uffici elettorali regionali, facendosi
# assistere, ove lo ritenga opportuno, da uno o piu' esperti scelti dal
#   presidente:
#     a) determina la cifra elettorale nazionale di ciascuna lista.
# Tale cifra e' data dalla somma delle cifre elettorali regionali
# conseguite nelle singole regioni dalle liste aventi il medesimo
# contrassegno;
  
  liste_naz <- merge(
    liste_naz,
    aggregate(
      CIFRA ~ LISTA,
      liste_circ,
      sum
    )
  )
  
# b) determina il totale nazionale dei voti validi. Esso e' dato
# dalla somma delle cifre elettorali regionali di tutte le liste;
  
  totale_naz <- sum(liste_naz$CIFRA)
  
# c) determina la cifra elettorale nazionale di ciascuna coalizione
# di liste. Tale cifra e' data dalla somma delle cifre elettorali
# nazionali delle liste collegate tra loro in coalizione. Non
# concorrono alla determinazione della cifra elettorale nazionale di
# coalizione i voti espressi a favore delle liste collegate che abbiano
# conseguito sul piano nazionale un numero di voti validi inferiore
# all'1 per cento del totale, tranne il caso in cui tali liste abbiano
# conseguito almeno in una regione un numero di voti validi pari almeno
# al 20 per cento dei voti validi espressi nella regione medesima
# ovvero, per le liste collegate rappresentative di minoranze
# linguistiche riconosciute, presentate esclusivamente in una regione
# ad autonomia speciale il cui statuto o le relative norme di
# attuazione prevedano una particolare tutela di tali minoranze
# linguistiche, i cui candidati siano stati proclamati eletti in almeno
# ((un quarto dei collegi uninominali della circoscrizione regionale ai
#   sensi dell'articolo 16, con arrotondamento all'unita' superiore));
  
  liste_naz$CIFRA_PERCENTUALE <- liste_naz$CIFRA / totale_naz * 100
  
  liste_circ <- merge(
    liste_circ,
    totali_circ,
    by = "CIRCOSCRIZIONE",
    suffixes = c("","_TOT")
  )
  
  liste_circ$CIFRA_PERCENTUALE <- liste_circ$CIFRA / liste_circ$CIFRA_TOT * 100
  
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
  
  liste_circ$SOGLIA20 <- liste_circ$CIFRA_PERCENTUALE >= 20
  liste_circ$SOGLIA_MINORANZA <- 
    liste_circ$ELETTI_MINORANZA >= ceiling(liste_circ$COLLEGI_UNI / 4)
  
  liste_naz <- merge(
    liste_naz,
    aggregate(
      SOGLIA20 ~ LISTA,
      liste_circ,
      function(x) Reduce("|", x)
    )
  )
  
  liste_naz <- merge(
    liste_naz,
    aggregate(
      SOGLIA_MINORANZA ~ LISTA,
      liste_circ,
      function(x) Reduce("|", x)
    )
  )
  
  liste_naz$SOGLIA1M <- 
    liste_naz$CIFRA_PERCENTUALE >= 1 |
    liste_naz$SOGLIA20 |
    liste_naz$SOGLIA_MINORANZA
  
  coal_naz <- aggregate(
    CIFRA ~ COALIZIONE,
    liste_naz,
    sum,
    subset = SOGLIA1M
  )
  
  # d) determina la cifra elettorale regionale di ciascuna coalizione
# di liste. Tale cifra e' data dalla somma delle cifre elettorali
#   regionali delle liste collegate tra loro in coalizione, individuate
#   ai sensi dell'ultimo periodo della lettera c);
  
  liste_circ <- merge(
    liste_circ,
    liste_naz[, c("LISTA", "SOGLIA1M", "COALIZIONE")]
  )
  
  coal_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + COALIZIONE,
    liste_circ,
    sum,
    subset = SOGLIA1M
  )
  
# e) individua quindi:
# 1) le coalizioni di liste che abbiano conseguito sul piano
# nazionale almeno il 10 per cento dei voti validi espressi e che
# comprendano almeno una lista collegata che abbia conseguito sul piano
# nazionale almeno il 3 per cento dei voti validi espressi ovvero una
# lista collegata che abbia conseguito almeno il 20 per cento dei voti
# validi espressi almeno in una regione ovvero una lista collegata
# rappresentativa di minoranze linguistiche riconosciute, presentata
# esclusivamente in una regione ad autonomia speciale, il cui statuto o
# le relative norme di attuazione prevedano una particolare tutela di
# tali minoranze linguistiche, i cui candidati siano stati proclamati
# eletti in almeno ((un quarto dei collegi uninominali della
# circoscrizione regionale ai sensi dell'articolo 16, con
#   arrotondamento all'unita' superiore));
  
  coal_naz$CIFRA_PERCENTUALE <- 
    coal_naz$CIFRA / totale_naz * 100
  
  liste_naz$SOGLIA3 <- liste_naz$CIFRA_PERCENTUALE >= 3
  
  liste_naz$SOGLIA3M <- 
    liste_naz$SOGLIA3 |
    liste_naz$SOGLIA20 |
    liste_naz$SOGLIA_MINORANZA
  
  coal_naz <- merge(
    coal_naz,
    aggregate(
      SOGLIA3M ~ COALIZIONE,
      data = liste_naz,
      function(x) Reduce("|", x)
    )
  )
  
  coal_naz$SOGLIA_COALIZIONE <- 
    coal_naz$CIFRA_PERCENTUALE >= 10 & 
    coal_naz$SOGLIA3M
  
  
# 2) le singole liste non collegate, o collegate in coalizioni
# che non abbiano raggiunto la percentuale di cui al numero 1), che
# abbiano conseguito sul piano nazionale almeno il 3 per cento dei voti
# validi espressi, e le singole liste non collegate, o collegate in
# coalizioni che non abbiano raggiunto la percentuale di cui al numero
# 1), che abbiano conseguito almeno il 20 per cento dei voti validi
# espressi almeno in una regione, nonche' le liste non collegate, o
# collegate in coalizioni che non abbiano raggiunto la percentuale di
# cui al numero 1), rappresentative di minoranze linguistiche
# riconosciute, presentate esclusivamente in una regione ad autonomia
# speciale il cui statuto o le relative norme di attuazione prevedano
# una particolare tutela di tali minoranze linguistiche, i cui
# candidati siano stati proclamati eletti in almeno ((un quarto dei
# collegi uninominali della circoscrizione regionale ai sensi
# dell'articolo 16, con arrotondamento all'unita' superiore));
  
  liste_naz <- merge(
    liste_naz,
    coal_naz[, c("COALIZIONE", "SOGLIA_COALIZIONE")],
    all.x = TRUE
  )
  
  liste_naz$SOGLIA_SOLA <- 
    (is.na(liste_naz$COALIZIONE) | !liste_naz$SOGLIA_COALIZIONE) &
    liste_naz$SOGLIA3M
  
# f) comunica agli Uffici elettorali regionali, a mezzo di estratto
# del verbale, l'elenco delle liste e delle coalizioni di liste
# individuate ai sensi della lettera e), numeri 1) e 2).
  
#### Art. 17. #######
# 1. L'Ufficio elettorale regionale procede all'assegnazione dei
#   seggi spettanti nei collegi plurinominali della regione alle liste
#   singole e alle coalizioni di liste individuate dall'Ufficio
# elettorale centrale nazionale ai sensi dell'articolo 16-bis, comma 1,
#   lettera e), numeri 1) e 2), e incluse nell'elenco di cui all'articolo
# 16-bis, comma 1, lettera f). A tale fine l'Ufficio procede alle
# seguenti operazioni:
# a) divide il totale delle cifre elettorali regionali delle
# coalizioni di liste di cui all'articolo 16-bis, comma 1, lettera e),
# numero 1), e delle singole liste che abbiano conseguito sul piano
# nazionale almeno il 3 per cento dei voti validi espressi o che
# abbiano conseguito almeno il 20 per cento dei voti validi espressi
# nella regione e delle singole liste rappresentative di minoranze
# linguistiche riconosciute, presentate esclusivamente in una regione
# ad autonomia speciale il cui statuto o le relative norme di
# attuazione prevedano una particolare tutela di tali minoranze
# linguistiche, i cui candidati siano stati proclamati eletti in almeno
# ((un quarto dei collegi uninominali della circoscrizione regionale ai
#   sensi dell'articolo 16, con arrotondamento all'unita' superiore)),
# per il numero di seggi da attribuire nei collegi plurinominali della
# regione, ottenendo cosi' il quoziente elettorale regionale.
#  Nell'effettuare tale divisione non tiene conto dell'eventuale parte
#   frazionaria del quoziente.
  
  liste_circ <- merge(
    liste_circ,
    coal_naz[, c("COALIZIONE", "SOGLIA_COALIZIONE")],
    all.x = TRUE
  )
  
  liste_circ <- merge(
    liste_circ,
    liste_naz[, c("LISTA", "SOGLIA3")]
  )
  
  liste_circ$SOGLIA_SOLA <- 
    (is.na(liste_circ$COALIZIONE) | !liste_circ$SOGLIA_COALIZIONE) &
    (liste_circ$SOGLIA3 | liste_circ$SOGLIA20 | liste_circ$SOGLIA_MINORANZA)
  
  liste_circ$SOGGETTO_RIPARTO <- NA
  
  liste_circ$SOGGETTO_RIPARTO[which(liste_circ$SOGLIA_COALIZIONE)] <-
    as.character(liste_circ$COALIZIONE[which(liste_circ$SOGLIA_COALIZIONE)])
  
  liste_circ$SOGGETTO_RIPARTO[which(liste_circ$SOGLIA_SOLA)] <-
    as.character(liste_circ$LISTA[which(liste_circ$SOGLIA_SOLA)])
  
  riparto_circ <- aggregate(
    CIFRA ~ CIRCOSCRIZIONE + SOGGETTO_RIPARTO,
    liste_circ,
    sum
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
  
  totali_circ <- merge(
    totali_circ,
    aggregate(
      SEGGI ~ CIRCOSCRIZIONE,
      totali_pluri,
      sum
    )
  )
  
  totali_circ$QUOZIENTE <- 
    totali_circ$CIFRA_AMMESSE_AL_RIPARTO %/% totali_circ$SEGGI
  

#  Divide poi la cifra elettorale regionale
#   di ciascuna coalizione di liste o singola lista per tale quoziente.
#   La parte intera del quoziente cosi' ottenuto rappresenta il numero
# dei seggi da assegnare a ciascuna coalizione di liste o singola
# lista.

  riparto_circ <- merge(
    riparto_circ,
    totali_circ[,c("CIRCOSCRIZIONE", "QUOZIENTE")]
  )
  
  riparto_circ$PARTE_INTERA <- riparto_circ$CIFRA %/% riparto_circ$QUOZIENTE
  riparto_circ$RESTO <- riparto_circ$CIFRA %% riparto_circ$QUOZIENTE

# I seggi che rimangono ancora da attribuire sono
# rispettivamente assegnati alle coalizioni di liste o singole liste
# per le quali queste ultime divisioni hanno dato i maggiori resti e,
# in caso di parita' di resti, a quelle che hanno conseguito la
#   maggiore cifra elettorale regionale; a parita' di quest'ultima si
#   procede a sorteggio;
  
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
      riparto_circ$RESTO,
      riparto_circ$CIFRA,
      decreasing = c(FALSE, TRUE, TRUE)
    ),
  ]
  
  riparto_circ$ORDINE <- ave(
    seq_along(riparto_circ$CIRCOSCRIZIONE),
    riparto_circ$CIRCOSCRIZIONE,
    FUN = seq_along
  )
  
  riparto_circ$SEGGIO_DA_RESTO <- 
    riparto_circ$ORDINE <= riparto_circ$DA_ASSEGNARE
  
  riparto_circ$SEGGI <- 
    riparto_circ$PARTE_INTERA + riparto_circ$SEGGIO_DA_RESTO
  
#   b) procede, per ciascuna coalizione di liste, al riparto dei
#   seggi fra le liste collegate ammesse al riparto che abbiano
#   conseguito sul piano nazionale almeno il 3 per cento dei voti validi
#   espressi, nonche' fra le liste collegate che abbiano conseguito
# almeno il 20 per cento dei voti validi espressi nella regione,
# nonche' fra le liste collegate rappresentative di minoranze
#   linguistiche riconosciute, presentate esclusivamente in una regione
#   ad autonomia speciale il cui statuto o le relative norme di
#   attuazione prevedano una particolare tutela di tali minoranze
#   linguistiche, i cui candidati siano stati proclamati eletti in almeno
#   ((un quarto dei collegi uninominali della circoscrizione regionale ai
#     sensi dell'articolo 16, con arrotondamento all'unita' superiore)).
  
  liste_circ$AMMESSA <-
    liste_circ$SOGLIA3 | liste_circ$SOGLIA20 | liste_circ$SOGLIA_MINORANZA

  ammesse_circ <- liste_circ[
    liste_circ$AMMESSA,
    c(
      "CIRCOSCRIZIONE",
      "SOGGETTO_RIPARTO",
      "LISTA",
      "CIFRA"
    )
  ]
  
# A
# tale fine, divide la somma delle cifre elettorali delle liste ammesse
# al riparto per il numero di seggi individuato ai sensi della lettera
# a). Nell'effettuare tale divisione non tiene conto dell'eventuale
# parte frazionaria del quoziente cosi' ottenuto.
  
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
  
# Divide poi la cifra
#     elettorale regionale di ciascuna lista ammessa al riparto per tale
#     quoziente. La parte intera del quoziente cosi' ottenuto rappresenta
# il numero dei seggi da assegnare a ciascuna lista.
  
  ammesse_circ <- merge(
    ammesse_circ,
    riparto_circ[, c("CIRCOSCRIZIONE", "SOGGETTO_RIPARTO", "QUOZIENTE_COAL")]
  )
  
  ammesse_circ$PARTE_INTERA <- 
    ammesse_circ$CIFRA %/% ammesse_circ$QUOZIENTE_COAL
  ammesse_circ$RESTO <- 
    ammesse_circ$CIFRA %% ammesse_circ$QUOZIENTE_COAL
  
# I seggi che
# rimangono ancora da attribuire sono rispettivamente assegnati alle
# liste per le quali queste ultime divisioni abbiano dato i maggiori
# resti e, in caso di parita' di resti, alle liste che abbiano
#     conseguito la maggiore cifra elettorale regionale; a parita' di
# quest'ultima si procede a sorteggio;
  
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
      ammesse_circ$RESTO,
      ammesse_circ$CIFRA,
      decreasing = c(FALSE, FALSE, TRUE, TRUE),
      method = "radix"
    ),
  ]
  
  ammesse_circ$ORDINE <- ave(
    seq_along(ammesse_circ$SOGGETTO_RIPARTO),
    paste(
      ammesse_circ$CIRCOSCRIZIONE,
      ammesse_circ$SOGGETTO_RIPARTO
    ),
    FUN = seq_along
  )
  
  ammesse_circ$SEGGIO_DA_RESTO <- 
    ammesse_circ$ORDINE <= ammesse_circ$DA_ASSEGNARE_COAL
  ammesse_circ$SEGGIO_DA_RESTO[is.na(ammesse_circ$SEGGIO_DA_RESTO)] <- FALSE
  
  ammesse_circ$SEGGI <- ammesse_circ$PARTE_INTERA + ammesse_circ$SEGGIO_DA_RESTO
  
#     c) nelle regioni ripartite in piu' collegi plurinominali, procede
# quindi alla distribuzione nei singoli collegi plurinominali dei seggi
# assegnati alle liste. A tale fine, per ciascun collegio plurinominale
# divide la somma delle cifre elettorali di collegio delle liste alle
# quali devono essere assegnati seggi per il numero dei seggi da
# attribuire nel collegio plurinominale, ottenendo cosi' il quoziente
#     elettorale di collegio. Nell'effettuare tale divisione non tiene
# conto dell'eventuale parte frazionaria del quoziente cosi' ottenuto.
  
  liste_pluri <- merge(
    liste_pluri,
    liste_circ[, c("CIRCOSCRIZIONE", "LISTA", "AMMESSA")]
  )
  
  ammesse_pluri <- liste_pluri[
    liste_pluri$AMMESSA,
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
  
# Divide poi la cifra elettorale di collegio di ciascuna lista per il
# quoziente elettorale di collegio, ottenendo cosi' il quoziente di
#     attribuzione. La parte intera del quoziente di attribuzione
#     rappresenta il numero dei seggi da assegnare a ciascuna lista.
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    totali_pluri[, c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "QUOZIENTE")]
  )
  
  ammesse_pluri$PARTE_INTERA <- 
    ammesse_pluri$CIFRA %/% ammesse_pluri$QUOZIENTE
  ammesse_pluri$DECIMALI <- 
    ( ammesse_pluri$CIFRA / ammesse_pluri$QUOZIENTE ) %% 1
  
#     I seggi che rimangono ancora da attribuire sono rispettivamente
#     assegnati alle liste per le quali queste ultime divisioni hanno dato
#     le maggiori parti decimali e, in caso di parita', alle liste che
# hanno conseguito la maggiore cifra elettorale di collegio; a parita'
#     di quest'ultima si procede a sorteggio. Esclude dall'attribuzione di
#     cui al periodo precedente le liste alle quali e' stato gia'
#     attribuito il numero di seggi ad esse assegnato a seguito delle
#     operazioni di cui alle lettere a) e b).
  
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
    ammesse_circ[,c("CIRCOSCRIZIONE", "LISTA", "ESCLUSE_PLURI")]
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
      ammesse_pluri$CIFRA,
      decreasing = c(FALSE, FALSE, FALSE, TRUE, TRUE)
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
  ammesse_pluri$SEGGIO_DA_DECIMALI[is.na(ammesse_pluri$SEGGIO_DA_DECIMALI)] <- 
    FALSE
  
  ammesse_pluri$SEGGI <- 
    ammesse_pluri$PARTE_INTERA + ammesse_pluri$SEGGIO_DA_DECIMALI
  
#  Successivamente l'ufficio
# accerta se il numero dei seggi assegnati in tutti i collegi
# plurinominali a ciascuna lista corrisponda al numero di seggi
# determinato ai sensi delle lettere a) e b). 
 
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
   
#  In caso negativo,
# determina la lista che ha il maggior numero di seggi eccedentari e, a
# parita' di essi, la lista che tra queste ha ottenuto il seggio
# eccedentario con la minore parte decimale del quoziente; sottrae
# quindi il seggio a tale lista nel collegio in cui e' stato ottenuto
# con la minore parte decimale dei quozienti di attribuzione e lo
# assegna alla lista deficitaria che ha il maggior numero di seggi
# deficitari e, a parita' di essi, alla lista che tra queste ha la
# maggiore parte decimale del quoziente che non ha dato luogo
# all'assegnazione di seggio; il seggio e' assegnato alla lista
# deficitaria nel collegio plurinominale in cui essa ha la maggiore
# parte decimale del quoziente di attribuzione non utilizzata; ripete
# quindi, in successione, tali operazioni sino alla assegnazione di
# tutti i seggi eccedentari alle liste deficitarie.
  
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
    decreasing = c(FALSE, TRUE, TRUE, FALSE)
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
    decreasing = c(FALSE, FALSE, FALSE, TRUE)
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
  
####   Art. 17-bis ####
  
#   ((1. Al termine delle operazioni di cui agli articoli precedenti,
#     l'Ufficio elettorale regionale proclama eletti in ciascun collegio
# plurinominale, nei limiti dei seggi ai quali ciascuna lista ha
# diritto, i candidati compresi nella lista del collegio, secondo
# l'ordine di presentazione.
  
  candidati_pluri_backup <- candidati_pluri
  
  candidati_pluri <- candidati_pluri[
    !(candidati_pluri$CANDIDATO %in% candidati_uni$CANDIDATO[candidati_uni$ELETTO]),
  ]
  
  ammesse_pluri$DECIMALI_USATI <-
    ammesse_pluri$SEGGIO_DA_DECIMALI + 
    ammesse_pluri$RICEVUTO - 
    ammesse_pluri$CEDUTO > 0
  
  ammesse_pluri <- merge(
    ammesse_pluri,
    aggregate(
      CANDIDATO ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
      candidati_pluri,
      length
    ),
    all.x = TRUE
  )
  
  names(ammesse_pluri)[names(ammesse_pluri) == "CANDIDATO"] <- "CANDIDATI"
  
  ammesse_pluri$CANDIDATI[is.na(ammesse_pluri$CANDIDATI)] <- 0
  
  ammesse_pluri$ELETTI <- 
    pmin(ammesse_pluri$SEGGI, ammesse_pluri$CANDIDATI)

#     2. Qualora una lista abbia esaurito il numero dei candidati
#     presentati in un collegio plurinominale e non sia quindi possibile
#     attribuire tutti i seggi a essa spettanti in quel collegio, si
#     applica l'articolo 84 del testo unico delle leggi recanti norme per
# la elezione della Camera dei deputati, di cui al decreto del
# Presidente della Repubblica 30 marzo 1957, n. 361, ad eccezione di
# quanto previsto dai commi 4, 6 e 7.
  
  subentro()
  subentro(livello = "pluri", uni = TRUE)
  subentro(livello = "circ", uni = TRUE)
  subentro(livello = "pluri", coal = TRUE)
  subentro(livello = "circ", coal = TRUE)

# 3. Nel caso di elezione in piu' collegi si applica quanto previsto
#     dall'articolo 85 del testo unico delle leggi recanti norme per la
# elezione della Camera dei deputati, di cui al decreto del Presidente
# della Repubblica 30 marzo 1957, n. 361)).
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
      candidati_pluri$CIRCOSCRIZIONE,
      candidati_pluri$COLLEGIOPLURINOMINALE,
      candidati_pluri$LISTA,
      candidati_pluri$NUMERO
    ),]
    
    candidati_pluri$ORDINE <- ave(
      seq_along(candidati_pluri$CANDIDATO),
      paste(
        candidati_pluri$CIRCOSCRIZIONE,
        candidati_pluri$COLLEGIOPLURINOMINALE,
        candidati_pluri$LISTA
      ),
      FUN = seq_along
    )
    
    candidati_pluri$ELETTO <- candidati_pluri$ORDINE <= candidati_pluri$ELETTI
    
    candidati_pluri <- candidati_pluri[order(
      candidati_pluri$CANDIDATO,
      candidati_pluri$ELETTO,
      candidati_pluri$CIFRA_PERCENTUALE,
      decreasing = c(FALSE, TRUE, FALSE)
    ),]
    
    if (sum(candidati_pluri$ELETTO & duplicated(candidati_pluri$CANDIDATO)) == 0)
      break
    
    candidati_pluri <- candidati_pluri[
      -which(candidati_pluri$ELETTO & duplicated(candidati_pluri$CANDIDATO)),
    ]
    
    ammesse_pluri$CANDIDATI <- NULL
    
    ammesse_pluri <- merge(
      ammesse_pluri,
      aggregate(
        CANDIDATO ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
        candidati_pluri,
        length
      ),
      all.x = TRUE
    )
    
    names(ammesse_pluri)[names(ammesse_pluri) == "CANDIDATO"] <- "CANDIDATI"
    ammesse_pluri$CANDIDATI[is.na(ammesse_pluri$CANDIDATI)] <- 0
    
    ammesse_pluri$ELETTI <- 
      pmin(ammesse_pluri$SEGGI, ammesse_pluri$CANDIDATI)
    
    subentro()
    subentro(livello = "pluri", uni = TRUE)
    subentro(livello = "circ", uni = TRUE)
    subentro(livello = "pluri", coal = TRUE)
    subentro(livello = "circ", coal = TRUE)
  }
}