Hare.Niemeyer <- function(votes, seats, details = FALSE) {
  if (seats > 0) {
    q <- sum(votes) / seats
    quotients <- votes %/% q
    still.to.assign <- seats - sum(quotients)
    remainders <- votes %% q
    remainders.order <- order(remainders, votes, runif(length(votes)), decreasing = TRUE)
    remainders.seats <- rep(0, length(votes))
    remainders.seats[remainders.order[1:still.to.assign]] <- 1
    assigned <- quotients + remainders.seats 
  } else {
    assigned <- rep(0, length(votes))
    remainders <- votes
    remainders.seats <- rep(0, length(votes))
  }
  
  if (details) {
    return( data.frame(assigned = assigned, remainders = remainders, remainders.seats = remainders.seats ))
  } else return(assigned)
}


Scrutinio <- function(
  prov_lista,
  province,
  liste
) {
  
  # load("dati_per_scrutinio.RData")
  
  prov_lista <- prov_lista[prov_lista$LISTA != "astensione", ]
  
  # Art. 3
  # Individuazione dei seggi e delle circoscrizioni provinciali
  
  # 1. Quaranta dei consiglieri assegnati all'Assemblea legislativa sono eletti
  # con criterio proporzionale sulla base di liste circoscrizionali concorrenti ai
  # sensi delle disposizioni di cui all'articolo 12, comma 3, e articolo 13, comma
  # 1, mediante riparto nelle singole circoscrizioni e recupero dei voti residui
  # nel collegio unico regionale. Nove dei consiglieri assegnati alla Regione sono
  # eletti con sistema maggioritario nell'ambito dei candidati concorrenti nelle
  # liste circoscrizionali in base ai voti conseguiti dalle coalizioni di liste o
  # gruppi di liste collegati ai candidati alla carica di Presidente della Giunta
  # regionale ai sensi dell'articolo 13, comma 2, lettere da b) a f). Un seggio è
  # riservato al candidato alla carica di Presidente della Giunta regionale che ha
  # conseguito un numero di voti validi immediatamente inferiore a quello del
  # candidato proclamato eletto Presidente ai sensi dell'articolo 13, comma 3.
  
  # 2. Le circoscrizioni elettorali coincidono con i territori delle province
  # emiliano-romagnole di cui all'articolo 1, comma 2, dello Statuto regionale. La
  # ripartizione dei seggi tra le circoscrizioni è effettuata dividendo il numero
  # degli abitanti della regione per i quaranta seggi di cui al primo comma del
  # presente articolo e assegnando i seggi in proporzione alla popolazione di ogni
  # circoscrizione sulla base dei quozienti interi e dei più alti resti. La
  # popolazione è determinata in base ai risultati dell'ultimo censimento generale
  # della stessa, riportati dalla più recente pubblicazione ufficiale
  # dell'Istituto nazionale di statistica.
  
  
  
  
  province$seggi_proporzionali <- Hare.Niemeyer(province$POP_2011, 40)
  
  
  
  # Art. 11
  # Soglie di sbarramento
  
  # 1. Non sono ammesse all'assegnazione dei seggi le liste circoscrizionali il
  # cui gruppo abbia ottenuto, nell'intera regione, meno del tre per cento dei
  # voti validi, se non collegato ad un candidato Presidente che ha ottenuto
  # almeno il cinque per cento dei voti nella relativa elezione.
  
  liste <- merge(
    liste,
    aggregate(VOTI_LISTA_ITER ~ LISTA, data = prov_lista, sum)
  )
  
  voti_validi <- sum(liste$VOTI_LISTA_ITER)
  
  liste$PERCENTUALE_DEI_VOTI_VALIDI <- liste$VOTI_LISTA_ITER / voti_validi
  
  liste$SOGLIA_3 <- liste$PERCENTUALE_DEI_VOTI_VALIDI >= 0.03
  
  coalizioni <- aggregate(
    VOTI_LISTA_ITER ~ COALIZIONE,
    liste,
    sum
  )
  
  coalizioni$PERCENTUALE_DEI_VOTI_VALIDI <- 
    coalizioni$VOTI_LISTA_ITER / voti_validi
  
  coalizioni$SOGLIA_COALIZIONE <- coalizioni$PERCENTUALE_DEI_VOTI_VALIDI >= 0.05
  
  liste <- merge(
    liste,
    coalizioni[, c(
      "COALIZIONE",
      "SOGLIA_COALIZIONE"
    )]
  )
  
  liste$SOGLIA <- liste$SOGLIA_3 | liste$SOGLIA_COALIZIONE
  
  liste$VOTI_UTILI <- liste$VOTI_LISTA_ITER * liste$SOGLIA
  
  prov_lista <- merge(
    prov_lista,
    liste[, c(
      "LISTA",
      "SOGLIA"
    )]
  )
  
  prov_lista$VOTI_UTILI <- prov_lista$VOTI_LISTA_ITER * prov_lista$SOGLIA
  
  # Art. 12
  # Operazioni degli uffici centrali circoscrizionali
  

  # 3. Compiute le suddette operazioni, l'ufficio centrale circoscrizionale:

  # a) somma i voti validi, compresi quelli assegnati ai sensi del comma 1,
  # lettera b), ottenuti da ciascun candidato alla carica di Presidente della
  # Giunta regionale nelle singole sezioni della circoscrizione;

  # b) determina la cifra elettorale circoscrizionale di ciascuna lista
  # circoscrizionale. La cifra elettorale circoscrizionale di ogni lista
  # circoscrizionale è data dalla somma dei voti di lista validi, compresi
  # quelli assegnati ai sensi del comma 1, lettera b), ottenuti da ciascuna
  # lista nelle singole sezioni della circoscrizione;

  # c) procede al riparto dei seggi tra le liste in base alla cifra elettorale
  # di ciascuna lista. A tal fine divide il totale delle cifre elettorali di
  # tutte le liste per il numero dei seggi assegnati alla circoscrizione più
  # uno, ottenendo così il quoziente elettorale circoscrizionale;
  # nell'effettuare la divisione trascura la eventuale parte frazionaria del
  # quoziente.
  
  
  
  province <- merge(
    province,
    aggregate(
      VOTI_UTILI ~
        PROVINCIA,
      prov_lista,
      sum
    )
  )
  
  province$QUOZIENTE_1 <- 
    floor(province$VOTI_UTILI / (province$seggi_proporzionali + 1))
  
  # Attribuisce quindi ad ogni lista tanti seggi quante volte il
  # quoziente elettorale risulti contenuto nella cifra elettorale di ciascuna
  # lista.
  
  prov_lista <- merge(
    prov_lista,
    province[, c(
      "PROVINCIA",
      "QUOZIENTE_1"
    )]
  )
  
  prov_lista$SEGGI_1 <- 0
  
  prov_lista$SEGGI_1 <- 
    floor(prov_lista$VOTI_UTILI / prov_lista$QUOZIENTE_1)
  
  # Se, con il quoziente così calcolato, il numero dei seggi da
  # attribuire in complesso alle liste superi quello dei seggi assegnati alla
  # circoscrizione, le operazioni si ripetono con un nuovo quoziente ottenuto
  # diminuendo di una unità il divisore. 
  
  province <- merge(
    province,
    aggregate(
      SEGGI_1 ~ PROVINCIA,
      prov_lista,
      sum
    )
  )
  
  province$QUOZIENTE_2 <-
    floor(province$VOTI_UTILI / province$seggi_proporzionali)
  
  prov_lista <- merge(
    prov_lista,
    province[, c(
      "PROVINCIA",
      "QUOZIENTE_2"
    )]
  )
  
  prov_lista$SEGGI_2 <- 0
  
  prov_lista$SEGGI_2 <- 
    floor(prov_lista$VOTI_UTILI / prov_lista$QUOZIENTE_2)
  
  province$USA_2 <- province$SEGGI_1 > province$seggi_proporzionali
  
  prov_lista <- merge(
    prov_lista,
    province[, c(
      "PROVINCIA",
      "USA_2"
    )]
  )
  
  prov_lista$SEGGI_CIRC <- ifelse(
    prov_lista$USA_2,
    prov_lista$SEGGI_2,
    prov_lista$SEGGI_1
  )
  
  # I seggi che rimangono non assegnati
  # vengono attribuiti al collegio unico regionale;
  
  province <- merge(
    province,
    aggregate(
      SEGGI_CIRC ~ PROVINCIA,
      prov_lista,
      sum
    )
  )
  
  seggi_non_assegnati <- 
    sum(province$seggi_proporzionali) - sum(province$SEGGI_CIRC)
  
  # d) stabilisce la somma dei voti residuati di ogni lista e il numero dei
  # seggi non potuti attribuire ad alcuna lista per insufficienza di quozienti o
  # di candidati. La determinazione della somma dei voti residuati deve essere
  # fatta anche nel caso che tutti i seggi assegnati alla circoscrizione vengano
  # attribuiti. Si considerano voti residuati anche quelli delle liste che non
  # abbiano raggiunto alcun quoziente ed i voti che, pur raggiungendo il
  # quoziente, rimangano inefficienti per mancanza di candidati;
  
  prov_lista$VOTI_RESIDUATI <- 
    prov_lista$VOTI_UTILI -
    ifelse(
      prov_lista$USA_2,
      prov_lista$QUOZIENTE_2 * prov_lista$SEGGI_2,
      prov_lista$QUOZIENTE_1 * prov_lista$SEGGI_1
    )
  
  # Art.13
  # Operazioni dell'ufficio centrale regionale
  
  # 1. L'ufficio centrale regionale, ricevuti gli estratti dei verbali da tutti
  # gli uffici centrali circoscrizionali:
  
  # a) determina il numero dei seggi non attribuiti nelle circoscrizioni;
  
  # b) determina, per ciascuna lista, il numero dei voti residuati.
  # Successivamente procede alla somma dei predetti voti per tutte le liste
  # aventi lo stesso contrassegno;
  
  liste <- merge(
    liste,
    aggregate(
      VOTI_RESIDUATI ~ LISTA,
      prov_lista,
      sum
    )
  )
  
  # c) procede alla assegnazione ai predetti gruppi di liste dei seggi indicati
  # alla lettera a). A tal fine divide la somma dei voti residuati di tutti i
  # gruppi di liste per il numero dei seggi da attribuire; nell'effettuare la
  # divisione, trascura la eventuale parte frazionaria del quoziente. Il
  # risultato costituisce il quoziente elettorale regionale. Divide, poi, la
  # somma dei voti residuati di ogni gruppo di liste per tale quoziente: il
  # risultato rappresenta il numero dei seggi da assegnare a ciascun gruppo. I
  # seggi che rimangono ancora da attribuire sono rispettivamente assegnati ai
  # gruppi per i quali queste ultime divisioni hanno dato maggiori resti e, in
  # caso di parità di resti, a quei gruppi che abbiano avuto maggiori voti
  # residuati. A parità anche di questi ultimi si procede a sorteggio.
  
  sr <- Hare.Niemeyer(liste$VOTI_RESIDUATI, seggi_non_assegnati, TRUE)
  liste$SEGGI_DA_VOTI_RESIDUATI <- sr$assigned
  liste$RESTI_VOTI_RESIDUATI <- sr$remainders
  liste$SEGGI_DA_RESTI_VOTI_RESIDUATI <- sr$remainders.seats
  
  # I seggi
  # spettanti a ciascun gruppo di liste vengono attribuiti alle rispettive liste
  # nelle singole circoscrizioni seguendo la graduatoria decrescente dei voti
  # residuati espressi in percentuale del relativo quoziente circoscrizionale. A
  # tal fine si moltiplica per cento il numero dei voti residuati di ciascuna
  # lista e si divide il prodotto per il quoziente circoscrizionale. Qualora in
  # una circoscrizione fosse assegnato un seggio ad una lista i cui candidati
  # fossero già stati tutti esauriti, l'ufficio centrale regionale attribuisce
  # il seggio alla lista di un'altra circoscrizione proseguendo nella
  # graduatoria anzidetta.
  
  # DOPO
  
  # 2. L'ufficio centrale regionale procede al riparto della restante quota di
  # seggi. A tal fine effettua le seguenti operazioni:
  
  # a) proclama eletto alla carica di Presidente della Giunta regionale il
  # candidato Presidente che nella Regione ha ottenuto il maggior numero di voti
  # validi sommando i voti ottenuti da ciascun candidato alla carica di
  # Presidente della Giunta regionale nelle singole circoscrizioni di cui
  # all'articolo 12, comma 3, lettera a). Individua, altresì, il candidato alla
  # carica di Presidente che ha ottenuto il totale dei voti validi
  # immediatamente inferiore al candidato proclamato eletto, ai fini della
  # riserva di un seggio da effettuare con le modalità di cui al comma 3;
  
  coalizioni$CLASSIFICA <- rank(
    - coalizioni$VOTI_LISTA_ITER, 
    ties.method = "random"
  )
  liste <- merge(
    liste,
    coalizioni[, c(
      "COALIZIONE",
      "CLASSIFICA"
    )]
  )
  
  # b) determina la cifra elettorale regionale di ciascun gruppo di liste
  # circoscrizionali, sommando le cifre elettorali circoscrizionali attribuite
  # alle liste circoscrizionali di ogni gruppo ai sensi dell'articolo 12, comma
  # 3, lettera b);
  
  
  # c) determina la cifra elettorale regionale attribuita alla coalizione di
  # liste ovvero al gruppo di liste non riunito in coalizione con cui il
  # Presidente della Giunta regionale eletto ha dichiarato collegamento sommando
  # le cifre elettorali circoscrizionali attribuite alle singole liste
  # circoscrizionali che ne fanno parte;
  coalizioni <- merge(
    coalizioni,
    aggregate(
      VOTI_UTILI ~ COALIZIONE,
      liste,
      sum
    )
  )
  
  # individua altresì il totale
  # dei seggi assegnati ai sensi dell'articolo 12, comma 3, e del comma 1 del
  # presente articolo, al gruppo di liste o alla coalizione collegati al
  # candidato alla carica di Presidente della Giunta regionale eletto;
  
  liste <- merge(
    liste,
    aggregate(
      SEGGI_CIRC ~ LISTA,
      prov_lista,
      sum
    )
  )
  
  liste$SEGGI_40 <- liste$SEGGI_CIRC + liste$SEGGI_DA_VOTI_RESIDUATI
  
  coalizioni <- merge(
    coalizioni,
    aggregate(
      SEGGI_40 ~ COALIZIONE,
      liste,
      sum
    )
  )
  
  # d) qualora il gruppo di liste o la coalizione di liste collegati al
  # candidato eletto Presidente della Giunta regionale abbia conseguito con
  # l'assegnazione di cui all'articolo 12, comma 3, e del comma 1 del presente
  # articolo, un numero di seggi superiore a ventiquattro, escluso il seggio
  # riservato al Presidente della Regione, assegna al medesimo gruppo di liste o
  # gruppi di liste che fanno parte della coalizione, quattro seggi di cui al
  # secondo periodo dell'articolo 3, comma 1.
  
  # A tal fine divide la somma delle cifre elettorali conseguite dai gruppi di
  # liste circoscrizionali in questione per il numero dei seggi da ripartire;
  # nell'effettuare l'operazione, trascura la eventuale parte frazionaria del
  # quoziente. Divide poi la cifra elettorale di ciascun gruppo di liste per il
  # quoziente così ottenuto: il risultato rappresenta il numero di seggi da
  # assegnare a ciascun gruppo. I seggi che rimangono ancora da attribuire sono
  # assegnati ai gruppi per i quali queste ultime divisioni hanno dato maggiori
  # resti e, in caso di parità di resti, ai gruppi che hanno conseguito le
  # maggiori cifre elettorali.
  
  # I seggi spettanti a ciascun gruppo di liste sono attribuiti nelle singole
  # circoscrizioni secondo le modalità di cui al comma 1, lettera c), settimo,
  # ottavo e nono periodo, ad iniziare dalla prima circoscrizione alla quale non
  # è stato ancora attribuito il seggio ai sensi del comma 1, lettera c),
  # settimo e ottavo periodo. Qualora tutti i posti della graduatoria abbiano
  # già dato luogo all'assegnazione di seggi, l'attribuzione di ulteriori seggi
  # ha nuovamente inizio a partire dalla prima circoscrizione della medesima
  # graduatoria.
  
  # I restanti cinque seggi da assegnare sono ripartiti tra i gruppi di liste
  # circoscrizionali non collegati al candidato alla carica di presidente eletto
  # con le modalità previste nei precedenti periodi;
  
  # e) qualora il gruppo di liste o la coalizione di liste collegati al
  # candidato eletto Presidente della Giunta regionale abbia conseguito, con
  # l'assegnazione di cui all'articolo 12, comma 3, e di cui al comma 1 del
  # presente articolo, un numero di seggi pari o inferiore a ventiquattro,
  # assegna al medesimo gruppo di liste o gruppi di liste che fanno parte della
  # coalizione, i nove seggi di cui all'articolo 3, comma 1, secondo periodo, li
  # ripartisce fra le medesime liste e li attribuisce nelle singole
  # circoscrizioni secondo le modalità di cui alla lettera d).
  
  liste$SEGGI_BONUS <- 0
  liste$SEGGI_BONUS_RESTI <- 0
  liste$SEGGI_BONUS_DA_RESTI <- 0
  
  if (coalizioni$SEGGI_40[coalizioni$CLASSIFICA == 1] > 24) {
    bonus_vincitori <- 4
    bonus_vinti <- 5
  } else {
    bonus_vincitori <- 9
    bonus_vinti <- 0
  }
  
  
  
  sr <- Hare.Niemeyer(
    liste$VOTI_UTILI[liste$CLASSIFICA != 1],
    bonus_vinti,
    TRUE
  )
  
  liste$SEGGI_BONUS[liste$CLASSIFICA != 1] <- sr$assigned
  liste$SEGGI_BONUS_RESTI[liste$CLASSIFICA != 1] <- sr$remainders
  liste$SEGGI_BONUS_DA_RESTI[liste$CLASSIFICA != 1] <- sr$remainders.seats
  
  # Verifica quindi se la cifra elettorale regionale conseguita dalla coalizione
  # di liste ovvero dal gruppo di liste non riunito in coalizione con cui il
  # Presidente della Giunta regionale eletto ha dichiarato collegamento, sia
  # pari o superiore al quaranta per cento del totale dei voti validi conseguiti
  # da tutte le coalizioni o gruppi di liste collegati ai candidati alla carica
  # di Presidente;
  
  # f) nel caso in cui la verifica prevista dal secondo periodo della lettera
  # e), dia esito negativo, verifica se il totale dei seggi conseguiti dal
  # gruppo di liste o dalla coalizione di liste collegati al candidato eletto
  # Presidente della Giunta regionale a seguito dell'assegnazione dei nove seggi
  # di cui al primo periodo della lettera e), sia pari o superiore a ventisette,
  # escluso il seggio riservato al Presidente della Giunta regionale;
  
  liste$SEGGI_CIRC_DA_TOGLIERE <- 0
  
  if (
    coalizioni$VOTI_UTILI[coalizioni$CLASSIFICA == 1] < 0.4 * sum(coalizioni$VOTI_UTILI) &
    ( coalizioni$SEGGI_40[coalizioni$CLASSIFICA == 1] + bonus_vincitori ) < 27
  ) {
    # qualora tale seconda verifica dia esito negativo, assegna con le modalità
    # di cui alla lettera d) una quota aggiuntiva di seggi al gruppo di liste o
    # ai gruppi di liste riuniti in coalizione collegati con il candidato
    # Presidente eletto fino al raggiungimento dei ventisette seggi.
    seggi_da_spostare <- 27 - ( coalizioni$SEGGI_40[coalizioni$CLASSIFICA == 1] + bonus_vincitori)
    bonus_vincitori <- bonus_vincitori + seggi_da_spostare
    
    # Tali seggi aggiuntivi vengono tolti alle liste circoscrizionali non
    # collegate al candidato alla carica di Presidente eletto a partire dai
    # seggi assegnati con il resto minore o il minor voto residuo ai sensi del
    # comma 1, e in subordine, qualora tutti i seggi siano stati assegnati con
    # quoziente intero in sede circoscrizionale, vengono tolti i seggi
    # attribuiti alle liste circoscrizionali non collegate al candidato alla
    # carica di Presidente eletto che hanno riportato la minore cifra
    # elettorale. A parità anche di queste ultime si procede a sorteggio.
    
    while (seggi_da_spostare > 0) {
      if (sum(liste$SEGGI_DA_RESTI_VOTI_RESIDUATI[liste$CLASSIFICA != 1]) > 0) {
        riga <- which(
          liste$RESTI_VOTI_RESIDUATI == min(
            liste$RESTI_VOTI_RESIDUATI[
              liste$SEGGI_DA_RESTI_VOTI_RESIDUATI > 0 & liste$CLASSIFICA != 1
            ]
          ) & liste$CLASSIFICA != 1
        )
        if (length(riga) > 1) riga <- sample(riga, 1)
        liste$SEGGI_DA_RESTI_VOTI_RESIDUATI[riga] <- 
          liste$SEGGI_DA_RESTI_VOTI_RESIDUATI[riga] -1
        liste$SEGGI_DA_VOTI_RESIDUATI[riga] <-
          liste$SEGGI_DA_VOTI_RESIDUATI[riga] - 1
        seggi_da_spostare <- seggi_da_spostare - 1
      } else if (sum(liste$SEGGI_DA_VOTI_RESIDUATI[liste$CLASSIFICA != 1]) > 0) {
        riga <- which(
          liste$VOTI_RESIDUATI == min(
            liste$VOTI_RESIDUATI[
              liste$SEGGI_DA_VOTI_RESIDUATI > 0 & liste$CLASSIFICA != 1
            ]
          ) & liste$CLASSIFICA != 1
        )
        if (length(riga) > 1) riga <- sample(riga, 1)
        liste$SEGGI_DA_VOTI_RESIDUATI[riga] <-
          liste$SEGGI_DA_VOTI_RESIDUATI[riga] - 1
        seggi_da_spostare <- seggi_da_spostare - 1
      } else {
        riga <- which(
          liste$VOTI_UTILI == min(
            liste$VOTI_UTILI[
              liste$SEGGI_CIRC > 0 & liste$CLASSIFICA != 1
            ]
          ) & liste$CLASSIFICA != 1
        )
        if (length(riga) > 1) riga <- sample(riga, 1)
        liste$SEGGI_CIRC[riga] <-
          liste$SEGGI_CIRC[riga] - 1
        seggi_da_spostare <- seggi_da_spostare - 1
        
        liste$SEGGI_CIRC_DA_TOGLIERE[riga] <- 
          liste$SEGGI_CIRC_DA_TOGLIERE[riga] + 1 
        
      }
    }
    
  }
  
  liste$SEGGI_BONUS[liste$CLASSIFICA == 1] <- Hare.Niemeyer(
    liste$VOTI_UTILI[liste$CLASSIFICA == 1],
    bonus_vincitori
  )
  
  # 3. L'ufficio centrale regionale proclama eletto alla carica di consigliere
  # il candidato alla carica di Presidente della Giunta regionale che ha
  # conseguito un numero di voti validi immediatamente inferiore a quello del
  # candidato proclamato eletto Presidente. A tal fine è utilizzato l'ultimo dei
  # seggi eventualmente spettante alle liste circoscrizionali collegate con il
  # medesimo candidato non eletto alla carica di Presidente della Giunta
  # regionale assegnato ai sensi del comma 2, lettera d), ultimo periodo; in
  # subordine è utilizzato il seggio attribuito con il resto minore o il minor
  # voto residuo ai sensi del comma 1 tra quelli delle stesse liste; in
  # subordine, qualora tutti i seggi spettanti alle liste collegate siano stati
  # assegnati con quoziente intero in sede circoscrizionale, l'ufficio centrale
  # regionale riserva il seggio che sarebbe stato attribuito alla lista
  # circoscrizionale collegata che ha riportato la minore cifra elettorale. A
  # parità anche di queste ultime si procede a sorteggio.
  
  if (sum(liste$SEGGI_BONUS_DA_RESTI[liste$CLASSIFICA == 2]) > 0) {
    riga <- which(
      liste$SEGGI_BONUS_RESTI == min(
        liste$SEGGI_BONUS_RESTI[
          liste$SEGGI_BONUS_DA_RESTI > 0 & liste$CLASSIFICA == 2
        ]
      ) & liste$CLASSIFICA == 2
    )
    if (length(riga) > 1) riga <- sample(riga, 1)
    liste$SEGGI_BONUS_DA_RESTI[riga] <- 
      liste$SEGGI_BONUS_DA_RESTI[riga] -1
    liste$SEGGI_BONUS[riga] <-
      liste$SEGGI_BONUS[riga] - 1
  } else if  (sum(liste$SEGGI_DA_RESTI_VOTI_RESIDUATI[liste$CLASSIFICA == 2]) > 0) {
    riga <- which(
      liste$RESTI_VOTI_RESIDUATI == min(
        liste$RESTI_VOTI_RESIDUATI[
          liste$SEGGI_DA_RESTI_VOTI_RESIDUATI > 0 & liste$CLASSIFICA == 2
        ]
      ) & liste$CLASSIFICA == 2
    )
    if (length(riga) > 1) riga <- sample(riga, 1)
    liste$SEGGI_DA_RESTI_VOTI_RESIDUATI[riga] <- 
      liste$SEGGI_DA_RESTI_VOTI_RESIDUATI[riga] -1
    liste$SEGGI_DA_VOTI_RESIDUATI[riga] <-
      liste$SEGGI_DA_VOTI_RESIDUATI[riga] - 1
  } else if (sum(liste$SEGGI_DA_VOTI_RESIDUATI[liste$CLASSIFICA == 2]) > 0) {
    riga <- which(
      liste$VOTI_RESIDUATI == min(
        liste$VOTI_RESIDUATI[
          liste$SEGGI_DA_VOTI_RESIDUATI > 0 & liste$CLASSIFICA == 2
        ]
      ) & liste$CLASSIFICA == 2
    )
    if (length(riga) > 1) riga <- sample(riga, 1)
    liste$SEGGI_DA_VOTI_RESIDUATI[riga] <-
      liste$SEGGI_DA_VOTI_RESIDUATI[riga] - 1
  } else {
    riga <- which(
      liste$VOTI_UTILI == min(
        liste$VOTI_UTILI[
          liste$SEGGI_CIRC > 0 & liste$CLASSIFICA == 2
        ]
      ) & liste$CLASSIFICA == 2
    )
    if (length(riga) > 1) riga <- sample(riga, 1)
    liste$SEGGI_CIRC[riga] <-
      liste$SEGGI_CIRC[riga] - 1
    
    
    liste$SEGGI_CIRC_DA_TOGLIERE[riga] <- 
      liste$SEGGI_CIRC_DA_TOGLIERE[riga] + 1 
  }
  
  #VEDI SOPRA Art. 13 comma 1 lett c
  
  # TODO: spostare fuori dalla funzione la determinazione del numero dei candidati
  prov_lista <- merge(
    prov_lista,
    province[, c(
      "PROVINCIA",
      "seggi_proporzionali"
    )]
  )
  names(prov_lista)[names(prov_lista) == "seggi_proporzionali"] <- "CANDIDATI"
  
  prov_lista$VOTI_RESIDUATI_RELATIVI <- 
    prov_lista$VOTI_RESIDUATI / ifelse(
      prov_lista$USA_2,
      prov_lista$QUOZIENTE_2,
      prov_lista$QUOZIENTE_1
    )
  
  prov_lista$SEGGI_DA_VOTI_RESIDUATI <- 0
  
  prov_lista$ORDINE_VOTI_RESIDUATI <- ave(
    prov_lista$VOTI_RESIDUATI_RELATIVI,
    prov_lista$LISTA,
    FUN = function(x) rank(- x, ties.method = "random")
  )
  
  prov_lista$ORDINE_INVERSO_VR <- 
    1 + dim(province)[1] - prov_lista$ORDINE_VOTI_RESIDUATI
  
  for (i in 1:dim(liste)[1]) {
    seggi_da_rimuovere <- liste$SEGGI_CIRC_DA_TOGLIERE[i]
    
    j <- 0
    while (seggi_da_rimuovere > 0) {
      
      riga <- which(
        prov_lista$LISTA == liste$LISTA[i] &
          prov_lista$ORDINE_INVERSO_VR == ( (j %% dim(province)[1]) + 1 )
      )
      
      if (prov_lista$SEGGI_CIRC[riga]  > 0) {
        
        prov_lista$SEGGI_CIRC[riga] <- 
          prov_lista$SEGGI_CIRC[riga] - 1
        
        seggi_da_rimuovere <- seggi_da_rimuovere - 1
        
        prov_lista$VOTI_RESIDUATI[riga] <- 
          prov_lista$VOTI_RESIDUATI[riga] + ifelse(
            prov_lista$USA_2[riga],
            prov_lista$QUOZIENTE_2[riga],
            prov_lista$QUOZIENTE_1[riga]
          )
        
      }
      j <- j + 1
      if (j > 1000) stop("Art. 13 comma 1 lettera c: impossibile togliere tutti i seggi alle circoscrizioni")
      
    }
    
    seggi_da_assegnare <- 
      liste$SEGGI_DA_VOTI_RESIDUATI[i] + liste$SEGGI_BONUS[i]
    j <- 0
    while (seggi_da_assegnare > 0) {
      riga <- which(
        prov_lista$LISTA == liste$LISTA[i] &
          prov_lista$ORDINE_VOTI_RESIDUATI == ( (j %% dim(province)[1]) + 1 )
      )
      if (
        prov_lista$CANDIDATI[riga] - 
        prov_lista$SEGGI_CIRC[riga] - 
        prov_lista$SEGGI_DA_VOTI_RESIDUATI[riga] > 0
      ) {
        prov_lista$SEGGI_DA_VOTI_RESIDUATI[riga] <- 
          prov_lista$SEGGI_DA_VOTI_RESIDUATI[riga] + 1
        seggi_da_assegnare <- seggi_da_assegnare - 1
      }
      j <- j + 1
      if (j > 1000) stop("Art. 13 comma 1 lettera c: impossibile assegnare tutti i seggi alle circoscrizioni")
    }
    
  }
  
  coalizioni$PRESIDENTE <- coalizioni$CLASSIFICA == 1
  coalizioni$MIGLIOR_PERDENTE <- coalizioni$CLASSIFICA == 2
  
  prov_lista$ELETTI <- 
    prov_lista$SEGGI_CIRC + prov_lista$SEGGI_DA_VOTI_RESIDUATI
  
  return(list(
    coalizioni = coalizioni[, c(
      "COALIZIONE",
      "PRESIDENTE",
      "MIGLIOR_PERDENTE"
    )],
    prov_lista = prov_lista[, c(
      "PROVINCIA",
      "LISTA",
      "ELETTI"
    )]
  ))
  
  
}