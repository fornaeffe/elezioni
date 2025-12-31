#' Determinazione del numero di consiglieri del consiglio comunale
#'
#' @param popolazione vettore di interi, popolazione legale dei comuni
#' @param capoluogo_provincia vettore booleano, TRUE se il comune è capoluogo di provincia
#'
#' @returns un vettore di numeri interi, corrispondenti al numero di consiglieri per ciascun comuned
#' @export
#'
#' @examples
numero_consiglieri <- function(popolazione, capoluogo_provincia = FALSE) {
  
  # DECRETO LEGISLATIVO 18 agosto 2000, n. 267 - Art.37  
  #  1. Il consiglio comunale è composto dal sindaco e:
  #     
  #     a) da 60 membri nei comuni con popolazione superiore ad un milione di abitanti;
  # b) da 50 membri nei comuni con popolazione superiore a 500.000 abitanti;
  # c) da 46 membri nei comuni con popolazione superiore a 250.000 abitanti.
  # d) da 40 membri nei comuni con popolazione superiore a 100.000 abitanti o che, pur avendo popolazione inferiore, siano capoluoghi di provincia;
  # e) da 30 membri nei comuni con popolazione superiore a 30.000 abitanti;
  # f) da 20 membri nei comuni con popolazione superiore a 10.000 abitanti;
  # g) da 16 membri nei comuni con popolazione superiore a 3.000 abitanti;
  # h) da 12 membri negli altri comuni.
  
  res <- integer(length(popolazione))
  
  res[popolazione > 1e6] <- 60
  res[popolazione > 5e5 & popolazione <= 1e6] <- 50
  res[popolazione > 2.5e5 & popolazione <= 5e5] <- 46
  res[
    (popolazione > 1e5 & popolazione <= 2.5e5) |
      (popolazione <= 1e5 & capoluogo_provincia)
  ] <- 40
  res[popolazione > 3e4 & popolazione <= 1e5 & !capoluogo_provincia] <- 30
  res[popolazione > 1e4 & popolazione <= 3e4 & !capoluogo_provincia] <- 20
  res[popolazione > 3e3 & popolazione <= 1e4 & !capoluogo_provincia] <- 16
  res[popolazione <= 3e3 & !capoluogo_provincia] <- 12
  
  # LEGGE 23 dicembre 2009, n. 191 - art. 2
  # 184. In relazione alle riduzioni del contributo ordinario di cui al comma 
  # 183, il numero dei consiglieri comunali e dei consiglieri provinciali è 
  # ridotto del 20 per cento. L'entità della riduzione è determinata con 
  # arrotondamento all'unità superiore. Ai fini della riduzione del numero 
  # dei consiglieri comunali e dei consiglieri provinciali di cui al primo 
  # periodo non sono computati il sindaco e il presidente della provincia.
  
  res <- floor(res * 0.8)
  
  res
}


#' Scrutinio delle elezioni comunali
#' 
#' @param liste le liste che si presentano alle elezioni, un data.table con le
#' seguenti colonne:
#' \describe{
#'  \item{LISTA}{string, nome della lista}
#'  \item{CANDIDATO_SINDACO}{string, cognome e nome del candidato sindaco 
#'  collegato alla lista}
#'  \item{VOTI_LISTA}{number, voti ricevuti dalla lista}
#'  \item{NUM_CANDIDATI}{number, OPZIONALE, numero di candidati della lista}
#' }
#' @param candidati_sindaci i candidati sindaci che si presentano alle elezioni,
#' un data.table con le seguenti colonne:
#' \describe{
#'  \item{CANDIDATO_SINDACO}{string, cognome e nome del candidato sindaco, eventualmente
#'  disambiguato con la data di nascita}
#'  \item{DATA_DI_NASCITA}{Date, data di nascita del candidato sindaco}
#'  \item{VOTI_SINDACO}{number, voti ricevuti dal sindaco al primo turno}
#'  \item{VOTI_BALLOTTAGGIO}{number, voti ricevuti dal sindaco al ballottaggio}
#' }
#' @param pop_legale integer: la popolazione legale del comune
#' @param num_consiglieri integer: numero di consiglieri da eleggere
#' @param candidati OPZIONALE, i candidati consiglieri che si presentano alle elezioni, un
#' data.table con le seguenti colonne:
#' \describe{
#'  \item{CANDIDATO_CONSIGLIERE}{string, cognome e nome del candidato consigliere,
#'  eventualmente disambiguato con la data di nascita}
#'  \item{DATA_DI_NASCITA}{Date, data di nascita del candidato sindaco}
#'  \item{LISTA}{string, nome della lista di appartenenza del candidato}
#'  \item{PREFERENZE}{number, numero di preferenze ricevute}
#' }
#'
#' @returns
#' @export
#'
#' @examples
scrutinio_comunali <- function(
    liste,
    candidati_sindaci,
    pop_legale,
    num_consiglieri,
    candidati = NULL
) {

  
  candidati_sindaci <- candidati_sindaci[
    liste[
      ,
      .(VOTI_LISTA = sum(VOTI_LISTA)),
      by = .(CANDIDATO_SINDACO)
    ],
    on = .(CANDIDATO_SINDACO)
  ]
  
  # DECRETO LEGISLATIVO 18 agosto 2000, n. 267
  # Art. 72
  # 4.È proclamato eletto sindaco il candidato alla carica che ottiene la 
  # maggioranza assoluta dei voti validi.
  
  # 5. Qualora nessun candidato ottenga la maggioranza di cui al comma 4, 
  # si procede ad un secondo turno elettorale che ha luogo la seconda domenica 
  # successiva a quella del primo. Sono ammessi al secondo turno i due candidati 
  # alla carica di sindaco che hanno ottenuto al primo turno il maggior numero di 
  # voti. In caso di parità di voti tra i candidati, è ammesso al ballottaggio il 
  # candidato collegato con la lista o il gruppo di liste per l'elezione del 
  # consiglio comunale che ha conseguito la maggiore cifra elettorale complessiva. 
  # A parità di cifra elettorale, partecipa al ballottaggio il candidato più 
  # anziano di età.
  data.table::setorder(candidati_sindaci, -VOTI_SINDACO, -VOTI_LISTA)
  
  candidati_sindaci[
    ,
    `:=`(
      PERCENTUALE_SINDACO = VOTI_SINDACO / sum(VOTI_SINDACO),
      PERCENTUALE_LISTE = VOTI_LISTA / sum(VOTI_LISTA)
    )
  ]
  
  ballottaggio <- candidati_sindaci$PERCENTUALE_SINDACO[1] <= 0.5
  
  # 9. Dopo il secondo turno è proclamato eletto sindaco il candidato che ha 
  # ottenuto il maggior numero di voti validi. In caso di parità di voti. è 
  # proclamato eletto sindaco il candidato collegato. ai sensi del comma 7, con 
  # la lista o il gruppo di liste per l'elezione del consiglio comunale che ha 
  # conseguito la maggiore cifra elettorale complessiva. A parità di cifra 
  # elettorale, è proclamato eletto sindaco il candidato più anziano d'età.
  
  if (ballottaggio) data.table::setorder(candidati_sindaci, -VOTI_BALLOTTAGGIO, -VOTI_LISTA, DATA_DI_NASCITA)
  
  # TODO simulare ballottaggio
  
  candidati_sindaci[
    ,
    SINDACO := c(TRUE, rep(FALSE, .N - 1))
  ]
  
  # Art. 73 - Elezione del consiglio comunale nei comuni con popolazione 
  # superiore a 15.000 abitanti
  
  # 7. Non sono ammesse all'assegnazione dei seggi quelle liste che abbiano 
  # ottenuto al primo turno meno del 3 per cento dei voti validi e che non 
  # appartengano a nessun gruppo di liste che abbia superato tale soglia.
  
  
  
  candidati_sindaci[
    ,
    SOGLIA_GRUPPO := PERCENTUALE_LISTE >= 0.03
  ]
  
  liste <- liste[
    candidati_sindaci[, .(CANDIDATO_SINDACO, SOGLIA_GRUPPO)],
    on = .(CANDIDATO_SINDACO)
  ]
  
  liste[
    ,
    PERCENTUALE := VOTI_LISTA / sum(VOTI_LISTA),
  ]
  
  liste[
    ,
    SOGLIA_LISTA := PERCENTUALE >= 0.03
  ]
  
  liste[
    ,
    SOGLIA := SOGLIA_GRUPPO | SOGLIA_LISTA
  ]
  
  # 8. Salvo quanto disposto dal comma 10, per l'assegnazione del numero dei
  # consiglieri a ciascuna lista o a ciascun gruppo di liste collegate, nel
  # turno di elezione del sindaco, con i rispettivi candidati alla carica di
  # sindaco si divide la cifra elettorale di ciascuna lista o gruppo di liste
  # collegate successivamente per 1, 2, 3, 4, sino a concorrenza del numero dei
  # consiglieri da eleggere e quindi si scelgono, fra i quozienti cosi ottenuti,
  # i più alti, in numero eguale a quello dei consiglieri da eleggere,
  # disponendoli in una graduatoria decrescente. Ciascuna lista o gruppo di
  # liste avrà tanti rappresentanti quanti sono i quozienti ad essa appartenenti
  # compresi nella graduatoria. A parità di quoziente, nelle cifre intere e
  # decimali, il posto è attribuito alla lista o gruppo di liste che ha ottenuto
  # la maggiore cifra elettorale e, a parità di quest'ultima, per sorteggio. Se
  # ad una lista spettano più posti di quanti sono i suoi candidati, i posti
  # eccedenti sono distribuiti, fra le altre liste, secondo l'ordine dei
  # quozienti.
  

  dHondt <- function(
    liste,
    voti,
    num_consiglieri,
    num_candidati = rep(num_consiglieri, length(liste))
  ) {
    quozienti <- data.table::data.table(
      LISTA = rep(liste, each = num_consiglieri),
      VOTI_LISTA = rep(voti, each = num_consiglieri),
      NUM_CANDIDATI = rep(num_candidati, each = num_consiglieri),
      NUM_QUOZIENTE = rep(1:num_consiglieri, length(liste))
    )
    
    quozienti <- quozienti[NUM_QUOZIENTE <= NUM_CANDIDATI]
    
    quozienti[
      ,
      `:=`(
        QUOZIENTE = VOTI_LISTA / NUM_QUOZIENTE,
        SORTEGGIO = runif(.N)
      )
    ]
    
    data.table::setorder(quozienti, -QUOZIENTE, -VOTI_LISTA, SORTEGGIO)
    
    quozienti[
      ,
      ELETTO := .I <= num_consiglieri
    ]
    
    return(
      quozienti[
        ,
        .(ELETTI = sum(ELETTO)),
        by = .(LISTA)
      ]
    )
  }
  
  candidati_sindaci <- candidati_sindaci[
    dHondt(
      CANDIDATO_SINDACO,
      VOTI_LISTA,
      num_consiglieri
    ),
    on = .(CANDIDATO_SINDACO = LISTA)
  ]
  
  # 10. Qualora un candidato alla carica di sindaco sia proclamato eletto al 
  # primo turno, alla lista o al gruppo di liste a lui collegate che non abbia 
  # già conseguito, ai sensi del comma 8, almeno il 60 per cento dei seggi del 
  # consiglio, ma abbia ottenuto almeno il 40 per cento dei voti validi, viene 
  # assegnato il 60 per cento dei seggi, semprechè nessuna altra lista o altra 
  # gruppo di liste collegate abbia superato il 50 per cento dei voti validi.
  # Qualora un candidato alla carica di sindaco sia proclamato eletto al secondo 
  # turno, alla lista o al gruppo di liste ad esso collegate che non abbia già 
  # conseguito, ai sensi del comma 8, almeno il 60 per cento dei seggi del 
  # consiglio, viene assegnato il 60 per cento dei seggi, semprechè nessuna 
  # altra lista o altro gruppo di liste collegate al primo turno abbia già 
  # superato nel turno medesimo il 50 per cento dei voti validi. I restanti 
  # seggi vengono assegnati alle altre liste o gruppi di liste collegate ai 
  # sensi del comma 8.
  if (!ballottaggio) {
    
  }
}
