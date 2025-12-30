#' Scrutinio delle elezioni comunali
#' 
#' @param liste le liste che si presentano alle elezioni, un data.table con le
#' seguenti colonne:
#' \describe{
#'  \item{LISTA}{string, nome della lista}
#'  \item{CANDIDATO_SINDACO}{string, cognome e nome del candidato sindaco 
#'  collegato alla lista}
#'  \item{VOTI_LISTA}{number, voti ricevuti dalla lista}
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
#' @param candidati i candidati consiglieri che si presentano alle elezioni, un
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
}
