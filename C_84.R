##### Art. 84  ####

# Art. 84.
# ((1. Al termine delle operazioni di cui agli articoli precedenti,
#   l'Ufficio centrale circoscrizionale proclama eletti in ciascun
# collegio plurinominale, nei limiti dei seggi ai quali ciascuna lista
# ha diritto, i candidati compresi nella lista del collegio, secondo
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

source("condivisi.R")

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
