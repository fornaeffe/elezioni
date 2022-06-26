##### Art. 84 - nomine pluri ####

# Art. 84.
# ((1. Al termine delle operazioni di cui agli articoli precedenti,
#   l'Ufficio centrale circoscrizionale proclama eletti in ciascun
# collegio plurinominale, nei limiti dei seggi ai quali ciascuna lista
# ha diritto, i candidati compresi nella lista del collegio, secondo
# l'ordine di presentazione.

candidati_pluri <- dati$camera_candidati_pluri

candidati_pluri <- merge(
  candidati_pluri,
  ammesse_pluri[,c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "LISTA",
    "SEGGI_FLIPPER"
  )]
)

candidati_pluri$SEGGI_FLIPPER[is.na(candidati_pluri$SEGGI_FLIPPER)] <- 0

candidati_pluri$ELETTO <- 
  candidati_pluri$NUMERO <= candidati_pluri$SEGGI_FLIPPER

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

ammesse_pluri <- merge(
  ammesse_pluri,
  aggregate(
    ELETTO ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
    candidati_pluri,
    sum
  )
)

names(ammesse_pluri)[names(ammesse_pluri) == "ELETTO"] <- "ELETTI"

ammesse_pluri$DECIMALI_USATI <- 
  ammesse_pluri$SEGGIO_DA_RESTI + 
  ammesse_pluri$RICEVUTO - 
  ammesse_pluri$CEDUTO > 0

ammesse_pluri$DA_ELEGGERE <- ammesse_pluri$SEGGI_FLIPPER - ammesse_pluri$ELETTI

ammesse_circ <- merge(
  ammesse_circ,
  aggregate(
    DA_ELEGGERE ~ CIRCOSCRIZIONE + LISTA,
    ammesse_pluri,
    sum
  )
)

ammesse_pluri <- merge(
  ammesse_pluri,
  ammesse_circ[,c("CIRCOSCRIZIONE", "LISTA", "DA_ELEGGERE")],
  by = c("CIRCOSCRIZIONE", "LISTA"),
  suffixes = c("", "_CIRC")
)

candidati_pluri <- merge(
  candidati_pluri,
  ammesse_pluri[
    ,
    c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA",
      "DA_ELEGGERE_CIRC",
      "DECIMALI_USATI",
      "RESTO"
    )
  ]
)

candidati_pluri <- candidati_pluri[order(
  candidati_pluri$CIRCOSCRIZIONE,
  candidati_pluri$LISTA,
  candidati_pluri$ELETTO,
  candidati_pluri$DECIMALI_USATI,
  candidati_pluri$RESTO,
  candidati_pluri$NUMERO,
  decreasing = c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
),]

candidati_pluri$ORDINE[!candidati_pluri$ELETTO] <- ave(
  seq_along(candidati_pluri$LISTA[!candidati_pluri$ELETTO]),
  paste(
    candidati_pluri$CIRCOSCRIZIONE,
    candidati_pluri$LISTA
  )[!candidati_pluri$ELETTO],
  FUN = seq_along
)

candidati_pluri$ELETTO[which(
  candidati_pluri$ORDINE <= candidati_pluri$DA_ELEGGERE
)] <- TRUE

#   3. Qualora al termine delle operazioni di cui al comma 2 residuino
#   ancora seggi da assegnare ad una lista, questi sono attribuiti,
#   nell'ambito del collegio plurinominale originario, ai candidati della
# lista nei collegi uninominali non proclamati eletti secondo la
# graduatoria di cui all'articolo 77, comma 1, lettera h). Qualora
#   residuino ancora seggi da assegnare alla lista, questi sono
#   attribuiti ai candidati della lista nei collegi uninominali non
#   proclamati eletti nell'ambito della circoscrizione, secondo la
# graduatoria di cui all'articolo 77, comma 1, lettera h).
# 4. Qualora al termine delle operazioni di cui al comma 3 residuino
# ancora seggi da assegnare alla lista, l'Ufficio centrale nazionale,
# previa apposita comunicazione dell'Ufficio centrale circoscrizionale,
# individua la circoscrizione in cui la lista abbia la maggiore parte
# decimale del quoziente non utilizzata e procede a sua volta ad
# apposita comunicazione all'Ufficio centrale circoscrizionale
# competente. L'Ufficio centrale circoscrizionale provvede
# all'assegnazione dei seggi ai sensi del comma 2. Qualora al termine
# delle operazioni di cui ai precedenti periodi residuino ancora seggi
# da assegnare alla lista, questi le sono attribuiti nelle altre
# circoscrizioni in cui la stessa lista abbia la maggiore parte
# decimale del quoziente gia' utilizzata, procedendo secondo l'ordine
# decrescente.
# 5. Qualora al termine delle operazioni di cui al comma 4 residuino
# ancora seggi da assegnare ad una lista in un collegio plurinominale,
# questi sono attribuiti, nell'ambito del collegio plurinominale
# originario, alla lista facente parte della medesima coalizione della
# lista deficitaria che abbia la maggiore parte decimale del quoziente
# non utilizzata, procedendo secondo l'ordine decrescente; esaurite le
# liste con la parte decimale del quoziente non utilizzata, si procede
# con le liste facenti parte della medesima coalizione, sulla base
# delle parti decimali del quoziente gia' utilizzate, secondo l'ordine
# decrescente. Qualora al termine delle operazioni di cui al primo
# periodo residuino ancora seggi da assegnare alla lista, questi sono
# attribuiti alle liste facenti parte della medesima coalizione negli
# altri collegi plurinominali della circoscrizione, partendo da quello
# in cui la coalizione abbia la maggiore parte decimale del quoziente
# non utilizzata e procedendo secondo quanto previsto dal primo
# periodo; si procede successivamente nei collegi plurinominali in cui
# la coalizione abbia la maggiore parte decimale del quoziente gia'
# utilizzata, secondo l'ordine decrescente.
# 6. Qualora al termine delle operazioni di cui al comma 5 residuino
# ancora seggi da assegnare ad una lista, questi sono attribuiti ai
# candidati della lista nei collegi uninominali non proclamati eletti
# nelle altre circoscrizioni, secondo la graduatoria di cui
# all'articolo 77, comma 1, lettera h). A tale fine si procede con le
# modalita' previste dal comma 4.
# 7. Qualora al termine delle operazioni di cui al comma 6 residuino
# ancora seggi da assegnare ad una lista, questi sono attribuiti alle
# liste facenti parte della medesima coalizione della lista deficitaria
# nelle altre circoscrizioni. A tale fine si procede con le modalita'
# previste dai commi 4 e 5.
# 8. Nell'effettuare le operazioni di cui ai precedenti commi, in
# caso di parita' della parte decimale del quoziente, si procede
# mediante sorteggio.
# 9. Dell'avvenuta proclamazione effettuata ai sensi del presente
# articolo il presidente dell'Ufficio centrale circoscrizionale invia
# attestato ai deputati proclamati e ne da' immediata notizia alla
# segreteria generale della Camera dei deputati nonche' alle singole
# prefetture-uffici territoriali del Governo, che la portano a
# conoscenza del pubblico)).