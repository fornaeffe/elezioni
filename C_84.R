##### Art. 84  ####

# Art. 84.
# ((1. Al termine delle operazioni di cui agli articoli precedenti,
#   l'Ufficio centrale circoscrizionale proclama eletti in ciascun
# collegio plurinominale, nei limiti dei seggi ai quali ciascuna lista
# ha diritto, i candidati compresi nella lista del collegio, secondo
# l'ordine di presentazione.

candidati_pluri <- dati$candidati_pluri

candidati_pluri <- candidati_pluri[
  !(candidati_pluri$CANDIDATO %in% candidati_uni$CANDIDATO[candidati_uni$ELETTO]),
]

ammesse_pluri$SEGGI <- ammesse_pluri$SEGGI_FLIPPER

ammesse_pluri$DECIMALI_USATI <-
  ammesse_pluri$SEGGIO_DA_RESTI + 
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

subentro <- function(livello = "circ", uni = FALSE, coal = FALSE) {
  donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)
  for (i in donatori) {
    cat(
      "Devo spostare ",
      ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
      " seggi della lista ",
      as.character(ammesse_pluri$LISTA[i]),
      " da ",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
      ".\n",
      "Considero i candidati ",
      ifelse(uni, "uninominali", "plurinominali"),
      " della stessa ",
      ifelse(coal, "coalizione", "lista"),
      " a livello ",
      switch (livello,
              pluri = "di collegio plurinominale",
              circ = "di circoscrizione",
              naz = "nazionale"
      ),
      ".\n",
      sep = ""
    )
    
    if (livello == "naz") {
      cerca_naz(i, FALSE, coal, uni)
      if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) next
      
      cerca_naz(i, TRUE, coal, uni)
      next
    }
    
    if (uni) {
      cerca_accettori_uni(i, pluri = livello == "pluri")
      next
    }
    
    cerca_accettori(i)
  }
}

cerca_accettori <- function(
  i,
  circ = ammesse_pluri$CIRCOSCRIZIONE[i],
  coal = FALSE,
  pluri = FALSE
) {
  if (coal) {
    liste <- ammesse_pluri$LISTA %in% dati$camera_coalizioni$LISTA[
      dati$camera_coalizioni$COALIZIONE == dati$camera_coalizioni$COALIZIONE[
        dati$camera_coalizioni$LISTA == ammesse_pluri$LISTA[i]
      ]
    ]
  } else {
    liste <- ammesse_pluri$LISTA == ammesse_pluri$LISTA[i]
  }
  
  if (pluri) {
    ambito <- 
      ammesse_pluri$COLLEGIOPLURINOMINALE == 
      ammesse_pluri$COLLEGIOPLURINOMINALE[i]
  } else {
    ambito <- ammesse_pluri$CIRCOSCRIZIONE == circ
  }
  
  accettori <- which(
    ambito & liste & ammesse_pluri$ELETTI < ammesse_pluri$CANDIDATI
  )
  
  accettori <- accettori[order(
    ammesse_pluri$DECIMALI_USATI[accettori],
    ammesse_pluri$RESTO[accettori],
    decreasing = c(FALSE, TRUE)
  )]
  
  for (j in accettori) {
    
    ammesse_pluri$SEGGI[i] <<- ammesse_pluri$SEGGI[i] - 1
    ammesse_pluri$SEGGI[j] <<- ammesse_pluri$SEGGI[j] + 1
    
    ammesse_pluri$DECIMALI_USATI[j] <<- TRUE
    
    ammesse_pluri$ELETTI[c(i,j)] <<- 
      pmin(ammesse_pluri$SEGGI[c(i,j)], ammesse_pluri$CANDIDATI[c(i,j)])
    
    cat(
      "Ho spostato un seggio dalla lista",
      as.character(ammesse_pluri$LISTA[i]),
      "nel collegio",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
      "alla lista",
      as.character(ammesse_pluri$LISTA[j]),
      "nel collegio",
      as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[j]),
      "\n"
    )
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}

cerca_accettori_uni <- function(
    i,
    circ = ammesse_pluri$CIRCOSCRIZIONE[i],
    pluri = FALSE
){
  if (pluri) {
    ambito <- 
      candidati_uni$COLLEGIOPLURINOMINALE == 
      ammesse_pluri$COLLEGIOPLURINOMINALE[i]
  } else {
    ambito <- candidati_uni$CIRCOSCRIZIONE == circ
  }
  
  accettori <- which(
    ambito &
      candidati_uni$ELETTO == FALSE &
      candidati_uni$CANDIDATO %in% liste_uni$CANDIDATO[
        liste_uni$LISTA == ammesse_pluri$LISTA[i]
      ]
  )
  
  accettori <- accettori[order(
    candidati_uni$CIFRA_PERCENTUALE[accettori],
    candidati_uni$DATA_NASCITA[accettori],
    decreasing = c(TRUE, TRUE)
  )]
  
  for (j in accettori) {
    candidati_pluri <<- rbind(
      candidati_pluri,
      data.frame(
        LISTA = ammesse_pluri$LISTA[i],
        CIRCOSCRIZIONE = ammesse_pluri$CIRCOSCRIZIONE[i],
        COLLEGIOPLURINOMINALE = ammesse_pluri$COLLEGIOPLURINOMINALE[i],
        NUMERO = 1 + max(candidati_pluri$NUMERO[
          candidati_pluri$LISTA == ammesse_pluri$LISTA[i] &
            candidati_pluri$COLLEGIOPLURINOMINALE == 
            ammesse_pluri$COLLEGIOPLURINOMINALE[i]
        ]),
        CANDIDATO = candidati_uni$CANDIDATO[j]
      )
    )
    
    ammesse_pluri$CANDIDATI[i] <<- ammesse_pluri$CANDIDATI[i] + 1
    ammesse_pluri$ELETTI[i] <<- ammesse_pluri$ELETTI[i] + 1
    
    cat(
      "Ho ripescato",
      as.character(candidati_uni$CANDIDATO[accettori[j]]),
      "\n"
    )
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
  
}

cerca_naz <- function(
    i,
    decimali_usati = FALSE,
    coal = FALSE,
    uni = FALSE
) {
  if (decimali_usati) {
    decimali <- ammesse_pluri$DECIMALI_USATI
  } else {
    decimali <- !ammesse_pluri$DECIMALI_USATI
  }
  
  if (coal) {
    liste <- ammesse_pluri$LISTA %in% dati$camera_coalizioni$LISTA[
      dati$camera_coalizioni$COALIZIONE == dati$camera_coalizioni$COALIZIONE[
        dati$camera_coalizioni$LISTA == ammesse_pluri$LISTA[i]
      ]
    ]
  } else {
    liste <- ammesse_pluri$LISTA == ammesse_pluri$LISTA[i]
  }
  
  circ_accettrici <- aggregate(
    RESTO ~ CIRCOSCRIZIONE,
    ammesse_pluri[liste & decimali, ],
    max
  )
  
  circ_accettrici <- circ_accettrici[order(
    circ_accettrici$RESTO,
    decreasing = TRUE
  ),]
  
  for (k in seq_along(circ_accettrici$CIRCOSCRIZIONE)) {
    
    if (uni) {
      cerca_accettori_uni(i, circ_accettrici$CIRCOSCRIZIONE[k])
    } else {
      cerca_accettori(i, circ_accettrici$CIRCOSCRIZIONE[k], coal)
    }
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
}

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
