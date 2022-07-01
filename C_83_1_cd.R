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

liste_uni <- merge(
  liste_uni,
  candidati_uni[
    ,
    c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "ELETTO"
    )
  ]
)

liste_uni$MINORANZA <- FALSE

liste_uni$MINORANZA[
  liste_uni$LISTA %in% 
    dati$camera_coalizioni$LISTA[dati$camera_coalizioni$MINORANZE] &
    !(
      liste_uni$CANDIDATO %in% liste_uni$CANDIDATO[
        duplicated(
          liste_uni[
            ,
            c(
              "CIRCOSCRIZIONE", 
              "COLLEGIOPLURINOMINALE", 
              "COLLEGIOUNINOMINALE",
              "CANDIDATO"
            )
          ]
        )
      ]
    )
] <- TRUE

eletti_minoranze_circ <- aggregate(
  ELETTO ~
    CIRCOSCRIZIONE +
    LISTA,
  data = liste_uni[liste_uni$MINORANZA,],
  sum
)

numero_collegi <- as.data.frame(
  table(totali_uni$CIRCOSCRIZIONE), 
  responseName = "COLLEGI"
)

names(numero_collegi)[1] <- "CIRCOSCRIZIONE"

eletti_minoranze_circ <- merge(
  eletti_minoranze_circ,
  numero_collegi
)

liste_naz <- merge(
  liste_naz,
  dati$camera_coalizioni
)

liste_naz$SOGLIA_MINORANZE <- 
  liste_naz$MINORANZE &
  (
    liste_naz$LISTA %in% liste_circ$LISTA[liste_circ$CIFRA_PERCENTUALE > 20] |
      liste_naz$LISTA %in% eletti_minoranze_circ$LISTA[
        eletti_minoranze_circ$ELETTO >= 
          ceiling(eletti_minoranze_circ$COLLEGI / 4)
      ]
  )

liste_naz$SOGLIA1M <- liste_naz$SOGLIA1 | liste_naz$SOGLIA_MINORANZE

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

liste_circ <- merge(
  liste_circ,
  liste_naz[, c("COALIZIONE", "LISTA", "SOGLIA1M")]
)

liste_circ_coalizione <- aggregate(
  CIFRA ~ CIRCOSCRIZIONE + COALIZIONE,
  data = liste_circ,
  sum,
  subset = SOGLIA1M
)
