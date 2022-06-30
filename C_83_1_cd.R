##### Art. 83 comma 1 lettera c - cifra naz coalizioni ####

# c) determina la cifra elettorale nazionale di ciascuna coalizione
# di liste. Tale cifra e' data dalla somma delle cifre elettorali
# nazionali delle liste collegate in coalizione. Non concorrono alla
# determinazione della cifra elettorale nazionale di coalizione i voti
# espressi a favore delle liste collegate che abbiano conseguito sul
# piano nazionale un numero di voti validi inferiore all'1 per cento
# del totale, fatto salvo, per le liste rappresentative di minoranze
# linguistiche riconosciute, quanto previsto alla lettera e);

cifre_naz$CIFRA_PERCENTUALE <- cifre_naz$CIFRA / totale_naz * 100

cifre_naz$SOGLIA1 <- cifre_naz$CIFRA_PERCENTUALE >= 1

cifre_circ <- merge(
  cifre_circ,
  totali_circ,
  by = "CIRCOSCRIZIONE",
  suffixes = c("","_TOT")
)

cifre_circ$CIFRA_PERCENTUALE <- cifre_circ$CIFRA / cifre_circ$CIFRA_TOT * 100

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

cifre_naz <- merge(
  cifre_naz,
  dati$camera_coalizioni
)

cifre_naz$SOGLIA_MINORANZE <- 
  cifre_naz$MINORANZE &
  (
    cifre_naz$LISTA %in% cifre_circ$LISTA[cifre_circ$CIFRA_PERCENTUALE > 20] |
      cifre_naz$LISTA %in% eletti_minoranze_circ$LISTA[
        eletti_minoranze_circ$ELETTO >= 
          ceiling(eletti_minoranze_circ$COLLEGI / 4)
      ]
  )

cifre_naz$SOGLIA1M <- cifre_naz$SOGLIA1 | cifre_naz$SOGLIA_MINORANZE

cifre_naz_coalizione <- aggregate(
  CIFRA ~ COALIZIONE,
  data = cifre_naz,
  sum,
  subset = SOGLIA1M
)

##### Art. 83 comma 1 lettera d - cifra circoscrizionale coalizioni ####

# d) determina la cifra elettorale circoscrizionale di ciascuna
# coalizione di liste. Tale cifra e' data dalla somma delle cifre
# elettorali circoscrizionali delle liste collegate tra loro in
# coalizione, individuate ai sensi dell'ultimo periodo della lettera
# c);

cifre_circ <- merge(
  cifre_circ,
  cifre_naz[, c("COALIZIONE", "LISTA", "SOGLIA1M")]
)

cifre_circ_coalizione <- aggregate(
  CIFRA ~ CIRCOSCRIZIONE + COALIZIONE,
  data = cifre_circ,
  sum,
  subset = SOGLIA1M
)
