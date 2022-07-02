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


liste_naz$SOGLIA_MINORANZA <- 
  liste_naz$MINORANZA &
    liste_naz$LISTA %in% liste_circ$LISTA[
      liste_circ$CIFRA_PERCENTUALE >= 20 |
        liste_circ$ELETTI_MINORANZA >= ceiling(liste_circ$COLLEGI_UNI / 4)
    ]

liste_naz$SOGLIA1M <- liste_naz$SOGLIA1 | liste_naz$SOGLIA_MINORANZA

#### Sono arrivato fino a qui

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
