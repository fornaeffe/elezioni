##### Art. 83 comma 1 lettera a - cifra naz liste ####

# Art. 83.
# 1. L'Ufficio centrale nazionale, ricevuti gli estratti dei verbali
# da tutti gli Uffici centrali circoscrizionali, facendosi assistere,
# ove lo ritenga opportuno, da uno o piu' esperti scelti dal
# presidente:
#   a) determina la cifra elettorale nazionale di ciascuna lista.
# Tale cifra e' data dalla somma delle cifre elettorali
# circoscrizionali conseguite nelle singole circoscrizioni dalle liste
# aventi il medesimo contrassegno;

liste_naz <- aggregate(
  CIFRA ~ LISTA,
  liste_circ,
  sum
)

##### Art. 83 comma 1 lettera b - totale naz ####

# b) determina il totale nazionale dei voti validi. Esso e' dato
# dalla somma delle cifre elettorali circoscrizionali di tutte le
# liste;
totale_naz <- sum(liste_naz$CIFRA)
