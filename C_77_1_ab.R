##### Art. 77 comma 1 lettera a - cifra candidati uninominali ####

# Art. 77.
# ((1. L'Ufficio centrale circoscrizionale, compiute le operazioni di
# cui all'articolo 76, facendosi assistere, ove lo ritenga opportuno,
#   da uno o piu' esperti scelti dal presidente:
# a) determina la cifra elettorale individuale di ciascun candidato
# nel collegio uninominale; tale cifra e' data dalla somma dei voti
#   validi conseguiti dal candidato nelle singole sezioni elettorali del
#   collegio uninominale;

candidati_uni <- aggregate(
  VOTI_CANDIDATO ~ 
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE +
    CANDIDATO +
    DATA_NASCITA, 
  data = candidati_comune, 
  sum
)

##### Art. 77 comma 1 lettera b - elezione candidati uninominali ####

# b) proclama eletto in ciascun collegio uninominale il candidato
# che ha ottenuto il maggior numero di voti validi; in caso di parita',
# e' eletto il candidato piu' giovane di eta';

candidati_uni <- candidati_uni[
  order(
    candidati_uni$CIRCOSCRIZIONE, 
    candidati_uni$COLLEGIOPLURINOMINALE, 
    candidati_uni$COLLEGIOUNINOMINALE, 
    candidati_uni$VOTI_CANDIDATO,
    candidati_uni$DATA_NASCITA,
    decreasing = c("FALSE", "FALSE", "FALSE", "TRUE", "TRUE")
  ),
]
candidati_uni$ELETTO <- !duplicated(
  candidati_uni[
    ,
    c(
      "CIRCOSCRIZIONE", 
      "COLLEGIOPLURINOMINALE", 
      "COLLEGIOUNINOMINALE"
    )
  ]
)
