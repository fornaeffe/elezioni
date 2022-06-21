

# $camera_voti_candidato_per_comune
# CIRCOSCRIZIONE
# COLLEGIOPLURINOMINALE
# COLLEGIOUNINOMINALE
# COMUNE
# CANDIDATO
# VOTI_CANDIDATO

# Art. 77.
# ((1. L'Ufficio centrale circoscrizionale, compiute le operazioni di
# cui all'articolo 76, facendosi assistere, ove lo ritenga opportuno,
#   da uno o piu' esperti scelti dal presidente:
# a) determina la cifra elettorale individuale di ciascun candidato
# nel collegio uninominale; tale cifra e' data dalla somma dei voti
#   validi conseguiti dal candidato nelle singole sezioni elettorali del
#   collegio uninominale;

cifre_ind <- aggregate(
  VOTI_CANDIDATO ~ 
    CIRCOSCRIZIONE + 
    COLLEGIOPLURINOMINALE + 
    COLLEGIOUNINOMINALE + 
    CANDIDATO + 
    DATA_NASCITA, 
  data = dati$camera_voti_candidato_per_comune, 
  sum
)


# b) proclama eletto in ciascun collegio uninominale il candidato
# che ha ottenuto il maggior numero di voti validi; in caso di parita',
# e' eletto il candidato piu' giovane di eta';

cifre_ind <- cifre_ind[
  order(
    cifre_ind$CIRCOSCRIZIONE, 
    cifre_ind$COLLEGIOPLURINOMINALE, 
    cifre_ind$COLLEGIOUNINOMINALE, 
    cifre_ind$VOTI_CANDIDATO, 
    decreasing = c("FALSE", "FALSE", "FALSE", "TRUE"), 
    method = "radix"
  ),
]
cifre_ind$ELETTO <- !duplicated(
  cifre_ind[
    ,
    c(
      "CIRCOSCRIZIONE", 
      "COLLEGIOPLURINOMINALE", 
      "COLLEGIOUNINOMINALE"
    )
  ]
)

