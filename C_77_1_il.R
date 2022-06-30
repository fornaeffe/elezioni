##### Art. 77 comma 1 lettera i - totali circoscrizione ####

# i) determina il totale dei voti validi della circoscrizione. Tale
# totale e' dato dalla somma delle cifre elettorali circoscrizionali di
# tutte le liste;

totali_circ <- aggregate(
  CIFRA ~ CIRCOSCRIZIONE,
  data = liste_circ,
  sum
)

# l) comunica all'Ufficio centrale nazionale, a mezzo di estratto
# del verbale, la cifra elettorale circoscrizionale di ciascuna lista
# nonche' il totale dei voti validi della circoscrizione)).
