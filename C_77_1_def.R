##### Art. 77 comma 1 lettera d - liste cifra plurinominale ####

# d) determina la cifra elettorale di collegio plurinominale di
# ciascuna lista. Tale cifra e' data dalla somma delle cifre elettorali
# di collegio uninominale di ciascuna lista;

liste_pluri <- aggregate(
  CIFRA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
  liste_uni,
  sum
)

##### Art. 77 comma 1 lettera e - liste cifra % uninominale ####

# e) determina la cifra elettorale percentuale di collegio
# plurinominale di ciascuna lista. Tale cifra e' data dal quoziente
# risultante dalla divisione della cifra elettorale di collegio
# plurinominale di ciascuna lista per il totale dei voti validi del
# rispettivo collegio plurinominale, moltiplicato per cento;

liste_pluri <- merge(
  liste_pluri,
  aggregate(
    CIFRA ~ COLLEGIOPLURINOMINALE,
    liste_pluri,
    sum
  ),
  by = "COLLEGIOPLURINOMINALE",
  suffixes = c("", "_TOT")
)

liste_pluri$CIFRA_PERCENTUALE <- liste_pluri$CIFRA / liste_pluri$CIFRA_TOT * 100

##### Art. 77 comma 1 lettera f - liste cifra circoscrizionale ####

# f) determina la cifra elettorale circoscrizionale di ciascuna
# lista. Tale cifra e' data dalla somma delle cifre elettorali di
# collegio plurinominale della lista stessa;

liste_circ <- aggregate(CIFRA ~ CIRCOSCRIZIONE + LISTA, liste_pluri, sum)
