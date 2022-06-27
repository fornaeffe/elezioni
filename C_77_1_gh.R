##### Art. 77 comma 1 lettera g - cifra % candidato uninominale ####

# g) determina la cifra elettorale percentuale di ciascun candidato
# nel collegio uninominale. Tale cifra e' data dal quoziente risultante
# dalla divisione della cifra elettorale individuale di ciascun
# candidato per il totale dei voti validi del rispettivo collegio
# uninominale, moltiplicato per cento;

totali_uni <- aggregate(
  VOTI_CANDIDATO ~
    CIRCOSCRIZIONE +
    COLLEGIOPLURINOMINALE +
    COLLEGIOUNINOMINALE,
  data = cifre_ind,
  sum
)

cifre_ind <- merge(
  cifre_ind,
  totali_uni,
  by = c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE"
  ),
  suffixes = c("", "_TOT")
)

cifre_ind$CIFRA_PERCENTUALE <- 
  cifre_ind$VOTI_CANDIDATO / cifre_ind$VOTI_CANDIDATO_TOT * 100

##### Art. 77 comma 1 lettera h - graduatoria candidati uninominale ####

# h) determina, per ciascuna lista, la graduatoria dei candidati
# nei collegi uninominali della circoscrizione non proclamati eletti,
# disponendoli nell'ordine delle rispettive cifre elettorali
# individuali percentuali. A parita' di cifre individuali percentuali,
# prevale il piu' giovane di eta'. In caso di collegamento dei
# candidati con piu' liste, i candidati entrano a far parte della
# graduatoria relativa a ciascuna delle liste con cui e' stato
# dichiarato il collegamento;

# Operazioni non svolte qui, ma nell'art. 84 comma 3