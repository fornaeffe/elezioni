##### Art. 83 comma 1 lettera e - soglie di sbarramento ####

# e) individua quindi:
#   1) le coalizioni di liste che abbiano conseguito sul piano
# nazionale almeno il 10 per cento dei voti validi espressi e che
# comprendano almeno una lista collegata che abbia conseguito sul piano
# nazionale almeno il 3 per cento dei voti validi espressi ovvero una
# lista collegata rappresentativa di minoranze linguistiche
# riconosciute, presentata esclusivamente in una regione ad autonomia
# speciale il cui statuto o le relative norme di attuazione prevedano
# una particolare tutela di tali minoranze linguistiche, che abbia
# conseguito almeno il 20 per cento dei voti validi espressi nella
# regione medesima o i cui candidati siano stati proclamati eletti in
# almeno ((un quarto dei collegi uninominali della circoscrizione ai
#          sensi dell'articolo 77, con arrotondamento all'unita' superiore));

cifre_naz_coalizione$CIFRA_PERCENTUALE <- 
  cifre_naz_coalizione$CIFRA / totale_naz * 100

cifre_naz_coalizione$SOGLIA10 <- cifre_naz_coalizione$CIFRA_PERCENTUALE >= 10

cifre_naz$SOGLIA3 <- cifre_naz$CIFRA_PERCENTUALE >= 3

cifre_naz$SOGLIA3M <- cifre_naz$SOGLIA3 | cifre_naz$SOGLIA_MINORANZE

cifre_naz_coalizione <- merge(
  cifre_naz_coalizione,
  aggregate(
    SOGLIA3M ~ COALIZIONE,
    data = cifre_naz,
    function(x) Reduce("|", x)
  )
)

cifre_naz_coalizione$SOGLIA_COALIZIONE <- 
  cifre_naz_coalizione$SOGLIA10 & 
  cifre_naz_coalizione$SOGLIA3M


# 2) le singole liste non collegate, o collegate in coalizioni
# che non abbiano raggiunto la percentuale di cui al numero 1), che
# abbiano conseguito sul piano nazionale almeno il 3 per cento dei voti
# validi espressi, nonche' le singole liste non collegate e le liste
#          collegate in coalizioni che non abbiano raggiunto la percentuale di
#          cui al numero 1), rappresentative di minoranze linguistiche
#         riconosciute, presentate esclusivamente in una regione ad autonomia
#         speciale il cui statuto o le relative norme di attuazione prevedano
#         una particolare tutela di tali minoranze linguistiche, che abbiano
#         conseguito almeno il 20 per cento dei voti validi espressi nella
#         regione medesima o i cui candidati siano stati proclamati eletti in
#         almeno ((un quarto dei collegi uninominali della circoscrizione ai
#                  sensi dell'articolo 77, con arrotondamento all'unita' superiore));

cifre_naz <- merge(
  cifre_naz,
  cifre_naz_coalizione[, c("COALIZIONE", "SOGLIA_COALIZIONE")],
  all.x = TRUE
)

cifre_naz$SOGLIA_SOLA <- 
  (is.na(cifre_naz$COALIZIONE) | !cifre_naz$SOGLIA_COALIZIONE) &
  cifre_naz$SOGLIA3M