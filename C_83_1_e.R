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

coal_naz$CIFRA_PERCENTUALE <- 
  coal_naz$CIFRA / totale_naz * 100

coal_naz$SOGLIA10 <- coal_naz$CIFRA_PERCENTUALE >= 10

liste_naz$SOGLIA3M <- liste_naz$CIFRA_PERCENTUALE >= 3 | liste_naz$SOGLIA_MINORANZA

coal_naz <- merge(
  coal_naz,
  aggregate(
    SOGLIA3M ~ COALIZIONE,
    data = liste_naz,
    function(x) Reduce("|", x)
  )
)

coal_naz$SOGLIA_COALIZIONE <- 
  coal_naz$SOGLIA10 & 
  coal_naz$SOGLIA3M


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

liste_naz <- merge(
  liste_naz,
  coal_naz[, c("COALIZIONE", "SOGLIA_COALIZIONE")],
  all.x = TRUE
)

liste_naz$SOGLIA_SOLA <- 
  (is.na(liste_naz$COALIZIONE) | !liste_naz$SOGLIA_COALIZIONE) &
  liste_naz$SOGLIA3M
