##### Art. 83 comma 1 lettera g - riparto interno alle coalizioni ####

# g) procede, per ciascuna coalizione di liste, al riparto dei
# seggi fra le liste collegate che abbiano conseguito sul piano
# nazionale almeno il 3 per cento dei voti validi espressi nonche' fra
# le liste collegate rappresentative di minoranze linguistiche
# riconosciute, presentate esclusivamente in una regione ad autonomia
# speciale il cui statuto o le relative norme di attuazione prevedano
# una particolare tutela di tali minoranze linguistiche, che abbiano
# conseguito almeno il 20 per cento dei voti validi espressi nella
# regione medesima o i cui candidati siano stati proclamati eletti in
# almeno ((un quarto dei collegi uninominali della circoscrizione ai
#          sensi dell'articolo 77, con arrotondamento all'unita' superiore)). A
# tale fine, divide la somma delle cifre elettorali delle liste ammesse
# al riparto per il numero di seggi gia' individuato ai sensi della
#          lettera f) del presente comma. Nell'effettuare tale divisione non
# tiene conto dell'eventuale parte frazionaria del quoziente cosi'
# ottenuto.

ammesse_naz <- liste_naz[
  liste_naz$SOGLIA3M,
  c(
    "SOGGETTO_RIPARTO",
    "LISTA",
    "CIFRA"
  )
]

riparto_naz <- merge(
  riparto_naz,
  aggregate(
    CIFRA ~ SOGGETTO_RIPARTO,
    data = ammesse_naz,
    sum
  ),
  by = "SOGGETTO_RIPARTO",
  suffixes = c("","_AMMESSE_AL_RIPARTO")
)

riparto_naz$QUOZIENTE <- 
  riparto_naz$CIFRA_AMMESSE_AL_RIPARTO %/% riparto_naz$SEGGI


# Divide poi la cifra elettorale nazionale di ciascuna lista
# ammessa al riparto per tale quoziente. La parte intera del quoziente
# cosi' ottenuto rappresenta il numero dei seggi da assegnare a
#         ciascuna lista. I seggi che rimangono ancora da attribuire sono
#         rispettivamente assegnati alle liste per le quali queste ultime
#         divisioni abbiano dato i maggiori resti e, in caso di parita' di
# resti, alle liste che abbiano conseguito la maggiore cifra elettorale
# nazionale; a parita' di quest'ultima si procede a sorteggio;

ammesse_naz <- merge(
  ammesse_naz,
  riparto_naz[, c("SOGGETTO_RIPARTO", "QUOZIENTE")]
)

ammesse_naz$PARTE_INTERA <- ammesse_naz$CIFRA %/% ammesse_naz$QUOZIENTE
ammesse_naz$RESTO <- ammesse_naz$CIFRA %% ammesse_naz$QUOZIENTE

riparto_naz <- merge(
  riparto_naz,
  aggregate(
    PARTE_INTERA ~ SOGGETTO_RIPARTO,
    data = ammesse_naz,
    sum
  ),
  by = "SOGGETTO_RIPARTO",
  suffixes = c("", "_TOT")
)

riparto_naz$DA_ASSEGNARE <- riparto_naz$SEGGI - riparto_naz$PARTE_INTERA_TOT

ammesse_naz <- merge(
  ammesse_naz,
  riparto_naz[, c("SOGGETTO_RIPARTO", "DA_ASSEGNARE")]
)

ammesse_naz <- ammesse_naz[
  order(
    ammesse_naz$SOGGETTO_RIPARTO,
    ammesse_naz$RESTO,
    ammesse_naz$CIFRA,
    decreasing = c(FALSE, TRUE, TRUE),
    method = "radix"
  ),
]

ammesse_naz$ORDINE <- ave(
  seq_along(ammesse_naz$SOGGETTO_RIPARTO),
  ammesse_naz$SOGGETTO_RIPARTO,
  FUN = seq_along
)

ammesse_naz$SEGGIO_DA_RESTI <- ammesse_naz$ORDINE <= ammesse_naz$DA_ASSEGNARE

ammesse_naz$SEGGI <- ammesse_naz$PARTE_INTERA + ammesse_naz$SEGGIO_DA_RESTI
