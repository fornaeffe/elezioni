##### Art. 83 comma 1 lettera f - riparto nazionale ####

# f) procede al riparto ((dei seggi assegnati nelle circoscrizioni
# del territorio nazionale, con esclusione del seggio assegnato alla
# circoscrizione Valle d'Aosta)); a tale fine, detrae i ((...)) seggi
#         gia' attribuiti ai candidati proclamati eletti nei collegi
# uninominali ai sensi dell'articolo 77, comma 1, lettera b), e procede
# al riparto dei restanti seggi tra le coalizioni di liste e le singole
# liste di cui alla lettera e) del presente comma in base alla cifra
# elettorale nazionale di ciascuna di esse, fatto salvo quanto previsto
# all'articolo 92, primo comma. A tale fine divide il totale delle
# cifre elettorali nazionali delle coalizioni di liste e delle singole
# liste di cui alla lettera e) del presente comma per il numero dei
# seggi da attribuire, ottenendo cosi' il quoziente elettorale
# nazionale. Nell'effettuare tale divisione non tiene conto
# dell'eventuale parte frazionaria del quoziente. Divide poi la cifra
# elettorale nazionale di ciascuna coalizione di liste o singola lista
# per tale quoziente. La parte intera del quoziente cosi' ottenuto
# rappresenta il numero dei seggi da assegnare a ciascuna coalizione di
# liste o singola lista. I seggi che rimangono ancora da attribuire
# sono rispettivamente assegnati alle coalizioni di liste o singole
# liste per le quali queste ultime divisioni abbiano dato i maggiori
# resti, secondo l'ordine decrescente dei resti medesimi, e, in caso di
# parita' di resti, a quelle che abbiano conseguito la maggiore cifra
# elettorale nazionale; a parita' di quest'ultima si procede a
# sorteggio;

seggi_proporzionale <- dati$camera_seggi - sum(candidati_uni$ELETTO)

cifre_naz$SOGGETTO_RIPARTO <- NA

cifre_naz$SOGGETTO_RIPARTO[which(cifre_naz$SOGLIA_COALIZIONE)] <- 
  as.character(cifre_naz$COALIZIONE[which(cifre_naz$SOGLIA_COALIZIONE)])

cifre_naz$SOGGETTO_RIPARTO[which(cifre_naz$SOGLIA_SOLA)] <- 
  as.character(cifre_naz$LISTA[which(cifre_naz$SOGLIA_SOLA)])

cifre_naz$SOGGETTO_RIPARTO <- as.factor(cifre_naz$SOGGETTO_RIPARTO)

riparto_naz <- aggregate(
  CIFRA ~ SOGGETTO_RIPARTO,
  data = cifre_naz,
  sum,
  subset = SOGLIA1M
)

totale_naz_riparto <- sum(riparto_naz$CIFRA)
quoziente_elettorale_naz <- floor(totale_naz_riparto / seggi_proporzionale)

riparto_naz$PARTE_INTERA <- riparto_naz$CIFRA %/% quoziente_elettorale_naz
riparto_naz$RESTO <- riparto_naz$CIFRA %% quoziente_elettorale_naz

ancora_da_attribuire <- seggi_proporzionale - sum(riparto_naz$PARTE_INTERA)

riparto_naz <- riparto_naz[
  order(riparto_naz$RESTO, riparto_naz$CIFRA, decreasing = TRUE),
]

riparto_naz$ORDINE <- seq_along(riparto_naz$RESTO)

riparto_naz$SEGGIO_DA_RESTI <- riparto_naz$ORDINE <= ancora_da_attribuire

riparto_naz$SEGGI <- riparto_naz$PARTE_INTERA + riparto_naz$SEGGIO_DA_RESTI
