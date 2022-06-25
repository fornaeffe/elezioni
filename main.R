load("dati.RData")

##### Art. 77 comma 1 lettera a - cifra candidati uninominali ####
##### Art. 77 comma 1 lettera b - elezione candidati uninominali ####
source("C_77_1_ab.R")

##### Art. 77 comma 1 lettera c - liste cifra uninominale ####
source("C_77_1_c.R")

##### Art. 77 comma 1 lettera d - liste cifra plurinominale ####
##### Art. 77 comma 1 lettera e - liste cifra % uninominale ####
##### Art. 77 comma 1 lettera f - liste cifra circoscrizionale ####
source("C_77_1_def.R")

##### Art. 77 comma 1 lettera g - cifra % candidato uninominale ####
##### Art. 77 comma 1 lettera h - graduatoria candidati uninominale ####
source("C_77_1_gh.R")

##### Art. 77 comma 1 lettera i - totali circoscrizione ####
source("C_77_1_il.R")

##### Art. 83 comma 1 lettera a - cifra naz liste ####
##### Art. 83 comma 1 lettera b - totale naz ####
source("C_83_1_ab.R")

##### Art. 83 comma 1 lettera c - cifra naz coalizioni ####
##### Art. 83 comma 1 lettera d - cifra circoscrizionale coalizioni ####
source("C_83_1_cd.R")

##### Art. 83 comma 1 lettera e - soglie di sbarramento ####
source("C_83_1_e.R")

##### Art. 83 comma 1 lettera f - riparto nazionale ####
source("C_83_1_f.R")

##### Art. 83 comma 1 lettera g - riparto interno alle coalizioni ####
source("C_83_1_g.R")

##### Art. 83 comma 1 lettera h - riparto circoscrizionale ####
source("C_83_1_h.R")

# i) procede quindi all'attribuzione nelle singole circoscrizioni
# dei seggi spettanti alle liste di ciascuna coalizione. A tale fine,
# determina il quoziente circoscrizionale di ciascuna coalizione di
# liste dividendo il totale delle cifre elettorali circoscrizionali
# delle liste ammesse alla ripartizione ai sensi della lettera g),
# primo periodo, per il numero dei seggi assegnati alla coalizione
# nella circoscrizione ai sensi della lettera h). Nell'effettuare la
# divisione di cui al periodo precedente non tiene conto dell'eventuale
# parte frazionaria del quoziente. Divide quindi la cifra elettorale
# circoscrizionale di ciascuna lista della coalizione per tale
# quoziente circoscrizionale. La parte intera del quoziente cosi'
# ottenuto rappresenta il numero dei seggi da assegnare a ciascuna
# lista. I seggi che rimangono ancora da attribuire sono assegnati alle
# liste seguendo la graduatoria decrescente delle parti decimali dei
# quozienti cosi' ottenuti; in caso di parita', sono attribuiti alle
# liste con la maggiore cifra elettorale circoscrizionale; a parita' di
# quest'ultima, si procede a sorteggio. Esclude dall'attribuzione di
# cui al periodo precedente le liste alle quali e' stato attribuito il
# numero di seggi ad esse assegnato a seguito delle operazioni di cui
# alla lettera g). Successivamente l'ufficio accerta se il numero dei
# seggi assegnati in tutte le circoscrizioni a ciascuna lista
# corrisponda al numero dei seggi ad essa attribuito ai sensi della
# lettera g). In caso negativo, procede alle seguenti operazioni,
# iniziando dalla lista che abbia il maggior numero di seggi eccedenti
# e, in caso di parita' di seggi eccedenti da parte di piu' liste, da
# quella che abbia ottenuto la maggiore cifra elettorale nazionale,
# proseguendo poi con le altre liste, in ordine decrescente di seggi
# eccedenti: sottrae i seggi eccedenti alla lista nelle circoscrizioni
# nelle quali essa li ha ottenuti con le parti decimali dei quozienti,
# secondo il loro ordine crescente, e nelle quali inoltre le liste, che
# non abbiano ottenuto il numero di seggi spettante, abbiano parti
# decimali dei quozienti non utilizzate. Conseguentemente, assegna i
# seggi a tali liste. Qualora nella medesima circoscrizione due o piu'
# liste abbiano parti decimali dei quozienti non utilizzate, il seggio
# e' attribuito alla lista con la piu' alta parte decimale del
# quoziente non utilizzata o, in caso di parita', a quella con la
# maggiore cifra elettorale nazionale. Nel caso in cui non sia
# possibile attribuire il seggio eccedentario nella medesima
# circoscrizione, in quanto non vi siano liste deficitarie con parti
# decimali di quozienti non utilizzate, l'Ufficio prosegue, per la
# stessa lista eccedentaria, nell'ordine dei decimali crescenti, a
# individuare un'altra circoscrizione, fino a quando non sia possibile
# sottrarre il seggio eccedentario e attribuirlo ad una lista
# deficitaria nella medesima circoscrizione. Nel caso in cui non sia
# possibile fare riferimento alla medesima circoscrizione ai fini del
# completamento delle operazioni precedenti, fino a concorrenza dei
# seggi ancora da cedere, alla lista eccedentaria vengono sottratti i
# seggi nelle circoscrizioni nelle quali li ha ottenuti con le minori
# parti decimali del quoziente di attribuzione e alle liste deficitarie
# sono conseguentemente attribuiti seggi nelle altre circoscrizioni
# nelle quali abbiano le maggiori parti decimali del quoziente di
# attribuzione non utilizzate.
# 2. L'Ufficio centrale nazionale provvede a comunicare ai singoli
# Uffici centrali circoscrizionali il numero dei seggi assegnati a
# ciascuna lista.
# 3. Di tutte le operazioni dell'Ufficio centrale nazionale viene
# redatto, in duplice esemplare, un apposito verbale: un esemplare e'
# rimesso alla Segreteria generale della Camera dei deputati, la quale
# ne rilascia ricevuta; un altro esemplare e' depositato presso la
# cancelleria della Corte di cassazione.