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

##### Art. 83 comma 1 lettera i - riparto circoscrizionale interno alle coalizioni ####
source("C_83_1_i.R")


# Art. 83-bis.
# ((1. L'Ufficio centrale circoscrizionale, ricevute da parte
# dell'Ufficio elettorale centrale nazionale le comunicazioni di cui
#   all'articolo 83, comma 2, procede all'attribuzione nei singoli
#   collegi plurinominali dei seggi spettanti alle liste. A tale fine
#   l'ufficio determina il quoziente elettorale di collegio dividendo la
# somma delle cifre elettorali di collegio di tutte le liste per il
# numero dei seggi da attribuire nel collegio stesso. Nell'effettuare
#   tale divisione non tiene conto dell'eventuale parte frazionaria del
# quoziente. Divide quindi la cifra elettorale di collegio di ciascuna
# lista per tale quoziente di collegio. La parte intera del quoziente
# cosi' ottenuto rappresenta il numero dei seggi da assegnare a
#   ciascuna lista. I seggi che rimangono ancora da attribuire sono
#   assegnati alle liste seguendo la graduatoria decrescente delle parti
#   decimali dei quozienti cosi' ottenuti; in caso di parita', sono
#   attribuiti alle liste con la maggiore cifra elettorale
#   circoscrizionale; a parita' di quest'ultima, si procede a sorteggio.
#   L'Ufficio esclude dall'attribuzione di cui al periodo precedente le
#   liste alle quali e' stato attribuito il numero di seggi ad esse
# assegnato nella circoscrizione secondo la comunicazione di cui
# all'articolo 83, comma 2. Successivamente l'ufficio accerta se il
# numero dei seggi assegnati in tutti i collegi a ciascuna lista
# corrisponda al numero di seggi ad essa attribuito nella
# circoscrizione dall'Ufficio elettorale centrale nazionale. In caso
#   negativo, determina la lista che ha il maggior numero di seggi
#   eccedentari e, a parita' di essi, la lista che tra queste ha ottenuto
# il seggio eccedentario con la minore parte decimale del quoziente;
# sottrae quindi il seggio a tale lista nel collegio in cui e' stato
#   ottenuto con la minore parte decimale dei quozienti di attribuzione e
#   lo assegna alla lista deficitaria che ha il maggior numero di seggi
#   deficitari e, a parita' di essi, alla lista che tra queste ha la
# maggiore parte decimale del quoziente che non ha dato luogo
# all'assegnazione di seggio; il seggio e' assegnato alla lista
# deficitaria nel collegio plurinominale in cui essa ha la maggiore
# parte decimale del quoziente di attribuzione non utilizzata; ripete
# quindi, in successione, tali operazioni sino all'assegnazione di
#   tutti i seggi eccedentari alle liste deficitarie)).
# 2. Di tutte le operazioni dell'Ufficio centrale circoscrizionale
# viene redatto, in duplice esemplare, apposito verbale: un esemplare
# e' rimesso alla Segreteria generale della Camera dei deputati, la
# quale ne rilascia ricevuta; un altro esemplare e' depositato presso
# la cancelleria della Corte di cassazione.
