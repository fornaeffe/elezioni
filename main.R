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

##### Art. 83 bis - riparto plurinominali ####
source("C_83bis.R")

##### Art. 84 - subentro ####
source("C_84.R")

##### Art. 85 e 86 - risoluzione pluricandidature ####
source("C_85.R")

liste <- dati$camera_coalizioni


liste$COAL_O_LISTA <- factor(ifelse(
  is.na(liste$COALIZIONE),
  as.character(liste$LISTA),
  as.character(liste$COALIZIONE)
))

liste_pluri <- merge(
  cifre_pluri[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "LISTA"
  )],
  ammesse_pluri[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "LISTA",
    "ELETTI"
  )],
  all.x = TRUE
)
liste_pluri$ELETTI[is.na(liste_pluri$ELETTI)] <- 0

liste_pluri <- merge(
  liste_pluri,
  liste[,c("LISTA", "COAL_O_LISTA")]
)

liste_uni <- unique(
  dati$camera_voti_lista_per_comune[,c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "LISTA"
  )]
)

cifre_uni <- merge(
  cifre_uni,
  liste[,c(
    "LISTA",
    "COAL_O_LISTA"
  )]
)


candidati_uni <- merge(
  candidati_uni[,c(
    "CIRCOSCRIZIONE",
    "COLLEGIOUNINOMINALE",
    "COLLEGIOPLURINOMINALE",
    "CANDIDATO",
    "ELETTO"
  )],
  unique(cifre_uni[,c(
    "COLLEGIOUNINOMINALE",
    "CANDIDATO",
    "COAL_O_LISTA"
  )])
)

candidati <- data.frame(CANDIDATO = levels(candidati_uni$CANDIDATO))

candidati$ELETTO <- 
  candidati$CANDIDATO %in% candidati_uni$CANDIDATO[candidati_uni$ELETTO] |
  candidati$CANDIDATO %in% candidati_pluri$CANDIDATO[candidati_pluri$ELETTO]

candidati <- merge(
  candidati,
  candidati_pluri[candidati_pluri$ELETTO,c("CANDIDATO", "COLLEGIOPLURINOMINALE")],
  all.x = TRUE
)

candidati <- merge(
  candidati,
  candidati_uni[candidati_uni$ELETTO,c("CANDIDATO", "COLLEGIOUNINOMINALE")],
  all.x = TRUE
)
