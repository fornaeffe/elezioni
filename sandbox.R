liste_uni <- camera$liste_uni[, c(
  "CIRCOSCRIZIONE",
  "COLLEGIOPLURINOMINALE",
  "COLLEGIOUNINOMINALE",
  "CANDIDATO",
  "CAND_MINORANZA",
  "LISTA",
  "MINORANZA",
  "VOTI_LISTA"
)]

liste_naz <- liste_naz[, c(
  "LISTA",
  "COALIZIONE",
  "MINORANZA"
)]

candidati_uni <- camera$candidati_uni[, c(
  "CIRCOSCRIZIONE",
  "COLLEGIOPLURINOMINALE",
  "COLLEGIOUNINOMINALE",
  "CANDIDATO",
  "DATA_NASCITA",
  "VOTI_CANDIDATO"
)]

candidati_pluri <- camera$candidati_pluri[, c(
  "CIRCOSCRIZIONE",
  "COLLEGIOPLURINOMINALE",
  "LISTA",
  "NUMERO",
  "CANDIDATO"
)]

totali_pluri <- camera$collegi_pluri[, c(
  "CIRCOSCRIZIONE",
  "COLLEGIOPLURINOMINALE",
  "SEGGI"
)]

totale_seggi <- 392