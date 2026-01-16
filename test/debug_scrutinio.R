# Script per fare il debug della funzione scrutinio_politiche

load("dati/debug_scrutinio.RData")
# In questo workspace la simulazione numero 4 produce i seguenti warning:
# Messaggi di avvertimento:

#   1: In (function (liste_uni, candidati_uni, candidati_pluri, totali_pluri,  :
#   In alcuni collegi il numero di eletti non corrisponde al numero di seggi al
#   termine dei subentri. Ramo: cameraSIM: 4

#   2: In (function (liste_uni, candidati_uni, candidati_pluri, totali_pluri,  :
#   In alcuni collegi il numero di eletti non corrisponde al numero di seggi al
#   termine dei subentri. Ramo: senatoSIM: 4

ramo <- "camera"

uni_liste_sim <- data.table::copy(voti[[ramo]]$uni_liste_sim)
candidati_uni_sim <- data.table::copy(voti[[ramo]]$candidati_uni_sim)
candidati_pluri_sim <- data.table::copy(candidati[[ramo]]$candidati_pluri_sim)
pluri <- data.table::copy(dati_collegi[[ramo]]$pluri)
liste <- data.table::copy(parametri_input$liste)
candidati_pluri <- data.table::copy(dati_candidati[[ramo]]$candidati_pluri)

totale_seggi <- ifelse(ramo == "camera", 392, 196)

# Verifico quali liste rappresentanti di minoranze linguistiche si sono 
# presentate solo in una regione
if (ramo == "camera") {
  candidati_pluri[
    ,
    REG_COD := substr(CIRC_COD, 1, nchar(CIRC_COD) - 2)
  ]
} else {
  candidati_pluri[
    ,
    REG_COD := CIRC_COD
  ]
}

liste_minoranza <- candidati_pluri[
  ,
  .(
    REGIONI = length(unique(REG_COD)),
    MINORANZA = sum(MINORANZA)
  ),
  by = .(LISTA)
][REGIONI == 1 & MINORANZA > 0, LISTA]

liste[
  ,
  MINORANZA := LISTA %in% liste_minoranza
]

# Torno ai data.frame
uni_liste_sim_dt <- as.data.frame(uni_liste_sim[, .(
  CIRCOSCRIZIONE = CIRC_COD,
  COLLEGIOPLURINOMINALE = PLURI_COD,
  COLLEGIOUNINOMINALE = UNI_COD,
  CANDIDATO = CANDIDATO_ID,
  CAND_MINORANZA,
  LISTA,
  MINORANZA,
  VOTI_LISTA = VOTI_LISTA_SIM,
  SIM
)])
candidati_uni_sim_dt <- as.data.frame(candidati_uni_sim[, .(
  CIRCOSCRIZIONE = CIRC_COD,
  COLLEGIOPLURINOMINALE = PLURI_COD,
  COLLEGIOUNINOMINALE = UNI_COD,
  CANDIDATO = CANDIDATO_ID,
  DATA_NASCITA,
  VOTI_CANDIDATO,
  SIM
)])
candidati_pluri_sim_dt <- as.data.frame(candidati_pluri_sim[, .(
  CIRCOSCRIZIONE = CIRC_COD,
  COLLEGIOPLURINOMINALE = PLURI_COD,
  LISTA,
  NUMERO = NUMERO_CANDIDATO,
  CANDIDATO = CANDIDATO_ID,
  SIM
)])
pluri_dt <- as.data.frame(pluri[, .(
  CIRCOSCRIZIONE = CIRC_COD,
  COLLEGIOPLURINOMINALE = PLURI_COD,
  SEGGI = SEGGI_PLURI
)])
liste_dt <- as.data.frame(liste[, .(
  LISTA,
  COALIZIONE,
  MINORANZA
)])

uni_liste_sim_split <- split(uni_liste_sim_dt, uni_liste_sim$SIM)
candidati_uni_sim_split <- split(candidati_uni_sim_dt, candidati_uni_sim$SIM)
candidati_pluri_sim_split <- split(candidati_pluri_sim_dt, candidati_pluri_sim$SIM)

liste_uni <- uni_liste_sim_split[[4]]
candidati_uni <- candidati_uni_sim_split[[4]]
candidati_pluri <- candidati_pluri_sim_split[[4]]
totali_pluri <- pluri_dt
liste_naz <- liste_dt

# Da qui posso continuare dentro a scrutinio_politiche

