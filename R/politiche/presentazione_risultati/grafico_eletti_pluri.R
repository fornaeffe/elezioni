grafico_eletti_pluri <- function(
    risultato_ramo,
    lista,
    pluri_cod
){
  dati_pluri_lista <- risultato$camera$pluri_liste_sim[
    LISTA == lista & PLURI_COD == pluri_cod
  ]
  
  grafico_eletti(
    factor(dati_pr$NUMERO_MAX),
    dati_pr$PERCENTUALE,
    lista = lista,
    COLORE = risultato_ramo$liste[LISTA == lista]$COLORE
  )
}