grafico_eletti_pluri <- function(
    risultato_ramo,
    lista,
    pluri_cod
){
  dati_pluri_lista <- risultato_ramo$pluri_liste_sim[
    LISTA == lista & PLURI_COD == pluri_cod
  ]
  
  grafico_eletti(
    factor(dati_pluri_lista$NUMERO_MAX),
    dati_pluri_lista$PERCENTUALE,
    lista = lista,
    COLORE = risultato_ramo$liste[LISTA == lista]$COLORE
  )
}