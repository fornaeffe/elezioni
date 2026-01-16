grafici_eletti_comunali <- function(
    risultato
){
  grafici_eletti(risultato)
  
  coalizioni_sim <- data.table::copy(risultato$coalizioni_sim)
  coalizioni_sim[, PERCENTUALE := PERCENTUALE_SINDACO]
  coalizioni_sim[, ELETTI := factor(
    2 * SINDACO + SEGGIO_CANDIDATO_SINDACO,
    levels = 0:2,
    labels = c("0", "Cons.", "Sind.")
  )]
  
  spl <- split(coalizioni_sim, by = "COALIZIONE")
  
  lapply(
    names(spl),
    function(g) {
      grafico_eletti(
        spl[[g]]$ELETTI,
        spl[[g]]$PERCENTUALE,
        paste("Cand. sindaco", g),
        risultato$coalizioni[COALIZIONE == g]$COLORE
      )
    }
  )
  
  invisible()
}