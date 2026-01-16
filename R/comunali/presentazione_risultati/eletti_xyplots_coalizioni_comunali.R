eletti_percentuale_xyplots_coalizioni_comunali <- function(
    risultato
){
  spl <- split(risultato$coalizioni_sim, by = "COALIZIONE")
  
  lapply(
    names(spl),
    function(g) {
      eletti_percentuale_xyplot(
        spl[[g]]$ELETTI + spl[[g]]$SINDACO,
        spl[[g]]$PERCENTUALE_SINDACO,
        risultato$coalizioni[COALIZIONE == g]$COLORE,
        g
      )
    }
  )
  
  invisible()
}