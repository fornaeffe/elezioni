tabella_dati <- function(
    risultato
){
  tab <- kableExtra::kbl(
    risultato$liste[,.(COALIZIONE, LISTA, DATA, PERCENTUALE)],
    col.names = c("COALIZIONE", "LISTA", "Ultima elezione", "% su elettori"),
    caption = "Liste",
    row.names = FALSE
  )
  tab <- kableExtra::kable_minimal(tab)
  tab
}

boxplot_percentuali <- function(
    risultato
){
  x <- droplevels(risultato$liste_sim[LISTA != "astensione"])
  par(mar=c(4, 12, 0.1, 0.1))
  
  boxplot(
    PERCENTUALE * 100 ~ LISTA, 
    data = x, 
    horizontal = TRUE, 
    las=1,
    xlab = "Percentuale sui voti validi",
    ylab = NA,
    col = risultato$liste[LISTA != "astensione", COLORE],
    cex.axis = 0.8
  )
  
}