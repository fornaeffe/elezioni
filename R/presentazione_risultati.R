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

coalizioni_vincenti <- function(
    risultato
){
  vittoria <- risultato$coalizioni_sim[
    ,
    .(SINDACO = mean(SINDACO)),
    by = .(COALIZIONE)
  ]
  
  vittoria$SINDACO <- formattable::percent(vittoria$SINDACO, 0)
  
  
  tbl <- kableExtra::kbl(
    vittoria,
    col.names = c("CANDIDATO SINDACO", "Prob."),
    caption = "Probabilità di vittoria",
    row.names = FALSE
  )
  
  tbl <- kableExtra::kable_minimal(tbl)
  
  tbl
}

plot_consiglio_comunale <- function(
    risultato
){
  liste <- risultato$liste[LISTA != "astensione"]
  
  coalizioni <- risultato$coalizioni[
    risultato$liste[
      LISTA != "astensione",
      .(
        VOTI_SINDACO = sum(VOTI_LISTA),
        VOTI_BALLOTTAGGIO = sum(VOTI_LISTA)
      ),
      by = .(COALIZIONE)
    ],
    on = .(COALIZIONE)
  ]
  
  scrutinio <- scrutinio_comunali(
    liste,
    coalizioni,
    risultato$pop_legale,
    risultato$num_consiglieri
  )
  
  liste <- liste[, .(LISTA, COLORE)][
    scrutinio$liste[
      ,
      Gruppo := LISTA
    ],
    on = .(LISTA)
  ]
  
  coalizioni <- coalizioni[, .(COALIZIONE, COLORE)][
    scrutinio$coalizioni[
      ,
      `:=`(
        Gruppo = paste(ifelse(SINDACO, "Sindaco", "Cand. sindaco"), COALIZIONE),
        SEGGI = SINDACO + SEGGIO_CANDIDATO_SINDACO
      )
    ],
    on = .(COALIZIONE)
  ]
  
  parlamento <- rbind(
    liste[, c("Gruppo", "SEGGI", "COLORE")],
    coalizioni[, c("Gruppo", "SEGGI", "COLORE")]
  )
  
  parlamento <- parlamento[parlamento$SEGGI > 0, ]
  
  ggplot2::ggplot(parlamento) +
    ggpol::geom_parliament(ggplot2::aes(seats = SEGGI, fill = Gruppo), color = "black") +
    ggplot2::scale_fill_manual(values = parlamento$COLORE, labels = paste(
      parlamento$Gruppo,
      "-",
      parlamento$SEGGI
    )) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
  
}