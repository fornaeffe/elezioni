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
        ELETTI = SINDACO + SEGGIO_CANDIDATO_SINDACO
      )
    ],
    on = .(COALIZIONE)
  ]
  
  parlamento <- rbind(
    liste[, c("Gruppo", "ELETTI", "COLORE")],
    coalizioni[, c("Gruppo", "ELETTI", "COLORE")]
  )
  
  parlamento <- parlamento[parlamento$ELETTI > 0, ]
  
  ggplot2::ggplot(parlamento) +
    ggpol::geom_parliament(ggplot2::aes(seats = ELETTI, fill = Gruppo), color = "black") +
    ggplot2::scale_fill_manual(values = parlamento$COLORE, labels = paste(
      parlamento$Gruppo,
      "-",
      parlamento$ELETTI
    )) +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()
  
}