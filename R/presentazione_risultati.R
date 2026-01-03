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

superamento_soglia_comunali <- function(
  risultato
) {
  soglie <- risultato$liste_sim[
    ,
    .(PROB_ELEGGERE = mean(ELETTI > 0)),
    by = .(LISTA)
  ]
  soglie_coalizioni <- risultato$coalizioni_sim[
    ,
    .(
      PROB_ELEGGERE = mean(SINDACO + SEGGIO_CANDIDATO_SINDACO),
      LISTA = paste("Cand. sindaco", COALIZIONE)
    ),
    by = .(COALIZIONE)
  ]
  
  soglie <- rbind(
    soglie[, .(LISTA, PROB_ELEGGERE)],
    soglie_coalizioni[, .(LISTA, PROB_ELEGGERE)]
  )
  
  soglie$PROB_ELEGGERE <- formattable::percent(soglie$PROB_ELEGGERE, 0)
  tbl <- kableExtra::kbl(
    soglie,
    col.names = c("LISTA", "Prob."),
    caption = "Prob. di eleggere qualcuno",
    row.names = FALSE
  )
  tbl <- kableExtra::kable_minimal(tbl)
  tbl
}

eletti_percentuale_xyplots <- function(
    risultato
) {
  tapply(
    risultato$liste_sim,
    risultato$liste_sim$LISTA,
    function(df) {
      plot(
        ELETTI ~ PERCENTUALE,
        data = df,
        col = COLORE,
        xlab = "Percentuale sui voti validi",
        main = df$LISTA[1]
      )
    }
  )
}

grafico_eletti <- function(nmax, PERCENTUALE, lista, COLORE) {
  
  colori <- colorRampPalette(
    
    c(
      "#000000",
      COLORE,
      "#FFFFFF"
    )
  )(length(levels(nmax)) + 1)[-1]
  tab <- spineplot(
    nmax ~ I(
      PERCENTUALE*100
    ),
    breaks = unique(round(quantile(PERCENTUALE*100, seq(0, 1, 0.1)))),
    col = colori,
    yaxlabels = NA,
    ylab = NA,
    xlab = "Percentuale sui voti validi",
    main = lista
  )
  
  # From https://stackoverflow.com/questions/74814855/how-can-i-plot-data-labels-over-spineplot-in-r
  nums <- t(apply(tab, 1,rev))
  pcts <- prop.table(cbind(0, nums), 1)
  pcts <- t(apply(pcts, 1, cumsum))
  yvals <- pcts[,-ncol(pcts)] + (pcts[,-1] - pcts[,-ncol(pcts)])/2
  xvals <- cumsum(c(0, rowSums(nums)/sum(rowSums(nums))))
  xvals <- xvals[-length(xvals)] + (xvals[-1] - xvals[-length(xvals)])/2
  xvals <- array(xvals, dim=dim(yvals))
  xvals <- c(xvals)
  yvals <- c(yvals)
  labs <- rep(colnames(nums), each = nrow(nums))
  
  text(x = xvals[nums > 5], y = yvals[nums > 5], labels = labs[nums > 5])
}

grafici_eletti <- function(
    risultato
) {
  
  risultato$liste_sim[
    ,
    grafico_eletti(
      factor(ELETTI),
      PERCENTUALE,
      LISTA,
      COLORE
    ),
    by = .(LISTA, COLORE)
  ]
  
  
}

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
  
  coalizioni_sim[
    ,
    grafico_eletti(
      ELETTI,
      PERCENTUALE,
      paste("Cand. sindaco", COALIZIONE),
      COLORE
    ),
    by = .(COALIZIONE, COLORE)
  ]
}