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

eletti_percentuale_xyplot <- function(
    ELETTI,
    PERCENTUALE,
    COLORE,
    TITOLO
){
  p <- plot(
    ELETTI ~ PERCENTUALE,
    pch = 16,
    col = paste0(COLORE, "10"),
    xlab = "Percentuale sui voti validi",
    xaxt = "n",
    main = TITOLO
  )
  ticks <- formattable::percent(pretty(PERCENTUALE), 0)
  axis(side = 1, at = ticks, labels = ticks)
  
  invisible(p)
}

eletti_percentuale_xyplots_liste <- function(
    risultato
) {
  
  spl <- split(risultato$liste_sim, by = "LISTA")
  
  lapply(
    names(spl),
    function(g) {
      eletti_percentuale_xyplot(
        spl[[g]]$ELETTI,
        spl[[g]]$PERCENTUALE,
        risultato$liste[LISTA == g]$COLORE,
        g
      )
    }
  )
  
  invisible()
}

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

grafico_eletti <- function(nmax, PERCENTUALE, lista, COLORE) {
  
  colori <- colorRampPalette(
    
    c(
      "#000000",
      COLORE,
      "#FFFFFF"
    )
  )(length(levels(nmax)) + 1)[-1]
  ticks <- formattable::percent(unique(round(quantile(PERCENTUALE, seq(0, 1, 0.1)), 2)), 0)
  tab <- spineplot(
    nmax ~ PERCENTUALE,
    breaks = ticks,
    col = colori,
    yaxlabels = NA,
    xaxlabels = ticks,
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
  
  invisible(tab)
}

grafici_eletti <- function(
    risultato
) {
  
  spl <- split(risultato$liste_sim, by = "LISTA")
  
  lapply(
    names(spl),
    function(g) {
      grafico_eletti(
        factor(spl[[g]]$ELETTI),
        spl[[g]]$PERCENTUALE,
        g,
        risultato$liste[LISTA == g]$COLORE
      )
    }
  )
  
  invisible()
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

disegna_tabella_prob_comunali <- function(risultato) {
  lp <- data.table::copy(risultato$liste_sim)
  lp$ELETTI <- factor(lp$ELETTI, levels = 0:risultato$num_consiglieri)
  tbl <- table(lp$LISTA, lp$ELETTI)
  tbl2 <- apply(
    tbl,
    1,
    function(x) {
      rev(cumsum(rev(x[-1]))) / sum(x)
    }
  )
  
  tbl2 <- as.data.frame(tbl2)
  for (i in 1:ncol(tbl2)) {
    tbl2[,i] <- formattable::percent(tbl2[, i], 1)
  }
  
  kb <- kableExtra::kbl(tbl2, row.names = TRUE)
  kb <- kableExtra::kable_minimal(kb)
  
  valori_hsv <- rgb2hsv(col2rgb(risultato$liste[LISTA != "astensione"]$COLORE))
  
  for (i in 1:ncol(tbl2)) {
    kb <- kableExtra::column_spec(
      kb,
      i+1,
      background = hsv(
        valori_hsv["h", i], 
        valori_hsv["s", i]*tbl2[,i], 
        1
      )
    )
  }
  kb
}

andamento_passato <- function(risultato, log_percent = FALSE) {
  elettori <- risultato$comuni_liste[
    LISTA == "astensione",
    sum(ELETTORI)
  ]
  astenuti <- risultato$liste_sim[
    ,
    .(
      LISTA = "astensione",
      VOTI_LISTA = elettori - sum(VOTI_LISTA),
      COLORE = risultato$liste[LISTA == "astensione", COLORE]
    ),
    by = .(SIM)
  ]
  liste_sim <- rbind(risultato$liste_sim, astenuti, fill = TRUE)
  liste_sim[
    ,
    PERCENTUALE_ELETTORI := VOTI_LISTA / elettori
  ]
  
  lista_elezione <- merge(
    risultato$liste_elezioni,
    risultato$liste[, c("LISTA", "COLORE")],
    by = "LISTA"
  )
  
  # Ordino secondo la data 
  lista_elezione <- lista_elezione[order(lista_elezione$DATA),]
  
  
  
  # Grafico dell'andamento della percentuale in base alla data
  plot(
    PERCENTUALE * 100 ~ DATA, 
    data = lista_elezione, 
    pch = "",
    ylab = "Percentuale sugli elettori",
    xlim = c(min(DATA), data_elezione),
    ylim = c(0, max(liste_sim[
      ,
      .(MAX = 100*quantile(PERCENTUALE_ELETTORI, .975)),
      by = .(LISTA)
    ]$MAX)),
    log = ifelse(log_percent, "y", "")
  )
  tapply(lista_elezione, lista_elezione$LISTA, function(df) {
    lines(PERCENTUALE * 100 ~ DATA, data = df, col = df$COLORE, lwd = 2)
  })
  
  for (lista in risultato$liste$LISTA) {
    y0 <- 100*plogis(risultato$liste$LOGIT_P[risultato$liste$LISTA == lista])
    y1 <- 100*quantile(
      liste_sim$PERCENTUALE_ELETTORI[liste_sim$LISTA == lista],
      0.975
    )
    y2 <- 100*quantile(
      liste_sim$PERCENTUALE_ELETTORI[liste_sim$LISTA == lista],
      0.025
    )
    polygon(
      c(risultato$liste$DATA[risultato$liste$LISTA == lista], data_elezione, data_elezione),
      c(y0, y1, y2),
      border = risultato$liste$COLORE[risultato$liste$LISTA == lista],
      col = adjustcolor(risultato$liste$COLORE[risultato$liste$LISTA == lista], alpha.f = 0.5)
    )
  }
  
  legend(
    "bottomleft", 
    inset=c(0,-0.6), 
    legend=risultato$liste$LISTA, 
    lwd = 2, 
    col = risultato$liste$COLORE,
    ncol = 2
  )
  
  
}
