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

eletti_percentuale_xyplots_coalizioni <- function(
    risultato,
    colonna_percentuale = "PERCENTUALE"
){
  spl <- split(risultato$coalizioni_sim, by = "COALIZIONE")
  
  lapply(
    names(spl),
    function(g) {
      eletti_percentuale_xyplot(
        spl[[g]]$ELETTI,
        spl[[g]][[colonna_percentuale]],
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
  nums <- t(apply(tab, 1, rev))
  
  # cutoff
  cutoff <- length(nmax) * 0.005
  
  if (ncol(nums) == 1) {
    ## ---- CASO DEGENERATO: un solo valore di eletti ----
    
    xvals <- cumsum(c(0, rowSums(nums) / sum(rowSums(nums))))
    xvals <- xvals[-length(xvals)] + diff(xvals) / 2
    yvals <- rep(0.5, length(xvals))
    
    labs <- colnames(nums)
    
    text(
      x = xvals[nums[,1] > cutoff],
      y = yvals[nums[,1] > cutoff],
      labels = labs[nums[,1] > cutoff]
    )
    
  } else {
    ## ---- CASO NORMALE ----
    
    pcts <- prop.table(cbind(0, nums), 1)
    pcts <- t(apply(pcts, 1, cumsum))
    
    yvals <- pcts[, -ncol(pcts)] +
      (pcts[, -1] - pcts[, -ncol(pcts)]) / 2
    
    xvals <- cumsum(c(0, rowSums(nums) / sum(rowSums(nums))))
    xvals <- xvals[-length(xvals)] + diff(xvals) / 2
    xvals <- array(xvals, dim = dim(yvals))
    
    labs <- rep(colnames(nums), each = nrow(nums))
    
    text(
      x = xvals[nums > cutoff],
      y = yvals[nums > cutoff],
      labels = labs[nums > cutoff]
    )
  }
  
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
  
  kb <- kableExtra::kable_styling(
    kb,
    htmltable_class = "tabella-piccola"
  )
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

tabella_flussi <- function(
    risultato
){
  data.table::setorder(risultato$corrispondenza_liste, DATA, ELEZIONE, LISTA_ORIGINALE, LISTA)
  tab <- kableExtra::kbl(
    risultato$corrispondenza_liste[,.(ELEZIONE, LISTA_ORIGINALE, LISTA, FATTORE)],
    col.names = c("ELEZIONE", "LISTA", "LISTA FUTURA", "FRAZIONE"),
    caption = "Corrispondenza tra liste passate e future",
    row.names = FALSE
  )
  tab <- kableExtra::kable_minimal(tab)
  tab
}
