#### Parametri ####

# Scenario
scenario <- "01"

# Numero di iterazioni
iterazioni <- 200

# Deviazione standard del logit della distribuzione della percentuale 
# di ciascuna lista, ai vari livelli
sd_naz <- .4
sd_circ <- .1
sd_pluri <- .2

#### Carico librerie e script ####

library(readxl)
source("C.R")
source("S.R")

#### Caricamento dati ####



liste_naz <- read_xlsx(paste0("dati_2023/", scenario, ".xlsx"))
liste_naz$CL <- as.character(liste_naz$COALIZIONE)
liste_naz$CL[is.na(liste_naz$CL)] <- 
  as.character(liste_naz$LISTA[is.na(liste_naz$CL)])




load("dati_collegi/collegi.RData")

simula <- function(
    ramo,
    scenario,
    dati,
    liste_naz,
    
    iterazioni = 200,
    
    sd_naz = .4,
    sd_circ = .1,
    sd_pluri = .2
) {
  names(dati$collegi_pluri)[names(dati$collegi_pluri) == "SEGGI_PLURI"] <-
    "SEGGI"
  
  
  #### Calcolo il numero massimo di candidati per collegio pluri ####
  dati$collegi_pluri$CANDIDATI_MAX <- 
    pmin(4, dati$collegi_pluri$SEGGI)
  if (ramo == "Senato") {
    dati$collegi_pluri$CANDIDATI_MAX[dati$collegi_pluri$SEGGI == 1] <- 1
  }
  
  
  #### Preparo il data frame dei candidati ####
  n_cand <- sum(dati$collegi_pluri$CANDIDATI_MAX) + nrow(dati$collegi_uni)
  dati$candidati <- data.frame(
    LISTA = rep(liste_naz$LISTA, each = n_cand),
    CL = rep(liste_naz$CL, each = n_cand)
  )
  
  dati$candidati$CANDIDATO <- 
    factor(paste(dati$candidati$LISTA, seq_along(dati$candidati$LISTA)))
  
  
  #### Preparo i data frame delle liste ai diversi livelli ####
  
  dati$liste_circ <- merge(
    dati$circoscrizioni,
    liste_naz[, c("LISTA", "PERCENTUALE")]
  )
  
  dati$liste_pluri <- merge(
    dati$collegi_pluri,
    liste_naz[, c("LISTA")]
  )
  
  dati$liste_uni <- merge(
    dati$collegi_uni,
    liste_naz[, c("LISTA", "CL")]
  )
  
  dati$liste_uni$CAND_MINORANZA <- FALSE
  dati$liste_uni$MINORANZA <- FALSE
  
  #### Inizio iterazioni ####
  
  for (j in seq_len(iterazioni)) {
    ##### Randomizzo i voti ####
    
    dati$liste_circ$PERC_GREZZA <- plogis(rnorm(
      dati$liste_circ$PERCENTUALE,
      qlogis(dati$liste_circ$PERCENTUALE),
      sd_naz
    ))
    
    dati$liste_circ$PERCENTUALE_CIRC <- ave(
      dati$liste_circ$PERC_GREZZA,
      dati$liste_circ$CIRCOSCRIZIONE,
      FUN = function(x) x / sum(x)
    )
    
    dati$liste_pluri$PERCENTUALE_CIRC <- NULL
    
    dati$liste_pluri <- merge(
      dati$liste_pluri,
      dati$liste_circ[, c("CIRCOSCRIZIONE", "LISTA", "PERCENTUALE_CIRC")]
    )
    
    dati$liste_pluri$PERC_GREZZA <- plogis(rnorm(
      dati$liste_pluri$PERCENTUALE_CIRC,
      qlogis(dati$liste_pluri$PERCENTUALE_CIRC),
      sd_circ
    ))
    
    dati$liste_pluri$PERCENTUALE_PLURI <- ave(
      dati$liste_pluri$PERC_GREZZA,
      paste(
        dati$liste_pluri$CIRCOSCRIZIONE,
        dati$liste_pluri$COLLEGIOPLURINOMINALE
      ),
      FUN = function(x) x / sum(x)
    )
    
    dati$liste_uni$PERCENTUALE_PLURI <- NULL
    
    dati$liste_uni <- merge(
      dati$liste_uni,
      dati$liste_pluri[, c("COLLEGIOPLURINOMINALE", "LISTA", "PERCENTUALE_PLURI")]
    )
    
    dati$liste_uni$PERC_GREZZA <- plogis(rnorm(
      dati$liste_uni$PERCENTUALE_PLURI,
      qlogis(dati$liste_uni$PERCENTUALE_PLURI),
      sd_pluri
    ))
    
    dati$liste_uni$PERCENTUALE_UNI <- ave(
      dati$liste_uni$PERC_GREZZA,
      paste(
        dati$liste_uni$CIRCOSCRIZIONE,
        dati$liste_uni$COLLEGIOPLURINOMINALE,
        dati$liste_uni$COLLEGIOUNINOMINALE
      ),
      FUN = function(x) x / sum(x)
    )
    
    dati$liste_uni$VOTI_LISTA <- 
      dati$liste_uni$POP_2011 * dati$liste_uni$PERCENTUALE_UNI
    
    
    ##### Sorteggio i candidati ####
    
    dati$candidati_uni <- unique(dati$liste_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CL"
    )])
    
    dati$candidati$SCELTO_UNI <- FALSE
    dati$candidati_uni$CANDIDATO <- 
      factor(NA, levels = levels(dati$candidati$CANDIDATO))
    
    
    for (i in seq_along(dati$candidati_uni$CL)) {
      lista <- sample(
        liste_naz$LISTA[liste_naz$CL == dati$candidati_uni$CL[i]], 
        1, 
        prob = liste_naz$FRAZ_UNI[liste_naz$CL == dati$candidati_uni$CL[i]]
      )
      candidato <- sample(which(
        dati$candidati$LISTA == lista &
          !dati$candidati$SCELTO_UNI
      ), 1)
      
      dati$candidati_uni$CANDIDATO[i] <- dati$candidati$CANDIDATO[candidato]
      dati$candidati$SCELTO_UNI[candidato] <- TRUE
    }
    
    if (sum(duplicated(dati$candidati_uni$CANDIDATO)) > 0) stop(
      "Candidati uninominali duplicati"
    )
    
    
    dati$candidati_pluri <- dati$liste_pluri[
      rep(
        seq_along(dati$liste_pluri$CANDIDATI_MAX), 
        dati$liste_pluri$CANDIDATI_MAX
      ),
      c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "LISTA"
      )
    ]
    
    dati$candidati_pluri$NUMERO <- ave(
      seq_along(dati$candidati_pluri$LISTA),
      paste(
        dati$candidati_pluri$CIRCOSCRIZIONE,
        dati$candidati_pluri$COLLEGIOPLURINOMINALE,
        dati$candidati_pluri$LISTA
      ),
      FUN = seq_along
    )
    
    dati$candidati$SCELTO_PLURI <- 0
    dati$candidati_pluri$CANDIDATO <- 
      factor(NA, levels = levels(dati$candidati$CANDIDATO))
    
    for (i in seq_along(dati$candidati_pluri$NUMERO)) {
      if (runif(1) < liste_naz$FRAZ_PLURICAND[
        liste_naz$LISTA == dati$candidati_pluri$LISTA[i]
      ]) {
        papabili <- which(
          dati$candidati$LISTA == dati$candidati_pluri$LISTA[i] &
            dati$candidati$SCELTO_PLURI < 5 &
            (dati$candidati$SCELTO_UNI | dati$candidati$SCELTO_PLURI > 0)
        )
        
        if (length(papabili) > 0) {
          candidato <- sample(papabili, 1)
          
          dati$candidati_pluri$CANDIDATO[i] <- dati$candidati$CANDIDATO[candidato]
          dati$candidati$SCELTO_PLURI[candidato] <- 
            dati$candidati$SCELTO_PLURI[candidato] + 1
          
          next
        }
      }
      
      candidato <- which(
        dati$candidati$LISTA == dati$candidati_pluri$LISTA[i] &
          dati$candidati$SCELTO_PLURI == 0 &
          !dati$candidati$SCELTO_UNI
      )[1]
      
      dati$candidati_pluri$CANDIDATO[i] <- dati$candidati$CANDIDATO[candidato]
      dati$candidati$SCELTO_PLURI[candidato] <- 1
    }
    
    
    ##### Preparo i data frame per lo scrutinio ####
    dati$liste_uni$CANDIDATO <- NULL
    dati$liste_uni <- merge(
      dati$liste_uni,
      dati$candidati_uni
    )
    
    
    dati$candidati_uni <- merge(
      dati$candidati_uni,
      aggregate(
        VOTI_LISTA ~ 
          CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + COLLEGIOUNINOMINALE + CANDIDATO,
        dati$liste_uni,
        sum
      )
    )
    names(dati$candidati_uni)[names(dati$candidati_uni) == "VOTI_LISTA"] <-
      "VOTI_CANDIDATO"
    
    dati$candidati_uni$DATA_NASCITA <- as.POSIXct("1990-01-01")
    
    if (ramo == "Senato") {
      scrutinio <- S_scrutinio(
        dati$liste_uni[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "COLLEGIOUNINOMINALE",
          "CANDIDATO",
          "CAND_MINORANZA",
          "LISTA",
          "MINORANZA",
          "VOTI_LISTA"
        )],
        liste_naz[, c(
          "LISTA",
          "COALIZIONE",
          "MINORANZA"
        )],
        dati$candidati_uni[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "COLLEGIOUNINOMINALE",
          "CANDIDATO",
          "DATA_NASCITA",
          "VOTI_CANDIDATO"
        )],
        dati$candidati_pluri[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "LISTA",
          "NUMERO",
          "CANDIDATO"
        )],
        dati$collegi_pluri[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "SEGGI"
        )],
        296
      )
    } else {
      scrutinio <- C_scrutinio(
        dati$liste_uni[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "COLLEGIOUNINOMINALE",
          "CANDIDATO",
          "CAND_MINORANZA",
          "LISTA",
          "MINORANZA",
          "VOTI_LISTA"
        )],
        liste_naz[, c(
          "LISTA",
          "COALIZIONE",
          "MINORANZA"
        )],
        dati$candidati_uni[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "COLLEGIOUNINOMINALE",
          "CANDIDATO",
          "DATA_NASCITA",
          "VOTI_CANDIDATO"
        )],
        dati$candidati_pluri[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "LISTA",
          "NUMERO",
          "CANDIDATO"
        )],
        dati$collegi_pluri[, c(
          "CIRCOSCRIZIONE",
          "COLLEGIOPLURINOMINALE",
          "SEGGI"
        )],
        392
      )
    }
    
    
    scrutinio$liste_pluri <- merge(
      scrutinio$liste_pluri,
      aggregate(
        VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
        dati$liste_uni,
        sum
      )
    )
    
    scrutinio$liste_pluri <- merge(
      scrutinio$liste_pluri,
      aggregate(
        VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
        scrutinio$liste_pluri,
        sum
      ),
      by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE"),
      suffixes = c("", "_TOT")
    )
    
    scrutinio$liste_pluri$PERCENTUALE <-
      scrutinio$liste_pluri$VOTI_LISTA / scrutinio$liste_pluri$VOTI_LISTA_TOT
    
    scrutinio$liste_naz <- aggregate(
      VOTI_LISTA ~ LISTA,
      scrutinio$liste_pluri,
      sum
    )
    
    scrutinio$liste_naz$PERCENTUALE <- 
      scrutinio$liste_naz$VOTI_LISTA / sum(scrutinio$liste_naz$VOTI_LISTA)
    
    scrutinio$liste_naz <- merge(
      scrutinio$liste_naz,
      aggregate(
        ELETTI ~ LISTA,
        scrutinio$liste_pluri,
        sum
      )
    )
    
    scrutinio$liste_naz <- merge(
      scrutinio$liste_naz,
      liste_naz[, c("LISTA", "CL")]
    )
    
    scrutinio$candidati_uni <- merge(
      scrutinio$candidati_uni,
      dati$candidati_uni[, c("CANDIDATO", "CL")]
    )
    
    scrutinio$cl_naz <- aggregate(
      VOTI_LISTA ~ CL,
      scrutinio$liste_naz,
      sum
    )
    
    scrutinio$cl_naz$PERCENTUALE <- 
      scrutinio$cl_naz$VOTI_LISTA / sum(scrutinio$cl_naz$VOTI_LISTA)
    
    scrutinio$cl_naz <- merge(
      scrutinio$cl_naz,
      aggregate(
        ELETTI ~ CL,
        scrutinio$liste_naz,
        sum
      )
    )
    
    scrutinio$cl_naz <- merge(
      scrutinio$cl_naz,
      aggregate(
        ELETTO ~ CL,
        scrutinio$candidati_uni,
        sum
      )
    )
    
    scrutinio$cl_naz$ELETTI_TOT <- 
      scrutinio$cl_naz$ELETTI + scrutinio$cl_naz$ELETTO
    
    scrutinio$cl_naz <- scrutinio$cl_naz[order(
      scrutinio$cl_naz$CL
    ), ]
    
    scrutinio$liste_pluri$ITER <- j
    scrutinio$liste_naz$ITER <- j
    scrutinio$cl_naz$ITER <- j
    
    if (j == 1) {
      risultato <- list()
      risultato$liste_pluri <- scrutinio$liste_pluri
      risultato$liste_naz <- scrutinio$liste_naz
      risultato$cl_naz <- scrutinio$cl_naz
    } else {
      risultato$liste_pluri <- rbind(risultato$liste_pluri, scrutinio$liste_pluri)
      risultato$liste_naz <- rbind(risultato$liste_naz, scrutinio$liste_naz)
      risultato$cl_naz <- rbind(risultato$cl_naz, scrutinio$cl_naz)
    }
  }
  
  liste_naz$COL <- "#DDDDDD"
  liste_naz$COL[!is.na(liste_naz$COLORE)] <- 
    hsv(liste_naz$COLORE[!is.na(liste_naz$COLORE)] / 360, 1, .8, 1)
  
  risultato$liste_naz$COL <- NULL
  risultato$liste_naz <- merge(
    risultato$liste_naz,
    liste_naz[, c("LISTA", "COL")]
  )
  
  png(
    paste0("output/", scenario, substr(ramo, 1, 1), "_l_naz.png"),
    width = 800,
    height = 800,
    res = 120
  )
  plot(
    ELETTI ~ I(PERCENTUALE * 100),
    data = risultato$liste_naz,
    col = COL,
    main = paste0(ramo, ": eletti nei collegi plurinominali"),
    xlab = "Percentuale",
    ylab = "Eletti"
  )
  legend(
    "topleft",
    legend = liste_naz$LISTA[!is.na(liste_naz$COLORE)],
    pch = 19,
    col = liste_naz$COL[!is.na(liste_naz$COLORE)]
  )
  dev.off()
  
  risultato$cl_naz <- merge(
    risultato$cl_naz,
    liste_naz[!duplicated(liste_naz$CL), c("CL", "COL")]
  )
  
  png(
    paste0("output/", scenario, substr(ramo, 1, 1), "_cl_naz.png"),
    width = 800,
    height = 800,
    res = 120
  )
  plot(
    ELETTI_TOT ~ I(PERCENTUALE * 100),
    data = risultato$cl_naz,
    col = COL,
    main = paste0(ramo, ": eletti nei collegi uni e plurinominali"),
    xlab = "Percentuale",
    ylab = "Eletti"
  )
  abline(h = 391 / 2, lty = "dotted")
  legend(
    "topleft",
    legend = liste_naz$CL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)],
    pch = 19,
    col = liste_naz$COL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)]
  )
  dev.off()
  
  png(
    paste0("output/", scenario, substr(ramo, 1, 1), "_cl_naz_magg.png"),
    width = 800,
    height = 800,
    res = 120
  )
  plot(
    ELETTO ~ I(PERCENTUALE * 100),
    data = risultato$cl_naz,
    col = COL,
    main = paste0(ramo, ": eletti nei collegi uninominali"),
    xlab = "Percentuale nazionale",
    ylab = "Eletti"
  )
  abline(h = nrow(dati$collegi_uni) / 2, lty = "dotted")
  legend(
    "topleft",
    legend = liste_naz$CL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)],
    pch = 19,
    col = liste_naz$COL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)]
  )
  dev.off()
  
  for (lista in liste_naz$LISTA[liste_naz$GRAFICI]) {
    png(
      paste0(
        "output/",
        scenario,
        substr(ramo, 1, 1),
        "_l_naz_",
        liste_naz$ABBREV[liste_naz$LISTA == lista],
        ".png"
      ),
      width = 800,
      height = 800,
      res = 120
    )
    plot(
      ELETTI ~ I(PERCENTUALE * 100),
      data = risultato$liste_naz[risultato$liste_naz$LISTA == lista,],
      col = COL,
      xlab = "Percentuale",
      ylab = "Eletti"
    )
    mytitle = paste0(ramo, ": eletti nei collegi plurinominali")
    mysubtitle = lista
    mtext(side=3, line=2, cex = 1.5, mytitle)
    mtext(side=3, line=1, mysubtitle)
    dev.off()
    
    
    png(
      paste0(
        "output/",
        scenario,
        substr(ramo, 1, 1),
        "_nmax_",
        liste_naz$ABBREV[liste_naz$LISTA == lista],
        ".png"
      ),
      width = 800,
      height = 800,
      res = 120
    )
    nmax <- factor(
      risultato$liste_pluri$NUMERO_MAX[risultato$liste_pluri$LISTA == lista],
      levels = 0:4
    )
    nmax[is.na(nmax)] <- 4
    colori <- c(hcl.colors(4), "#FFFFFF")
    spineplot(
      nmax ~ I(
        risultato$liste_pluri$PERCENTUALE[
          risultato$liste_pluri$LISTA == lista
        ] *100
      ),
      breaks = 20,
      col = colori,
      yaxlabels = NA,
      ylab = NA,
      xlab = "Percentuale nel collegio plurinominale"
    )
    mytitle = "Probabilità di elezone in base alla posizione nel listino"
    mysubtitle = paste0(
      lista,
      "  -  ",
      ramo,
      "  -  Fraz. pluricandature: ", 
      format(
        liste_naz$FRAZ_PLURICAND[liste_naz$LISTA == lista] * 100,
        digits = 2
      ), 
      "%"
    )
    mtext(side=3, line=2, cex = 1.5, mytitle)
    mtext(side=3, line=1, mysubtitle)
    legend(
      "topleft",
      legend = levels(nmax)[-1],
      fill = rev(colori[-5]),
      title = "Posizione"
    )
    dev.off()
  }
}

simula(
  "Camera",
  scenario,
  camera,
  liste_naz,
  iterazioni,
  sd_naz,
  sd_circ,
  sd_pluri
)

simula(
  "Senato",
  scenario,
  senato,
  liste_naz,
  iterazioni,
  sd_naz,
  sd_circ,
  sd_pluri
)




