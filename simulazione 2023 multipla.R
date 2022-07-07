#### Parametri ####

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

#### Caricamento dati ####



liste_naz <- read_xlsx("dati_2023/dati_2023.xlsx")
liste_naz$CL <- as.character(liste_naz$COALIZIONE)
liste_naz$CL[is.na(liste_naz$CL)] <- 
  as.character(liste_naz$LISTA[is.na(liste_naz$CL)])




load("dati_collegi/collegi.RData")


names(camera$collegi_pluri)[names(camera$collegi_pluri) == "SEGGI_PLURI"] <-
  "SEGGI"


#### Calcolo il numero massimo di candidati per collegio pluri ####
camera$collegi_pluri$CANDIDATI_MAX <- 
  pmin(4, ceiling(camera$collegi_pluri$SEGGI / 2))

#### Preparo il data frame dei candidati ####
n_cand <- sum(camera$collegi_pluri$CANDIDATI_MAX) + nrow(camera$collegi_uni)
camera$candidati <- data.frame(
  LISTA = rep(liste_naz$LISTA, each = n_cand),
  CL = rep(liste_naz$CL, each = n_cand)
)

camera$candidati$CANDIDATO <- 
  factor(paste(camera$candidati$LISTA, seq_along(camera$candidati$LISTA)))


#### Preparo i data frame delle liste ai diversi livelli ####

camera$liste_circ <- merge(
  camera$circoscrizioni,
  liste_naz[, c("LISTA", "PERCENTUALE")]
)

camera$liste_pluri <- merge(
  camera$collegi_pluri,
  liste_naz[, c("LISTA")]
)

camera$liste_uni <- merge(
  camera$collegi_uni,
  liste_naz[, c("LISTA", "CL")]
)

camera$liste_uni$CAND_MINORANZA <- FALSE
camera$liste_uni$MINORANZA <- FALSE

#### Inizio iterazioni ####

for (j in seq_len(iterazioni)) {
  ##### Randomizzo i voti ####
  
  camera$liste_circ$PERC_GREZZA <- plogis(rnorm(
    camera$liste_circ$PERCENTUALE,
    qlogis(camera$liste_circ$PERCENTUALE),
    sd_naz
  ))
  
  camera$liste_circ$PERCENTUALE_CIRC <- ave(
    camera$liste_circ$PERC_GREZZA,
    camera$liste_circ$CIRCOSCRIZIONE,
    FUN = function(x) x / sum(x)
  )
  
  camera$liste_pluri$PERCENTUALE_CIRC <- NULL
  
  camera$liste_pluri <- merge(
    camera$liste_pluri,
    camera$liste_circ[, c("CIRCOSCRIZIONE", "LISTA", "PERCENTUALE_CIRC")]
  )
  
  camera$liste_pluri$PERC_GREZZA <- plogis(rnorm(
    camera$liste_pluri$PERCENTUALE_CIRC,
    qlogis(camera$liste_pluri$PERCENTUALE_CIRC),
    sd_circ
  ))
  
  camera$liste_pluri$PERCENTUALE_PLURI <- ave(
    camera$liste_pluri$PERC_GREZZA,
    paste(
      camera$liste_pluri$CIRCOSCRIZIONE,
      camera$liste_pluri$COLLEGIOPLURINOMINALE
    ),
    FUN = function(x) x / sum(x)
  )
  
  camera$liste_uni$PERCENTUALE_PLURI <- NULL
  
  camera$liste_uni <- merge(
    camera$liste_uni,
    camera$liste_pluri[, c("COLLEGIOPLURINOMINALE", "LISTA", "PERCENTUALE_PLURI")]
  )
  
  camera$liste_uni$PERC_GREZZA <- plogis(rnorm(
    camera$liste_uni$PERCENTUALE_PLURI,
    qlogis(camera$liste_uni$PERCENTUALE_PLURI),
    sd_pluri
  ))
  
  camera$liste_uni$PERCENTUALE_UNI <- ave(
    camera$liste_uni$PERC_GREZZA,
    paste(
      camera$liste_uni$CIRCOSCRIZIONE,
      camera$liste_uni$COLLEGIOPLURINOMINALE,
      camera$liste_uni$COLLEGIOUNINOMINALE
    ),
    FUN = function(x) x / sum(x)
  )
  
  camera$liste_uni$VOTI_LISTA <- 
    camera$liste_uni$POP_2011 * camera$liste_uni$PERCENTUALE_UNI
  
  
  ##### Sorteggio i candidati ####
  
  camera$candidati_uni <- unique(camera$liste_uni[, c(
    "CIRCOSCRIZIONE",
    "COLLEGIOPLURINOMINALE",
    "COLLEGIOUNINOMINALE",
    "CL"
  )])
  
  camera$candidati$SCELTO_UNI <- FALSE
  camera$candidati_uni$CANDIDATO <- 
    factor(NA, levels = levels(camera$candidati$CANDIDATO))
  
  
  for (i in seq_along(camera$candidati_uni$CL)) {
    lista <- sample(
      liste_naz$LISTA[liste_naz$CL == camera$candidati_uni$CL[i]], 
      1, 
      prob = liste_naz$FRAZ_UNI[liste_naz$CL == camera$candidati_uni$CL[i]]
    )
    candidato <- sample(which(
      camera$candidati$LISTA == lista &
        !camera$candidati$SCELTO_UNI
    ), 1)
    
    camera$candidati_uni$CANDIDATO[i] <- camera$candidati$CANDIDATO[candidato]
    camera$candidati$SCELTO_UNI[candidato] <- TRUE
  }
  
  if (sum(duplicated(camera$candidati_uni$CANDIDATO)) > 0) stop(
    "Candidati uninominali duplicati"
  )
  
  
  camera$candidati_pluri <- camera$liste_pluri[
    rep(
      seq_along(camera$liste_pluri$CANDIDATI_MAX), 
      camera$liste_pluri$CANDIDATI_MAX
    ),
    c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA"
    )
  ]
  
  camera$candidati_pluri$NUMERO <- ave(
    seq_along(camera$candidati_pluri$LISTA),
    paste(
      camera$candidati_pluri$CIRCOSCRIZIONE,
      camera$candidati_pluri$COLLEGIOPLURINOMINALE,
      camera$candidati_pluri$LISTA
    ),
    FUN = seq_along
  )
  
  camera$candidati$SCELTO_PLURI <- 0
  camera$candidati_pluri$CANDIDATO <- 
    factor(NA, levels = levels(camera$candidati$CANDIDATO))
  
  for (i in seq_along(camera$candidati_pluri$NUMERO)) {
    if (runif(1) < liste_naz$FRAZ_PLURICAND[
      liste_naz$LISTA == camera$candidati_pluri$LISTA[i]
    ]) {
      papabili <- which(
        camera$candidati$LISTA == camera$candidati_pluri$LISTA[i] &
          camera$candidati$SCELTO_PLURI < 5 &
          (camera$candidati$SCELTO_UNI | camera$candidati$SCELTO_PLURI > 0)
      )
      
      if (length(papabili) > 0) {
        candidato <- sample(papabili, 1)
        
        camera$candidati_pluri$CANDIDATO[i] <- camera$candidati$CANDIDATO[candidato]
        camera$candidati$SCELTO_PLURI[candidato] <- 
          camera$candidati$SCELTO_PLURI[candidato] + 1
        
        next
      }
    }
    
    candidato <- which(
      camera$candidati$LISTA == camera$candidati_pluri$LISTA[i] &
        camera$candidati$SCELTO_PLURI == 0 &
        !camera$candidati$SCELTO_UNI
    )[1]
    
    camera$candidati_pluri$CANDIDATO[i] <- camera$candidati$CANDIDATO[candidato]
    camera$candidati$SCELTO_PLURI[candidato] <- 1
  }
  

  ##### Preparo i data frame per lo scrutinio ####
  camera$liste_uni$CANDIDATO <- NULL
  camera$liste_uni <- merge(
    camera$liste_uni,
    camera$candidati_uni
  )
  
  
  camera$candidati_uni <- merge(
    camera$candidati_uni,
    aggregate(
      VOTI_LISTA ~ 
        CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + COLLEGIOUNINOMINALE + CANDIDATO,
      camera$liste_uni,
      sum
    )
  )
  names(camera$candidati_uni)[names(camera$candidati_uni) == "VOTI_LISTA"] <-
    "VOTI_CANDIDATO"
  
  camera$candidati_uni$DATA_NASCITA <- as.POSIXct("1990-01-01")
  
  risultato <- C_scrutinio(
    camera$liste_uni[, c(
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
    camera$candidati_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CANDIDATO",
      "DATA_NASCITA",
      "VOTI_CANDIDATO"
    )],
    camera$candidati_pluri[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "LISTA",
      "NUMERO",
      "CANDIDATO"
    )],
    camera$collegi_pluri[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "SEGGI"
    )],
    392
  )
  
  risultato$liste_pluri <- merge(
    risultato$liste_pluri,
    aggregate(
      VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
      camera$liste_uni,
      sum
    )
  )
  
  risultato$liste_pluri <- merge(
    risultato$liste_pluri,
    aggregate(
      VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
      risultato$liste_pluri,
      sum
    ),
    by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE"),
    suffixes = c("", "_TOT")
  )
  
  risultato$liste_pluri$PERCENTUALE <-
    risultato$liste_pluri$VOTI_LISTA / risultato$liste_pluri$VOTI_LISTA_TOT
  
  risultato$liste_pluri <- risultato$liste_pluri[order(
    risultato$liste_pluri$CIRCOSCRIZIONE,
    risultato$liste_pluri$COLLEGIOPLURINOMINALE,
    risultato$liste_pluri$LISTA
  ),]
  
  risultato$liste_naz <- aggregate(
    VOTI_LISTA ~ LISTA,
    risultato$liste_pluri,
    sum
  )
  
  risultato$liste_naz$PERCENTUALE <- 
    risultato$liste_naz$VOTI_LISTA / sum(risultato$liste_naz$VOTI_LISTA)
  
  risultato$liste_naz <- merge(
    risultato$liste_naz,
    aggregate(
      ELETTI ~ LISTA,
      risultato$liste_pluri,
      sum
    )
  )
  
  risultato$liste_naz <- merge(
    risultato$liste_naz,
    liste_naz[, c("LISTA", "CL")]
  )
  
  risultato$liste_naz <- risultato$liste_naz[order(
    risultato$liste_naz$LISTA
  ), ]
  
  risultato$candidati_uni <- merge(
    risultato$candidati_uni,
    camera$candidati_uni[, c("CANDIDATO", "CL")]
  )
  
  risultato$candidati_uni <- risultato$candidati_uni[order(
    risultato$candidati_uni$CIRCOSCRIZIONE,
    risultato$candidati_uni$COLLEGIOPLURINOMINALE,
    risultato$candidati_uni$COLLEGIOUNINOMINALE,
    risultato$candidati_uni$CANDIDATO
  ), ]
  
  risultato$cl_naz <- aggregate(
    VOTI_LISTA ~ CL,
    risultato$liste_naz,
    sum
  )
  
  risultato$cl_naz$PERCENTUALE <- 
    risultato$cl_naz$VOTI_LISTA / sum(risultato$cl_naz$VOTI_LISTA)
  
  risultato$cl_naz <- merge(
    risultato$cl_naz,
    aggregate(
      ELETTI ~ CL,
      risultato$liste_naz,
      sum
    )
  )
  
  risultato$cl_naz <- merge(
    risultato$cl_naz,
    aggregate(
      ELETTO ~ CL,
      risultato$candidati_uni,
      sum
    )
  )
  
  risultato$cl_naz$ELETTI_TOT <- 
    risultato$cl_naz$ELETTI + risultato$cl_naz$ELETTO
  
  risultato$cl_naz <- risultato$cl_naz[order(
    risultato$cl_naz$CL
  ), ]
  
  if (j == 1) {
    cat("j = ", j, "\n\n")
    
    etichette_circ <- risultato$liste_pluri$CIRCOSCRIZIONE
    etichette_pluri <- risultato$liste_pluri$COLLEGIOPLURINOMINALE
    etichette_liste <- risultato$liste_pluri$LISTA
    res_liste_pluri_eletti <- matrix(
      risultato$liste_pluri$ELETTI,
      ncol = 1,
      dimnames = list(paste(etichette_circ, etichette_pluri, etichette_liste))
    )
    res_liste_pluri_nmax <- matrix(
      risultato$liste_pluri$NUMERO_MAX,
      ncol = 1,
      dimnames = list(paste(etichette_circ, etichette_pluri, etichette_liste))
    )
    res_liste_pluri_percentuale <- matrix(
      risultato$liste_pluri$PERCENTUALE,
      ncol = 1,
      dimnames = list(paste(etichette_circ, etichette_pluri, etichette_liste))
    )
    
    res_liste_naz_percentuale <- matrix(
      risultato$liste_naz$PERCENTUALE,
      ncol = 1,
      dimnames = list(risultato$liste_naz$LISTA)
    )
    
    res_liste_naz_eletti <- matrix(
      risultato$liste_naz$ELETTI,
      ncol = 1,
      dimnames = list(risultato$liste_naz$LISTA)
    )
    
    res_cl_naz_percentuale <- matrix(
      risultato$cl_naz$PERCENTUALE,
      ncol = 1,
      dimnames = list(risultato$cl_naz$CL)
    )
    
    res_cl_naz_uni <- matrix(
      risultato$cl_naz$ELETTO,
      ncol = 1,
      dimnames = list(risultato$cl_naz$CL)
    )
    
    res_cl_naz_eletti <- matrix(
      risultato$cl_naz$ELETTI_TOT,
      ncol = 1,
      dimnames = list(risultato$cl_naz$CL)
    )
    
  } else {
    res_liste_pluri_eletti <- 
      cbind(res_liste_pluri_eletti, risultato$liste_pluri$ELETTI)
    
    res_liste_pluri_nmax <- 
      cbind(res_liste_pluri_nmax, risultato$liste_pluri$NUMERO_MAX)
    
    res_liste_pluri_percentuale <- 
      cbind(res_liste_pluri_percentuale, risultato$liste_pluri$PERCENTUALE)
    
    res_liste_naz_percentuale <- 
      cbind(res_liste_naz_percentuale, risultato$liste_naz$PERCENTUALE)
    
    res_liste_naz_eletti <- 
      cbind(res_liste_naz_eletti, risultato$liste_naz$ELETTI)
    
    res_cl_naz_percentuale <- 
      cbind(res_cl_naz_percentuale, risultato$cl_naz$PERCENTUALE)
    
    res_cl_naz_uni <- 
      cbind(res_cl_naz_uni, risultato$cl_naz$ELETTO)
    
    res_cl_naz_eletti <- 
      cbind(res_cl_naz_eletti, risultato$cl_naz$ELETTI_TOT)
  }
}

if (iterazioni == 1) {
  liste_naz <- merge(
    liste_naz,
    aggregate(
      ELETTO ~ LISTA,
      risultato$candidati_pluri,
      sum
    ),
    all.x = TRUE
  )
  barplot(
    ELETTO ~ LISTA,
    data = liste_naz
  )
} else {
  plot(res_liste_naz_eletti ~ res_liste_naz_percentuale, col = 1:10)
  legend(
    "topleft",
    legend = rownames(res_liste_naz_eletti),
    pch = 1,
    col = 1:10
  )
  
  boxplot(t(res_cl_naz_eletti))
  
  plot(
    res_cl_naz_eletti ~ I(res_cl_naz_percentuale * 100),
    xlim = c(40, 60),
    ylim = c(150, 250),
    col = c("blue", "black", "orange"),
    main = "Seggi alla Camera",
    xlab = "Percentuale",
    ylab = "Seggi"
  )
  legend(
    "topleft",
    legend = rownames(res_cl_naz_eletti),
    pch = 1,
    col = c("blue", "black", "orange")
  )
  abline(h = 391 / 2)
  
  print(
    sum(res_cl_naz_eletti[rownames(res_cl_naz_eletti) == "DX",] > 391 / 2) / iterazioni
  )
  
  print(
    mean(res_liste_naz_eletti[rownames(res_liste_naz_eletti) == "Fratelli d'Italia",])
  )
  
  print(
    sum(res_liste_naz_eletti[rownames(res_liste_naz_eletti) == "EV - SI",] == 0) / iterazioni
  )
  
  print(
    mean(
      res_liste_naz_eletti[
        rownames(res_liste_naz_eletti) == "EV - SI",
        res_liste_naz_eletti[rownames(res_liste_naz_eletti) == "EV - SI",] > 0
      ]
    )
  )
  
  plot(
    res_liste_naz_eletti[rownames(res_liste_naz_eletti) == "EV - SI",] ~
      I(res_liste_naz_percentuale[rownames(res_liste_naz_percentuale) == "EV - SI",] * 100),
    xlab = "Percentuale",
    ylab = "Seggi",
    main = "Seggi Camera (proporz.) di EV - SI",
    col = "#00BB00"
  )
  abline(v = 3.5, lty = "dotted")
  
  lista <- "Fratelli d'Italia"

  nmax <- factor(
    c(res_liste_pluri_nmax[etichette_liste == lista, ]),
    levels = 0:4
  )
  nmax[is.na(nmax)] <- 4
  colori <- c(hcl.colors(5)[-5], "#FFFFFF")
  spineplot(
    nmax ~ I(c(res_liste_pluri_percentuale[etichette_liste == lista, ]) * 100),
    breaks = 20,
    col = colori,
    yaxlabels = NA,
    ylab = NA,
    xlab = "Percentuale nel collegio plurinominale"
  )
  mytitle = "Probabilità di elezone in base al numero di listino"
  mysubtitle = paste0(
    lista,
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
    lwd = 10,
    col = rev(colori[-5])
  )
}

