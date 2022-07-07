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
  
  scrutinio <- C_scrutinio(
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
  
  scrutinio$liste_pluri <- merge(
    scrutinio$liste_pluri,
    aggregate(
      VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
      camera$liste_uni,
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
    camera$candidati_uni[, c("CANDIDATO", "CL")]
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

if (iterazioni == 1) {
  liste_naz <- merge(
    liste_naz,
    aggregate(
      ELETTO ~ LISTA,
      scrutinio$candidati_pluri,
      sum
    ),
    all.x = TRUE
  )
  barplot(
    ELETTO ~ LISTA,
    data = liste_naz
  )
} else {
  plot(
    ELETTI ~ PERCENTUALE,
    data = risultato$liste_naz
  )
  
  
  boxplot(
    ELETTI_TOT ~ CL,
    data = risultato$cl_naz
  )
  
  
  lista <- "Movimento 5 Stelle"

  nmax <- factor(
    risultato$liste_pluri$NUMERO_MAX[risultato$liste_pluri$LISTA == lista],
    levels = 0:4
  )
  nmax[is.na(nmax)] <- 4
  colori <- c(hcl.colors(5)[-5], "#FFFFFF")
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

