library(rstan)
library(shinystan)
library(gtools)

N <- 5
K <- 4

provincia <- rep(1:N, 4)
L <- length(provincia)

y <- t(rmultinom(L, 50, c(.3, .1, .2, .4)))

fit <- stan(
  "sandbox.stan",
  data = list(
    L = L,
    K = K,
    y = y
  ),
  cores = 4
)

summary(fit)
launch_shinystan(fit)


plot(
  PERCENTUALE_UNI ~ PERC_LISTA_BASE,
  data = dati$liste_uni[dati$liste_uni$LISTA == "Europa Verce", ]
)

# Emilia-Romagna - P01

lapply(
  lista_dataframes,
  function(df) {
    setdiff(
      c(
        "REGIONE",
        "PROVINCIA",
        "COMUNE",
        "ELETTORI",
        "VOTANTI",
        "COGNOME",
        "NOME",
        "LISTA",
        "VOTI_LISTA"
      ),
      toupper(names(df))
    )
  }
)
