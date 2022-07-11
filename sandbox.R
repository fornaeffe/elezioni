library(parallel)

l <- lapply(
  seq_len(100000),
  function(x) {
    X <- runif(1000, 0, 1)
    Y <- X * x + rnorm(1000, 0, 1)
    list(
      X = X,
      Y = Y
    )
  }
)


t0 <- proc.time()

risultato <- lapply(
  l,
  function(ll) {
    modello <- lm(Y ~ X, ll)
    modello$coefficients[2]
  }
)

proc.time() - t0



t0 <- proc.time()

cl <- makeCluster(10)

risultato <- parLapply(
  cl,
  l,
  function(ll) {
    modello <- lm(Y ~ X, ll)
    modello$coefficients[2]
  }
)

stopCluster(cl)

proc.time() - t0
