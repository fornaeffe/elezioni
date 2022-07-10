

data {
  int<lower=0> N; // Numero di province
  int<lower=0> L; // Numero di dati
  int<lower=0> K; // Numero di aree
  int<lower=1, upper=N> provincia[L]; // Provincia di ciascun dato
  int y[L, K]; // Dati
  vector<lower=0>[N] pop; // Frazioine nazionale di votanti per ciascuna provincia
  int<lower = 0> y0[K]; // Risultati ultimo sondaggio nazionale
}

parameters {
  real<lower=0> Sigma; // Variabilità spaziale
  real<lower=0> sigma; // Variabilità temporale
  vector[K] THETA; // Coefficienti nazionali
  vector[K] Theta[N]; // Coefficienti base per provincia
  vector[K] theta[L]; // Coefficienti per provincia per ciascuna estrazione
  vector[K] theta0[N]; // Coefficienti per provincia alle future elezioni
}

transformed parameters {
  matrix[K, N] P;
  vector[K] p;
  
  for (i in 1:N) {
    P[, i] = softmax(theta0[i]);
  }
  
  p = P * pop;
}

model {
  for (i in 1:L) {
    y[i] ~ multinomial(softmax(theta[i]));
    theta[i] ~ normal(Theta[provincia[i]], sigma);
  }
  
  for (i in 1:N) {
    theta0[i] ~ normal(Theta[i], sigma);
    Theta[i] ~ normal(THETA, Sigma);
  }
  
  y0 ~ multinomial(p);
  
  THETA ~ normal(0, 5);
  sigma ~ gamma(2, .1);
  Sigma ~ gamma(2, .1);
}

