MCDownAndInPut <- function(S0, K, T, r, sigma, N, q, n, H) {
  # Parametri
  S0 <- 2.7
  K <- 2.65
  T <- 0.08
  r <- 0.03
  sigma <- 0.25
  N <- 10000
  H <- 2
  n <- 30
  q <- 0
  tau <- T / n

  S[1] <- S0
  P <- numeric(N)
  activated <- FALSE
  
  for (j in 1:N) {
    for (i in 1:n) {
      epsilon <- rnorm(1)
      S[i + 1] <- S[i] * exp((r - sigma^2 - q) * tau + sqrt(tau) * sigma * epsilon)
    }
    
    if (min(S[i]) > H) {
      activated <- TRUE
      break
    }
  }
  
  if (activated) {
    payoff <- max(K - S[n], 0)
  } else {
    payoff <- 0
  }
  
  MCDownAndInPut <- exp(-r * T) * payoff
  return(MCDownAndInPut)
}

# Esempio di utilizzo
risultato <- MCDownAndInPut(2.7, 2.5, 0.08, 0.03, 0.25, 10000, 0, 30, 2)
print(result)














DAIOP <- function(sigma, S0, K, r, T, q, n, H) {
  tau <- T/n
  eta <- r/(sigma*sigma) - 1/2
  gamma <- sqrt(eta*eta + 2*r/(sigma*sigma))
  
  a1 <- (log(S0/K)/(sigma*sqrt(T))) + (1+eta)*sigma*sqrt(T)
  a2 <- a1 - sigma*sqrt(T)
  b1 <- (log(H*H/(S0*K))/(sigma*sqrt(T))) + (1+eta)*sigma*sqrt(T)
  b2 <- b1 - sigma*sqrt(T)
  c1 <- (log(H/S0)/(sigma*sqrt(T))) + gamma*sigma*sqrt(T)
  c2 <- c1 - 2*gamma*sigma*sqrt(T)
  
  DAIOP <- (K * exp(-r * T) * (1 - pnorm(a2)) - S0 * (1 - pnorm(a1))) +
    (K * exp(-r * T) * ((H / S0)^(2 * eta)) * (1 - pnorm(b2)) - S0 * ((H / S0)^(2 * (eta + 1))) * (1 - pnorm(b1))) 
  
  return(DAIOP)
}

# Esempio di utilizzo
result <- DAIOP(0.25, 2.70, 2.65, 0.03, 0.08, 0, 30, 2)
print(result)







# Monte Carlo
result <- MCDownAndInPut(2.7, 2.5, 0.08, 0.03, 0.25, 10000, 0, 30, 2.5)
print(result)



# B&S
result1 <- DAIOP(0.25, 2.70, 2.65, 0.03, 0.08, 0, 30, 2)
print(result1)






# Numero di prove
num_prove <- 100

# Inizializza il contatore
conteggio <- 0

# Esegui il confronto su 100 prove
for (i in 1:num_prove) {
  result <- MCDownAndInPut(2.7, 2.5, 0.08, 0.03, 0.25, 10000, 0, 30, 2)
  result1 <- DAIOP(0.25, 2.70, 2.65, 0.03, 0.08, 0, 30, 2)
  
  if (result > result1) {
    conteggio <- conteggio + 1
  }
}

# Stampare il conteggio dei casi in cui il primo è maggiore del secondo
cat("Il primo risultato è maggiore del secondo in", conteggio, "su", num_prove, "prove.")

