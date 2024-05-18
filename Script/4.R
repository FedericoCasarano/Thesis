MCDAIP <- function(S0, X, T, r, sigma, M, q, H, n) {
  dt <- T / n
  S <- numeric(n + 1)
  S[1] <- S0
  P <- numeric(M)
  
  for (j in 1:M) {
    for (i in 1:n) {
      Y <- rnorm(1)
      S[i + 1] <- S[i] * exp((r - sigma * sigma - q) * dt + sqrt(dt) * sigma * Y)
    }
    
    if (min(S) > H) {
      P[j] <- max(X - S[n], 0)
    } else {
      P[j] <- 0
    }
  }
  
  MCDAIP <- exp(-r * T) * mean(P)
  return(MCDAIP)
}

# Impostazione dei parametri
S0 <- 2.7
X <- 2.65
H <- 2
R <- 10
r <- 0.03
T <- 0.08
sigma <- 0.43
M <- 10000
q <- 0
n <- 30  # Numero di rilevazioni del prezzo

# Calcolo il valore dell'opzione MCDAOC
result <- MCDAIP(S0, X, T, r, sigma, M, q, H, n)
cat("Il valore dell'opzione MCDAOC è:", result, "\n")


# Dichiarazione dei parametri
S0 <- 2.7
X <- 2.65
H <- 2
R <- 10
r <- 0.03
T <- 0.08
sigma <- 0.43
M <- 10000
q <- 0
n <- 30  # Numero di rilevazioni del prezzo

# Creazione di un vettore vuoto per i risultati di P
P <- numeric(M)

# Imposta il seed per la riproducibilità
set.seed(123)

# Calcola i valori di P utilizzando l'algoritmo MCDAIP
for (j in 1:M) {
  S <- numeric(n + 1)
  S[1] <- S0
  for (i in 1:n) {
    Y <- rnorm(1)
    S[i + 1] <- S[i] * exp((r - sigma * sigma - q) * T/n + sqrt(T/n) * sigma * Y)
  }
  
  if (min(S) > H) {
    P[j] <- max(X - S[n], 0)
  } else {
    P[j] <- 0
  }
}

# Crea un dataframe con i risultati di P
df <- data.frame(Simulazione = 1:M, Valore_P = P)

# Calcola il valore medio dei risultati di P
valore_medio_P <- mean(df$Valore_P)

# Crea il grafico di dispersione con i limiti dell'asse y e una retta rossa
library(ggplot2)

grafico_dispersione <- ggplot(df, aes(x = Simulazione, y = Valore_P)) +
  geom_point(color = "blue", alpha=0.5) +
  geom_hline(yintercept = valore_medio_P, color = "red") +  # Retta rossa
  ylim(-0.25, 1) +  # Limiti dell'asse y
  labs(x = "Simulazioni", y = "Risultati", title = "")


# Visualizza il grafico
print(grafico_dispersione)

scatter_plot <- ggplot(data = df2, aes(x = Simulazioni, y = Risultati)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = risultato, color = "red") +
  labs(x = "Simulazioni", y = "Risultati", title = "")








