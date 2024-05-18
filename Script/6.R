MonteCarloFixedLookbackCall <- function(sigma, S0, r, T, q, n, N, K) {
  # Parametri
  tau <- T / n
  S <- matrix(0, n + 1, N)
  
  # Simulazione dei percorsi del prezzo del titolo
  for (j in 1:N) {
    S[1, j] <- S0
    for (i in 1:n) {
      epsilon <- rnorm(1)
      S[i + 1, j] <- S[i, j] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon)
    }
  }
  
  # Calcolo dei payoff per ciascuna simulazione
  payoff <- numeric(N)
  for (j in 1:N) {
    max_price <- max(S[-1, j])
    payoff[j] <- max(0, max_price - K)
  }
  
  # Calcolo del valore stimato della Fixed Lookback Option Call
  MCFLC <- exp(-r * T) * mean(payoff)
  
  return(MCFLC)
}

# Impostazione dei parametri
sigma <- 0.25
S0 <- 90
r <- 0.05
T <- 0.5
q <- 0.03
n <- 180
N <- 10000
K <- 80.30  # Prezzo di esercizio

# Calcolo del valore stimato della Fixed Lookback Option Call
result <- MonteCarloFixedLookbackCall(sigma, S0, r, T, q, n, N, K)
cat("Il valore stimato della Fixed Lookback Option Call è:", result, "\n")











# Parametri della simulazione
S0 <- 2.70
K <- 2.65
H <- 2
r <- 0.03
T <- 1/12
sigma <- 0.43
q <- 0
n <- 30
tau <- T/n  # Prezzo di esercizio

# Funzione per la simulazione Monte Carlo
MonteCarloELP1 <- function(sigma, S0, r, T, q, n, N, K) {
  # Inizializzazione del vettore dei payoff
  payoffs <- numeric(N)
  
  # Simulazione Monte Carlo
  for (j in 1:N) {
    S <- numeric(n + 1)
    S[1] <- S0
    tau <- T / n
    
    for (i in 1:n) {
      epsilon <- rnorm(1)
      S[i + 1] <- S[i] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon)
    }
    
    # Calcolo del payoff della Extrema Lookback Option Put
    payoff <- max(K - min(S[-(1:n)]), 0)
    payoffs[j] <- exp(-r * T) * payoff
  }
  
  return(payoffs)
}

result <- MonteCarloFixedLookbackCall(sigma, S0, r, T, q, n, N, K)
cat("Il valore stimato della Fixed Lookback Option Call è:", result, "\n")

# Simulazione e calcolo dei payoff
set.seed(123)  # Imposta il seed per la riproducibilità
payoffs <- MonteCarloELP1(sigma, S0, r, T, q, n, N, K)

# Creazione del dataframe per il grafico
df <- data.frame(Simulazione = 1:N, Payoff = payoffs)

# Calcolo del valore medio dei payoff
valore_medio_payoff <- mean(payoffs)

# Creazione del grafico di dispersione (istogramma) con retta rossa del valore medio
library(ggplot2)
grafico_dispersione <- ggplot(df, aes(x = Simulazione, y = Payoff)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = valore_medio_payoff, color = "red", size = 1) +  # Retta rossa
  ylim(-0.25, 1) +  # Impostazione dell'asse y
  labs(x = "Simulazioni", y = "Risultati", title = "")

# Visualizza il grafico
print(grafico_dispersione)

# Distribuzione dei Payoff della Extrema Lookback Option Put




# ... (Codice precedente)

# Visualizza il grafico con larghezza personalizzata
library(ggplot2)
grafico_dispersione <- ggplot(df, aes(x = Simulazione, y = Payoff)) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = valore_medio_payoff, color = "red", size = 1) +  # Retta rossa
  ylim(-0.25, 0.75) +  # Impostazione dell'asse y
  ggtitle("Distribuzione dei Payoff della Extrema Lookback Option Put") +
  xlab("Simulazioni") +
  ylab("Payoff") +
  theme_minimal()

# Imposta la larghezza del grafico
width_in_inches <- 10
height_in_inches <- 6
ggsave(filename = "grafico_dispersione.png", plot = grafico_dispersione, width = width_in_inches, height = height_in_inches)

# Visualizza il grafico
print(grafico_dispersione)





# Installazione del pacchetto ggplot2 se non è già installato
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Caricamento del pacchetto ggplot2
library(ggplot2)

# Definizione dei parametri








library(ggplot2)

library(ggplot2)

# Definizione dei parametri
S0 <- 90         # Prezzo iniziale dell'attività sottostante
K <- 80.30       # Strike price
r <- 0.03        # Tasso privo di rischio
T <- 0.5         # Tempo residuo alla scadenza
sigma <- 0.25    # Volatilità
q <- 0           # Dividend yield (0 per un'opzione europea)
N <- 10000  
n <- 180 # Numero di simulazioni Monte Carlo

# Simulazione della Fixed Lookback Option Call
MonteCarloFixedLookbackCall <- function(sigma, S0, r, T, q, n, N, K) {
  tau <- T / n
  S <- matrix(0, n + 1, N)
  
  for (j in 1:N) {
    S[1, j] <- S0
    for (i in 1:n) {
      epsilon <- rnorm(1)
      S[i + 1, j] <- S[i, j] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon)
    }
  }
  
  payoff <- numeric(N)
  for (j in 1:N) {
    max_price <- max(S[, j])
    payoff[j] <- max(0, max_price - K)
  }
  
  MCFLC <- exp(-r * T) * mean(payoff)
  
  return(MCFLC)
}

# Calcolo dei payoff
payoffs <- numeric(N)
for (j in 1:N) {
  max_price <- max(S[, j])
  payoffs[j] <- max(0, max_price - K)
}

# Creazione del dataframe dei risultati
df <- data.frame(Simulazione = 1:N, Payoff = payoffs)

# Creazione del grafico
grafico_payoff <- ggplot(df, aes(x = Simulazione, y = Payoff)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = mean(payoffs), color = "red") +
  labs(x = "Simulazioni", y = "Payoff", title = "Grafico dei Payoff") +
  ylim(-0.25, 0.75) +
  theme_minimal() +
  theme(plot.width = unit(10, "cm"), plot.height = unit(6, "cm"))

# Visualizza il grafico
print(grafico_payoff)





library(ggplot2)

# Definizione dei parametri
# Impostazione dei parametri
S0 <- 90         # Prezzo iniziale dell'attività sottostante
K <- 80.30       # Strike price
r <- 0.03        # Tasso privo di rischio
T <- 0.5         # Tempo residuo alla scadenza
sigma <- 0.25    # Volatilità
q <- 0           # Dividend yield (0 per un'opzione europea)
N <- 10000       # Numero di simulazioni
n <- 180         # Numero di passi nel percorso

# Simulazione della Fixed Lookback Option Call
MonteCarloFixedLookbackCall <- function(sigma, S0, r, T, q, n, N, K) {
  # Parametri
  tau <- T / n
  S <- matrix(0, n + 1, N)
  
  # Simulazione dei percorsi del prezzo del titolo
  for (j in 1:N) {
    S[1, j] <- S0
    for (i in 1:n) {
      epsilon <- rnorm(1)
      S[i + 1, j] <- S[i, j] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon)
    }
  }
  
  # Calcolo dei payoff per ciascuna simulazione
  payoff <- numeric(N)
  for (j in 1:N) {
    max_price <- max(S[, j])
    payoff[j] <- max(0, max_price - K)
  }
  
  # Calcolo del valore stimato della Fixed Lookback Option Call
  MCFLC <- exp(-r * T) * mean(payoff)
  
  return(MCFLC)
}



# Calcola il valore della Fixed Lookback Option Call
MCFLC <- MonteCarloFixedLookbackCall(sigma, S0, r, T, q, n, N, K)

# Stampa il risultato
cat("Il valore stimato della Fixed Lookback Option Call è:", MCFLC, "\n")

# Calcolo dei payoff
S <- matrix(0, n + 1, N)
for (j in 1:N) {
  S[1, j] <- S0
  for (i in 1:n) {
    epsilon <- rnorm(1)
    S[i + 1, j] <- S[i, j] * exp((r - sigma^2 / 2 - q) * (T / n) + sqrt(T / n) * sigma * epsilon)
  }
}

payoffs <- numeric(N)
for (j in 1:N) {
  max_price <- max(S[, j])
  payoffs[j] <- max(0, max_price - K)
}

# Creazione del dataframe dei risultati
df <- data.frame(Simulazione = 1:N, Payoff = payoffs)


# Creazione del grafico
library(ggplot2)
grafico_payoff <- ggplot(df, aes(x = Simulazione, y = Payoff)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = mean(payoffs), color = "red") +
  labs(x = "Simulazioni", y = "Valori", title = "") 

# Visualizza il grafico
print(grafico_payoff)



