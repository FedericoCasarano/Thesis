# Asian Option 

library(ggplot2)
library(gridExtra)
library(dplyr)

# Serie storica Petrolio Greggio 

pg = read.csv("futurepg.csv", header=T)
pg


pg = as.data.frame(pg)

pg <- pg %>%
  mutate(Ultimo = as.numeric(gsub(",", ".", gsub("\\.", "", Ultimo))),
         Apertura = as.numeric(gsub(",", ".", gsub("\\.", "", Apertura))))

media_p = mean(brent$close)
str(brent)

# Conversione della colonna 'Date' in formato data
brent$Date <- as.Date(brent$Date)

# Creazione del primo grafico
grafico1 <- ggplot(data = brent, aes(x = Date, y = close)) +
  geom_line(color = "blue") +
  labs(x = "", y = NULL, title = "") +
  theme_minimal() +
  scale_y_continuous(
    limits = c(65, 100),
    breaks = seq(65, 100, by = 10),
    sec.axis = sec_axis(~., name = "", breaks = seq(65, 100, by = 10))
  )

# Creazione del secondo grafico 
grafico2 <- ggplot(data = brent, aes(x = Date, y = close)) +
  geom_line(color = "red") +
  labs(x = "", y = NULL, title = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_blank(), # Nasconde l'asse y di sinistra
        axis.text.y = element_blank())  # Nasconde i tick sull'asse y di sinistra

# Combino i due grafici

grid.arrange(grafico1, grafico2, ncol = 1, heights = c(3, 1))



# Parametri dell'opzione asiatica
S0 <- 90         # Prezzo iniziale dell'attività sottostante
K <- 80.30       # Strike price
r <- 0.03        # Tasso privo di rischio
T <- 0.5         # Tempo residuo alla scadenza
sigma <- 0.25    # Volatilità
q <- 0           # Dividend yield (0 per un'opzione europea)
N <- 10000       # Numero di simulazioni Monte Carlo

# Definizione della funzione Monte Carlo per l'opzione asiatica
MCAsianCall <- function(S0, K, T, r, sigma, N, q) {
  tau <- T / 180  # Istanti di rilevazione dei prezzi
  S <- numeric(181)
  S[1] <- S0
  m <- numeric(N)
  R <- numeric(N)
  
  for (j in 1:N) {
    for (i in 1:180) {
      epsilon <- rnorm(1)
      S[i + 1] <- S[i] * exp((r - q - (sigma^2 / 2)) * tau + 
                               sigma * sqrt(tau) * epsilon)
    }
    m[j] <- exp(1 / 181 * sum(log(S)))
    R[j] <- exp(-r * T) * mean(pmax(S[181] - K, 0))
  }
  
  MCAsianCall <- mean(R)
  return(MCAsianCall)
}

# Simulazione dei sentieri dei prezzi S
set.seed(123)  # Per riproducibilità
N_simulations <- 10000  # Numero di simulazioni

simulatePaths <- function(S0, T, r, sigma, N, q) {
  tau <- T / 180  # Istanti di rilevazione dei prezzi
  S <- matrix(0, nrow = N, ncol = 181)
  S[, 1] <- S0
  
  for (j in 1:N) {
    for (i in 1:180) {
      epsilon <- rnorm(1)
      S[j, i + 1] <- S[j, i] * exp((r - q - (sigma^2 / 2)) * tau + 
                                     sigma * sqrt(tau) * epsilon)
    }
  }
  
  return(S)
}

# Simula i sentieri dei prezzi S
S_simulated <- simulatePaths(S0, T, r, sigma, N_simulations, q)

# Crea un dataframe per i dati dei sentieri dei prezzi S
df <- data.frame(time = rep(seq(0, T, length.out = 181), N_simulations), 
                 S_simulated = as.vector(S_simulated))

# Crea un dataframe per i dati dei valori di S(i+1)
df_values <- data.frame(S_i1 = as.vector(S_simulated[, 2:181]))

# Crea il grafico dei valori di S(i+1)
ggplot(df_values, aes(x = S_i1)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(x = "Valori di S(i+1)", y = "Frequenza", title = "Distribuzione dei Valori di S(i+1)") +
  theme_minimal()

# Calcola il prezzo dell'opzione asiatica utilizzando la funzione Monte Carlo
MCAsianCall(S0, K, T, r, sigma, N, q)











# Crea un dataframe per il grafico di dispersione
df2 <- data.frame(Simulazioni = 1:N, Risultati = replicate(N, MCAsianCall(S0, K, T, r, sigma, 1, q)))

# Grafico di dispersione con retta rossa sul valore medio
scatter_plot <- ggplot(data = df, aes(x = Simulazioni, y = Risultati)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = risultato, color = "red") +
  labs(x = "Simulazioni", y = "Risultati", title = "")

# Mostra il grafico
print(scatter_plot)



# Calcolo dell'Asian Option attraverso il modello di Vorst
AC <- function(sigma, S0, K, r, T, q, n) {
  tau <- T / n
  s <- 0
  
  for (i in 1:n) {
    s <- s + i
  }
  
  mu_log <- log(S0) + 1/n * (r - q - (sigma^2)/2) * s * tau
  sum <- 0
  
  for (i in 1:n) {
    for (j in 1:n) {
      sum <- sum + min(i * tau, j * tau)
    }
  }
  
  V <- (sigma^2) / (n^2) * sum
  somma <- 0
  
  for (i in 1:n) {
    somma <- somma + exp((r - q) * (i * tau))
  }
  
  Ea <- S0 * (somma) / (n)
  Eg <- exp(mu_log + V/2)
  Y <- K - (Ea - Eg)
  d <- (mu_log - log(Y) + V) / sqrt(V)
  
  AC <- exp(-r * T) * (exp(mu_log + V/2) * pnorm(d) - Y * pnorm(d - sqrt(V)))
  return(AC)
}

phi <- function(x) {
  pi <- 0.5 * erfc(-x / sqrt(2))
  return(pi)
}

# Parametri dell'opzione asiatica
S0 <- 90
X <- 80.40
r <- 0.03
T <- 0.5
sigma <- 0.25
q <- 0
n <- 180

# Calcola l'opzione asiatica
AsianCall <- AC(sigma, S0, X, r, T, q, n)
cat("Il valore dell'opzione asiatica è:", AsianCall)
