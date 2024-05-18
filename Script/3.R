MCDownAndInPut <- function(S0, K, T, r, sigma, N, q, n, H) {
  # Parametri
  tau <- T / n
  S <- numeric(n + 1)
  S[1] <- S0
  P <- numeric(N)
  
  for (j in 1:N) {
    activated <- FALSE
    
    for (i in 1:n) {
      epsilon <- rnorm(1)
      S[i + 1] <- S[i] * exp((r - sigma^2/2 - q) * tau + sigma * sqrt(tau) * epsilon)
      
      # Verifica se l'opzione è stata attivata
      if (min(S) < H) {
        activated <- TRUE
        break
      }
    }
    
    # Calcolo del payoff
    if (activated) {
      payoff <- max(K - S[n + 1], 0)
    } else {
      payoff <- 0
    }
    
    P[j] <- exp(-r * T) * payoff
  }
  
  # Calcolo del prezzo dell'opzione
  MCDownAndInPut <- mean(P)
  return(MCDownAndInPut)
}

# Imposta un seed diverso per ottenere risultati diversi
set.seed(123)  # Puoi cambiare il numero seed a tuo piacimento

# Esempio di utilizzo
S0 <- 2.7
K <- 2.5
T <- 0.08
r <- 0.03
sigma <- 0.25
N <- 10000
q <- 0
n <- 30
H <- 2

risultato <- MCDownAndInPut(S0, K, T, r, sigma, N, q, n, H)
print(risultato)

df2 <- data.frame(Simulazioni = 1:N, Risultati = replicate(N, MCDownAndInPut(S0, K, T, r, sigma, N, q, n, H)))
summary(df2)







# Installa e carica il pacchetto ggplot2 se non è già installato
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Definizione dei parametri
S0 <- 90         # Prezzo iniziale dell'attività sottostante
K <- 80.30       # Strike price
r <- 0.03        # Tasso privo di rischio
T <- 0.5         # Tempo residuo alla scadenza
sigma <- 0.25    # Volatilità
q <- 0           # Dividend yield (0 per un'opzione europea)
N <- 10000       # Numero di simulazioni Monte Carlo

# Funzione per il calcolo dell'opzione asiatica
MCAsianCall <- function(S0, K, T, r, sigma, N, q) {
  # (Incolla qui la funzione MCAsianCall dal tuo codice)
  
  # Resto del codice rimane invariato
  
  MCAsianCall <- mean(R)
  return(MCAsianCall)
}

# Esegui la simulazione Monte Carlo
set.seed(123)  # Imposta il seed per la riproducibilità
MC_result <- numeric(N)

for (i in 1:N) {
  MC_result[i] <- MCAsianCall(S0, K, T, r, sigma, N, q)
}

# Creazione del dataframe per il grafico
df <- data.frame(OptionPrice = MC_result)

# Creazione del grafico
ggplot(data = df, aes(x = OptionPrice)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(
    title = "Simulazione Monte Carlo - Opzione Asiatica",
    x = "Prezzo dell'Opzione Asiatica",
    y = "Frequenza"
  )
