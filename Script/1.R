# Carica le librerie necessarie
library(stats)
library(ggplot2)

# Parametri
S0 <- 50          # Prezzo iniziale dell'elettricità
r <- 0.03         # Tasso di interesse senza rischio
sigma <- 0.2      # Volatilità del prezzo dell'elettricità
T <- 1            # Tempo di maturità dell'opzione (anni)
K <- 55           # Prezzo di esercizio dell'opzione
N <- 10000        # Numero di simulazioni Monte Carlo

# Numero di passi temporali per la discretizzazione
n <- 252          # Ad esempio, 252 giorni lavorativi in un anno

# Funzione per la simulazione di un percorso di prezzi dell'elettricità
simulate_path <- function(S0, r, sigma, T, n) {
  dt <- T / n
  t <- seq(0, T, by = dt)
  dWt <- rnorm(n) * sqrt(dt)
  Wt <- cumsum(dWt)
  St <- S0 * exp((r - 0.5 * sigma^2) * t + sigma * Wt)
  return(St)
}

# Simulazione Monte Carlo per valutare l'opzione lookback
set.seed(123)  # Imposta il seed per la riproducibilità
payoffs <- numeric(N)

for (i in 1:N) {
  # Simula un percorso di prezzi dell'elettricità
  St <- simulate_path(S0, r, sigma, T, n)
  
  # Calcola il payoff dell'opzione lookback (tipo call)
  payoff <- max(max(St) - K, 0)
  
  payoffs[i] <- payoff
}

# Calcola il valore attuale medio del payoff
option_value <- exp(-r * T) * mean(payoffs)

# Stampa il risultato
cat("Il valore stimato dell'opzione lookback senza rebate è:", option_value, "\n")

# Creazione del grafico dell'istogramma dei payoff
hist_plot <- ggplot(data = data.frame(Payoff = payoffs), aes(x = Payoff)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(x = "Payoff dell'opzione", y = "Frequenza", title = "Istogramma dei Payoff")

# Mostra il grafico dell'istogramma
print(hist_plot)

# Creazione del grafico della densità dei payoff
density_plot <- ggplot(data = data.frame(Payoff = payoffs), aes(x = Payoff)) +
  geom_density(fill = "blue", color = "black") +
  labs(x = "Payoff dell'opzione", y = "Densità", title = "Densità dei Payoff")

# Mostra il grafico della densità
print(density_plot)




# Imposta il seme casuale per la riproducibilità
set.seed(123)

# Genera 10,000 valori casuali con media 0.049
n <- 10000
valori <- rnorm(n, mean = 0.049, sd = 0.01)  # Imposta la deviazione standard a tuo piacimento

# Crea un dataframe
df <- data.frame(Rilevazione = 1:n, Valore = valori)

# Calcola il valore medio
media <- mean(valori)

# Crea il grafico di dispersione
library(ggplot2)
scatter_plot <- ggplot(data = df, aes(x = Rilevazione, y = Valore)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = media, color = "red") +
  labs(x = "Simulazioni", y = "Risultati", title = "")

# Mostra il grafico
print(scatter_plot)









