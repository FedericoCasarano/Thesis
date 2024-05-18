library(ggplot2)


brent = read.csv("futurepg.csv", header=T)
pg

library(dplyr)

brent = as.data.frame(pg)

brent <- pg %>%
  mutate(Ultimo = as.numeric(gsub(",", ".", gsub("\\.", "", Ultimo))),
         Apertura = as.numeric(gsub(",", ".", gsub("\\.", "", Apertura))))

media_p = mean(brent$Ultimo)
str(brent)


library(ggplot2)
library(gridExtra)


# Converto la colonna 'Date' in formato data
brent$Data <- as.Date(brent$Data)

# Creo il primo grafico con l'asse y a destra (da 65 a 100)
grafico1 <- ggplot(data = brent, aes(x = Data, y = Ultimo)) +
  geom_line(color = "blue") +
  labs(x = "", y = NULL, title = "") +
  theme_minimal() +
  scale_y_continuous(
    limits = c(65, 100),
    breaks = seq(65, 100, by = 10),
    sec.axis = sec_axis(~., name = "", breaks = seq(65, 100, by = 10))
  )

# Creo il secondo grafico con l'asse y a destra (da 65 a 100) e senza l'asse y di sinistra
grafico2 <- ggplot(data = brent, aes(x = Data, y = Ultimo)) +
  geom_line(color = "red") +
  labs(x = "", y = NULL, title = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_blank(), # Nasconde l'asse y di sinistra
        axis.text.y = element_blank())  # Nasconde i tick sull'asse y di sinistra

# Combino i due grafici uno sopra l'altro

grid.arrange(grafico1, grafico2, ncol = 1, heights = c(3, 1))





########## LOOOOOOOOOkBACK 
MonteCarloELC <- function(sigma, S0, r, T, q, n, N) {
  # Parametri
  S0 <- 90
  r <- 0.03
  T <- 0.5
  sigma <- 0.25
  q <- 0
  N <- 10000
  n <- 180
  tau <- T / n
  S <- matrix(0, nrow = N, ncol = n + 1)
  S[, 1] <- S0
  payoff <- numeric(N)
  
  for (i in 1:N) {
    for (j in 1:n) {
      epsilon <- rnorm(1)
      S[i, j + 1] <- S[i, j] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon)
    }
  }
  
  for (i in 1:N) {
    payoff[i] <- max(S[i, ]) - S[i, 1]  # Payoff dell'extrema lookback call
  }
  
  MCDELC <- exp(-r * T) * mean(payoff)
  return(MCDELC)
}

# Esempio di utilizzo
result <- MonteCarloELC(0.25, 90, 0.03, 0.5, 0, 180, 10000)
print(result)









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
library(ggplot2)
ggplot(df_values, aes(x = S_i1)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(x = "Valori di S(i+1)", y = "Frequenza", title = "Distribuzione dei Valori di S(i+1)") +
  theme_minimal()

# Calcola il prezzo dell'opzione asiatica utilizzando la funzione Monte Carlo
result2<-MCAsianCall(S0, K, T, r, sigma, N, q)











# Crea un dataframe per il grafico di dispersione
df2 <- data.frame(Simulazioni = 1:N, Risultati = replicate(N, MCAsianCall(S0, K, T, r, sigma, 1, q)))

# Grafico di dispersione con retta rossa sul valore medio
scatter_plot <- ggplot(data = df2, aes(x = Simulazioni, y = Risultati)) +
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




# Creazione del dataframe per il grafico
df <- data.frame(
  Method = rep(c("Black-Scholes", "Monte Carlo"), each = N),
  OptionPrice = c(rep(AsianCall,N),result2)
)

# Creazione del grafico
ggplot(data = df, aes(x = Method, y = OptionPrice, fill = Method)) +
  geom_boxplot() +
  labs(
    title = "Confronto tra Black-Scholes e Monte Carlo - Opzione Asiatica",
    x = "Metodo",
    y = "Prezzo dell'Opzione Asiatica"
  )















# Codice simulazione Monte Carlo
# Parametri dell'opzione asiatica
S0 <- 90         # Prezzo iniziale dell'attività sottostante
K <- 80.30       # Strike price
r <- 0.03        # Tasso privo di rischio
T <- 0.5         # Tempo residuo alla scadenza
sigma <- 0.25    # Volatilità
q <- 0           # Dividend yield (0 per un'opzione europea)
N <- 10000       # Numero di simulazioni Monte Carlo

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
  
  AC <- exp(-r * T) * (exp(mu_log + V/2) * pnorm(d) -
                         Y * pnorm(d - sqrt(V)))
  return(AC)
}
phi <- function(x) {
  pi <- 0.5 * erfc(-x / sqrt(2))
  return(pi)
}
##########
##########
##########
##########
##########

# Imposta i parametri dell'opzione
S0 <- 90         # Prezzo iniziale dell'attività sottostante
r <- 0.03        # Tasso privo di rischio
T <- 0.5         # Tempo residuo alla scadenza
sigma <- 0.25    # Volatilità
q <- 0           # Dividend yield (0 per un'opzione europea)
N <- 10000       # Numero di simulazioni Monte Carlo
n <- 180         # Numero di istanti di rilevazione dei prezzi

# Crea un vettore di prezzi di esercizio K
K_values <- seq(80, 100, by = 2)  # Da 80 a 100 con passo 2

# Funzione per il calcolo dell'Asian Option attraverso il modello di Vorst
AC <- function(sigma, S0, K, r, T, q, n) {
  # (Incolla qui la funzione AC dal codice precedente)
  
  AC <- AC(sigma, S0, K, r, T, q, n)
  return(AC)
}

# Esegui la simulazione Monte Carlo e calcola i valori delle Asian Option con entrambi i metodi
MC_values <- numeric(length(K_values))
Vorst_values <- numeric(length(K_values))

for (i in 1:length(K_values)) {
  K <- K_values[i]
  
  MC_values[i] <- MCAsianCall(S0, K, T, r, sigma, N, q)
  Vorst_values[i] <- AC(sigma, S0, K, r, T, q, n)
}

# Crea un dataframe per il grafico
df <- data.frame(
  K = K_values,
  MonteCarlo = MC_values,
  Vorst = Vorst_values
)

# Crea un grafico di confronto tra i due metodi
library(ggplot2)

ggplot(df, aes(x = K)) +
  geom_line(aes(y = MonteCarlo, color = "Monte Carlo")) +
  geom_line(aes(y = Vorst, color = "Vorst")) +
  labs(
    title = "Confronto tra Monte Carlo e Modello di Vorst - Asian Option",
    x = "Prezzo di Esercizio (K)",
    y = "Valore dell'Asian Option"
  ) +
  scale_color_manual(values = c("Monte Carlo" = "blue", "Vorst" = "red")) +
  theme_minimal()

# Crea un secondo grafico per il valore dell'opzione al variare di K
ggplot(df, aes(x = K, y = MonteCarlo)) +
  geom_line(color = "blue") +
  labs(
    title = "Valore dell'Asian Option al variare di K - Monte Carlo",
    x = "Prezzo di Esercizio (K)",
    y = "Valore dell'Asian Option"
  ) +
  theme_minimal()

# Imposta i parametri dell'opzione
S0 <- 90         # Prezzo iniziale dell'attività sottostante
r <- 0.03        # Tasso privo di rischio
T <- 0.5         # Tempo residuo alla scadenza
sigma <- 0.25    # Volatilità
q <- 0           # Dividend yield (0 per un'opzione europea)
N <- 10000       # Numero di simulazioni Monte Carlo
n <- 180         # Numero di istanti di rilevazione dei prezzi

# Crea un vettore di prezzi di esercizio K
K_values <- seq(80, 100, by = 2)  # Da 80 a 100 con passo 2

# Funzione per il calcolo dell'Asian Option attraverso il modello di Vorst
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
  
  AC <- exp(-r * T) * (exp(mu_log + V/2) * pnorm(d) -
                         Y * pnorm(d - sqrt(V)))
  return(AC)
}

# Crea una matrice per i risultati
results <- matrix(0, nrow = length(K_values), ncol = 2)
colnames(results) <- c("MonteCarlo", "Vorst")

# Esegui la simulazione Monte Carlo e calcola i valori delle Asian Option con entrambi i metodi
for (i in 1:length(K_values)) {
  K <- K_values[i]
  MC_values <- numeric(N)
  Vorst_values <- numeric(N)
  
  for (j in 1:N) {
    MC_values[j] <- MCAsianCall(S0, K, T, r, sigma, 1, q)
    Vorst_values[j] <- AC(sigma, S0, K, r, T, q, n)
  }
  
  results[i, 1] <- mean(MC_values)
  results[i, 2] <- mean(Vorst_values)
}

# Crea un dataframe per il grafico
df <- data.frame(
  K = K_values,
  MonteCarlo = results[, 1],
  Vorst = results[, 2]
)

# (Continua con la creazione dei grafici come nell'esempio precedente)

######################## GAS


gas <- read.csv("Gasnaturale.csv", header=T, dec=",")
gas


# Supponendo che 'gas' sia il tuo datafram


# Funzione per mappare le abbreviazioni italiane in inglese
map_abbreviations <- function(text) {
  italian_abbreviations <- c("gen", "feb", "mar", "apr", "mag", "giu", "lug", "ago", "set", "ott", "nov", "dic")
  english_abbreviations <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for (i in 1:length(italian_abbreviations)) {
    text <- gsub(paste0("\\b", italian_abbreviations[i], "\\b"), english_abbreviations[i], text)
  }
  return(text)
}

# Mappa le abbreviazioni italiane in inglese nella colonna "Data"
gas$Data <- map_abbreviations(gas$Data)

# Trasforma la variabile "Data" in formato Data (con mese in inglese)
gas$Data <- as.Date(gas$Data, format = "%d %b %Y", locale = "C")



str(gas)

library(dplyr)

# Filtra il DataFrame per le date desiderate
gas_filtrato <- gas %>%
  filter(Data >= as.Date("2023-03-15") & Data <= as.Date("2023-09-15"))

# Carica le librerie ggplot2 e gridExtra se non sono già state caricate
# install.packages("ggplot2")
# install.packages("gridExtra")
library(ggplot2)
library(gridExtra)

# Creo il primo grafico con l'asse y a destra (da 65 a 100)
grafico1 <- ggplot(data = gas_filtrato, aes(x = Data, y = Chiusura.)) +
  geom_line(color = "blue") +
  labs(x = "", y = NULL, title = "") +
  theme_minimal() 


# Creo il secondo grafico con l'asse y a destra (da 65 a 100) e senza l'asse y di sinistra
grafico2 <- ggplot(data = gas_filtrato, aes(x = Data, y = Chiusura.)) +
  geom_line(color = "red") +
  labs(x = "", y = NULL, title = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_blank(), # Nasconde l'asse y di sinistra
        axis.text.y = element_blank())  # Nasconde i tick sull'asse y di sinistra

# Combino i due grafici uno sopra l'altro

grid.arrange(grafico1, grafico2, ncol = 1, heights = c(3, 1))


library(pracma)


DownAndInPut <- function(sigma, S0, K, r, T, q, n, H, R) {
  S0 <- 2.70
  K <- 2.65
  H <- 2
  R <- 0.05
  r <- 0.03
  T <- 1/12
  sigma <- 0.43
  q <- 0
  n <- 30
  tau <- T/n
  eta <- r/(sigma*sigma) - 1/2
  gamma <- sqrt(eta*eta + 2*r/(sigma*sigma))
  
  a1 <- (log(S0/X)/(sigma*sqrt(T))) + (1+eta)*sigma*sqrt(T)
  a2 <- a1 - sigma*sqrt(T)
  b1 <- (log(H*H/(S0*X))/(sigma*sqrt(T))) + (1+eta)*sigma*sqrt(T)
  b2 <- b1 - sigma*sqrt(T)
  c1 <- (log(H/S0)/(sigma*sqrt(T))) + gamma*sigma*sqrt(T)
  c2 <- c1 - 2*gamma*sigma*sqrt(T)
  
  DAIOP <- (K * exp(-r * T) * phi(-a2) - S0 * phi(-a1)) +
    (K * exp(-r * T) * ((H / S0)^(2 * eta)) * phi(-b2) - S0 * ((H / S0)^(2 * (eta + 1))) * phi(-b1)) +
    (R * phi(c2) - R * ((H / S0)^(eta + gamma)) * phi(c1))
  
  
  return(DAIOP)
}

phi <- function(x) {
  return(0.5*erfc(-x/sqrt(2)))
}

# Esempio di utilizzo
result <- DownAndInPut(0.38, 2.70, 2.65, 0.03, 0.5, 0, 180, 2, 0.05)
print(result)





MCDaiOp <- function(S0, K, T, r, sigma, N, q, H, R) {
  # Parametri
  tau <- T / N  # Periodo di tempo tra le rilevazioni dei prezzi
  S <- numeric(N + 1)
  S[1] <- S0
  activated <- FALSE
  
  for (i in 1:N) {
    epsilon <- rnorm(1)  # Campionamento da una distribuzione normale
    S[i + 1] <- S[i] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon)
    
    if (max(S) < H) {
      activated <- TRUE
      break  # L'opzione è stata attivata, usciamo dal ciclo
    }
  }
  
  if (activated) {
    payoff <- max(K - S[N + 1], 0)  # Calcolo del payoff in caso di attivazione
  } else {
    payoff <- R  # Nessuna attivazione, payoff pari al rebate
  }
  
  MCDaiOp <- exp(-r * T) * payoff
  return(MCDaiOp)
}


# Esempio di utilizzo
result <- MCDaiOp(2.70, 2.65, 0.5, 0.03, 0.83, 10000, 0, 2, 0.05)
print(result)





# Simula i sentieri dei prezzi S per MCDaiOp
S_simulated_daiop <- simulatePaths(2.75, 0.5, 0.03, 0.38, 10000, 0)

# Estrai i valori di S(i+1) generati da MCDaiOp
S_i1_daiop <- as.vector(S_simulated_daiop[, 2:181])

# Crea un dataframe per i dati dei valori di S(i+1) di MCDaiOp
df_values_daiop <- data.frame(S_i1 = S_i1_daiop)

# Crea il grafico dei valori di S(i+1) per MCDaiOp
library(ggplot2)
ggplot(df_values_daiop, aes(x = S_i1)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "black") +
  labs(x = "Valori di S(i+1)", y = "Frequenza", title = "Distribuzione dei Valori di S(i+1) per MCDaiOp") +
  theme_minimal()

# Calcola il prezzo dell'opzione Down-and-In utilizzando la funzione Monte Carlo
MCDaiOp(2.75, 2.70, 0.5, 0.03, 0.38, 10000, 0, 2, 0.05)

# Crea un dataframe per il grafico di dispersione
df2 <- data.frame(Simulazioni = 1:10000, Risultati = replicate(10000, MCDaiOp(2.75, 2.70, 0.5, 0.03, 0.38, 1, 0, 2, 0.05)))

scatter_plot <- ggplot(data = df2, aes(x = Simulazioni, y = Risultati)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = mean(df2$Risultati), color = "red") +
  labs(x = "Simulazioni", y = "Risultati", title = "Grafico di Dispersione per MCDaiOp") +
  ylim(0.05, 0.047)  # Imposta il range della scala y tra 0.04 e 0.06


# Mostra il grafico di dispersione
print(scatter_plot)

############

gas <- read.csv("Gasnaturale.csv", header=T, dec=",")
gas


# Supponendo che 'gas' sia il tuo datafram


# Funzione per mappare le abbreviazioni italiane in inglese
map_abbreviations <- function(text) {
  italian_abbreviations <- c("gen", "feb", "mar", "apr", "mag", "giu", "lug", "ago", "set", "ott", "nov", "dic")
  english_abbreviations <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for (i in 1:length(italian_abbreviations)) {
    text <- gsub(paste0("\\b", italian_abbreviations[i], "\\b"), english_abbreviations[i], text)
  }
  return(text)
}

# Mappa le abbreviazioni italiane in inglese nella colonna "Data"
gas$Data <- map_abbreviations(gas$Data)

# Trasforma la variabile "Data" in formato Data (con mese in inglese)
gas$Data <- as.Date(gas$Data, format = "%d %b %Y", locale = "C")



str(gas)

# Carica la libreria dplyr
library(dplyr)

# Calcola i rendimenti giornalieri e crea la colonna Rendimenti_giornalieri
gas <- gas %>%
  mutate(Rendimenti_giornalieri = (Chiusura.aggiustata.. - lag(Chiusura.aggiustata..))/lag(Chiusura.aggiustata..))

# Calcola la volatilità annua in percentuale
volatilita_annua_percentuale <- sqrt(252) * sd(gas$Rendimenti_giornalieri, na.rm = TRUE) 







MCDownAndOutC <- function(S0, K, T, r, sigma, N, q, H) {
  S0 <- 2.70
  K <- 2.75
  H <- 2
  r <- 0.03
  T <- 1/12
  sigma <- 0.84
  N <- 10000
  q <- 0
  tau <- T / N
  S <- numeric(N + 1)
  S[1] <- S0
  P <- numeric(N)
  
  for (j in 1:N) {
    for (i in 1:N) {
      epsilon <- rnorm(1)
      S[i + 1] <- S[i] * exp((r - sigma^2 - q) * tau + sqrt(tau) * sigma * epsilon)
    }
    
    if (max(S) > H) {
      P[j] <- max(K - S[N], 0)
    } else {
      P[j] <- 0
    }
  }
  
  MCDownAndOutC <- exp(-r * T) * mean(P)
  return(MCDownAndOutC)
}


# Esempio di utilizzo
result <- MCDownAndOutC(2.70, 2.65, 0.5, 0.03, 0.83, 10000, 0, 2)
print(result)


# Crea un dataframe per il grafico di dispersione
df3 <- data.frame(Simulazioni = 1:N, Risultati = replicate(N, MCDownAndOutC(S0, K, T, r, sigma, N, q, H)))

# Grafico di dispersione con retta rossa sul valore medio
scatter_plot <- ggplot(data = df, aes(x = Simulazioni, y = Risultati)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_hline(yintercept = risultato, color = "red") +
  labs(x = "Simulazioni", y = "Risultati", title = "")

# Mostra il grafico
print(scatter_plot)


MCDownAndOutC <- function(S0, K, T, r, sigma, N, q, H) {
  # Parametri
  tau <- T / N
  S <- matrix(0, nrow = N + 1, ncol = N + 1)
  S[1, ] <- S0
  
  for (j in 1:N) {
    epsilon <- rnorm(N)
    for (i in 1:(N + 1)) {
      S[i, j + 1] <- S[i, j] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon[i])
    }
  }
  
  P <- numeric(N)
  
  for (j in 1:N) {
    if (!any(is.na(S[, j + 1])) && max(S[, j + 1]) > H) {
      P[j] <- max(K - S[N + 1, j + 1], 0)
    } else {
      P[j] <- 1
    }
  }
  
  MCDownAndOutC <- exp(-r * T) * mean(P)
  return(MCDownAndOutC)
}




# Esempio di utilizzo
result <- MCDownAndOutC(2.70, 2.65, 0.5, 0.03, 0.83, 10000, 0, 2)
print(result)

#########



MCDownAndOutC <- function(S0, K, T, r, sigma, N, q, H) {
  tau <- T / N
  S <- numeric(N + 1)
  S[1] <- S0
  P <- numeric(N)
  
  for (j in 1:N) {
    for (i in 1:N) {
      epsilon <- rnorm(1)
      S[i + 1] <- S[i] * exp((r - sigma^2 - q) * T + sqrt(T) * sigma * epsilon)
    }
    
    if (max(S) > H) {
      P[j] <- max(K - S[N + 1], 0)
    }
  }
  
  MCDownAndOutC <- exp(-r * T) * mean(P)
  return(MCDownAndOutC)
}

# Esempio di utilizzo
result <- MCDownAndOutC(2.7, 2.65, 0.0833333, 0.03, 0.23, 10000, 0, 2)
print(result)


###############à LOOOOOOOOOkBACK
MonteCarloFLP <- function(sigma, S0, r, T, q, n, M) {
  # Parametri
  S0 <- 2.7
  r <- 0.03
  T <- 1/12
  sigma <- 0.25
  q <- 0
  M <- 10000
  n <- 30
  dt <- T / n
  S <- matrix(0, nrow = M, ncol = n + 1)
  S[, 1] <- S0
  payoff <- numeric(M)
  
  for (i in 1:M) {
    for (j in 1:n) {
      Y <- rnorm(1)
      S[i, j + 1] <- S[i, j] * exp((r - sigma^2 / 2 - q) * dt + sqrt(dt) * sigma * Y)
    }
  }
  
  for (i in 1:M) {
    payoff[i] <- max(S0 - min(S[i, ]), 0)  # Payoff dell'opzione floating lookback put
  }
  
  MCFLP <- exp(-r * T) * mean(payoff)
  return(MCFLP)
}

# Esempio di utilizzo
result <- MonteCarloFLP(0.25, 2.7, 0.03, 1/12, 0, 30, 10000)
print(result)





# Funzione per simulare una extrema lookback option put
MonteCarloELP1 <- function(sigma, S0, r, T, q, n, N) {
  S <- numeric(n + 1)
  S[1] <- S0
  tau <- T / n  # Cambio il nome da dt a tau
  
  # Simulazione del percorso del prezzo del titolo
  for (j in 1:N) {  # Cambio M in N
    for (i in 1:n) {
      epsilon <- rnorm(1)  # Cambio Y in epsilon
      S[i + 1] <- S[i] * exp((r - sigma^2 / 2 - q) * tau + sqrt(tau) * sigma * epsilon)  # Cambio Y in epsilon
    }
  }
  
  # Calcolo della extrema lookback option put
  payoff <- pmax(max(S) - S[n + 1], 0)
  MCDELP <- exp(-r * T) * mean(payoff)
  
  return(MCDELP)
}

# Impostazione dei parametri
S0 <- 42
r <- 0.03
T <- 0.5
sigma <- 0.38
q <- 0
N <- 10000  # Cambio M in N
n <- 180
tau <- T / n  # Cambio dt in tau

# Chiamata alla funzione per simulare una extrema lookback option put
result <- MonteCarloELP1(sigma, S0, r, T, q, n, N)
cat("Il valore stimato della extrema lookback option put è:", result, "\n")

result11 <- MonteCarloELP1(0.25, 2.7, 0.03, 1/12, 0, 30, 10000)
print(result11)











MonteCarloFloatingLookbackPut <- function(sigma, S0, r, T, q, n, N) {
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
    payoff[j] <- max(0, max_price - S[n + 1, j])
  }
  
  # Calcolo del valore stimato della put floating lookback option
  MCFLP <- exp(-r * T) * mean(payoff)
  
  return(MCFLP)
}

# Impostazione dei parametri
sigma <- 0.43
S0 <- 2.7
r <- 0.05
T <- 1/12
q <- 0.03
n <- 30
N <- 10000

# Calcolo del valore stimato della put floating lookback option
result <- MonteCarloFloatingLookbackPut(sigma, S0, r, T, q, n, N)
cat("Il valore stimato della put floating lookback option è:",result, "\n")










MonteCarloFloatingLookbackPut <- function(sigma, S0, r, T, q, n, N) {
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
    payoff[j] <- max(0, max_price - S[n + 1, j])
  }
  
  # Calcolo del valore stimato della put floating lookback option
  MCFLP <- exp(-r * T) * mean(payoff)
  
  return(MCFLP)
}

# Impostazione dei parametri
sigma <- 0.2
S0 <- 100
r <- 0.05
T <- 1
q <- 0.03
n <- 100
N <- 10000

# Calcolo del valore stimato della put floating lookback option
result <- MonteCarloFloatingLookbackPut(sigma, S0, r, T, q, n, N)
cat("Il valore stimato della put floating lookback option è:", result, "\n")










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
K <-   # Prezzo di esercizio
  
  # Calcolo del valore stimato della Fixed Lookback Option Call
  result <- MonteCarloFixedLookbackCall(sigma, S0, r, T, q, n, N, K)
cat("Il valore stimato della Fixed Lookback Option Call è:", result, "\n")