library(ggplot2)
library(gridExtra)
library(dplyr)
library(pracma)

# Serie storica Gas Naturale

gas <- read.csv("Gasnaturale.csv", header=T, dec=",")
gas

map_abbreviations <- function(text) {
  italian_abbreviations <- c("gen", "feb", "mar", "apr", "mag", "giu", "lug", "ago", "set", "ott", "nov", "dic")
  english_abbreviations <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  for (i in 1:length(italian_abbreviations)) {
    text <- gsub(paste0("\\b", italian_abbreviations[i], "\\b"), english_abbreviations[i], text)
  }
  return(text)
}

gas$Data <- map_abbreviations(gas$Data)

# Trasforma la variabile "Data" in formato Data (con mese in inglese)
gas$Data <- as.Date(gas$Data, format = "%d %b %Y", locale = "C")

str(gas)


# Filtra il DataFrame per le date desiderate
gas_filtrato <- gas %>%
  filter(Data >= as.Date("2023-03-15") & Data <= as.Date("2023-09-15"))

# Calcola i rendimenti giornalieri e crea la colonna Rendimenti_giornalieri
gas <- gas %>%
  mutate(Rendimenti_giornalieri = (Chiusura.aggiustata.. - lag(Chiusura.aggiustata..))/lag(Chiusura.aggiustata..))

# Calcola la volatilità annua in percentuale
volatilita_annua_percentuale <- sqrt(252) * sd(gas$Rendimenti_giornalieri, na.rm = TRUE) 


# Creo il primo grafico 
grafico1 <- ggplot(data = gas_filtrato, aes(x = Data, y = Chiusura.)) +
  geom_line(color = "blue") +
  labs(x = "", y = NULL, title = "") +
  theme_minimal() 
  

# Creo il secondo grafico 
grafico2 <- ggplot(data = gas_filtrato, aes(x = Data, y = Chiusura.)) +
  geom_line(color = "red") +
  labs(x = "", y = NULL, title = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_blank(), # Nasconde l'asse y di sinistra
        axis.text.y = element_blank())  # Nasconde i tick sull'asse y di sinistra

# Combino i due grafici

grid.arrange(grafico1, grafico2, ncol = 1, heights = c(3, 1))


# Calcolo dell'opzione Barriera put

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



# Metodo Monte Carlo

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



# Sentiero dei prezzi

# Simula i sentieri dei prezzi S per MCDaiOp
S_simulated_daiop <- simulatePaths(2.75, 0.5, 0.03, 0.38, 10000, 0)

# Estrai i valori di S(i+1) generati da MCDaiOp
S_i1_daiop <- as.vector(S_simulated_daiop[, 2:181])

# Crea un dataframe per i dati dei valori di S(i+1) di MCDaiOp
df_values_daiop <- data.frame(S_i1 = S_i1_daiop)

# Crea il grafico dei valori di S(i+1) per MCDaiOp

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

