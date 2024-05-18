# Installa e carica la libreria extraDistr
install.packages("extraDistr")
library(extraDistr)

# Definizione dei parametri
mu <- 0  # Media (per la distribuzione normale standard)
sigma <- 2  # Deviazione standard (per la distribuzione normale standard)
LSL <- 1.5  # Limite di specificazione inferiore
USL <- 1.5+8.04  # Limite di specificazione superiore

# Calcolo delle probabilità usando la distribuzione normale standard
prob_Cp_0.67 <- 2 * pnorm(LSL, mean = mu, sd = sigma)  # Cp = 0.67
prob_Cp_1 <- 2 * pnorm(-USL, mean = mu, sd = sigma)  # Cp = 1
prob_Cp_2 <- 2 * pnorm(-USL, mean = mu, sd = sigma)  # Cp = 2

# Stampa dei risultati
cat("Probabilità di non conformità per Cp = 0.67:", prob_Cp_0.67, "\n")
cat("Probabilità di non conformità per Cp = 1:", 1 - prob_Cp_1, "\n")
cat("Probabilità di non conformità per Cp = 2:", 1 - prob_Cp_2, "\n")

