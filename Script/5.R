# Definizione dei parametri
St <- 72  # Prezzo corrente del titolo sottostante
Tau <- 0.5  # Periodo residuo tra la data di scadenza e la data corrente di valutazione
Sigma <- 0.25  # Volatilità del titolo sottostante
q <- 0.03  # Dividend yield
r <- 0.05  # Tasso privo di rischio
Smin <- 70  # Prezzo minimo registrato nel periodo di validità
Smax <- 95  # Prezzo massimo registrato nel periodo di validità
eta <- (r - q) / Sigma

# Calcolo di d1, d2 e d3
d1 <- (log(St * exp(-q * Tau) / (Smin * exp(-r * Tau))) + (Sigma^2 / 2) * Tau) / (Sigma * sqrt(Tau))
d2 <- (log(St * exp(-q * Tau) / (Smax * exp(-r * Tau))) + (Sigma^2 / 2) * Tau) / (Sigma * sqrt(Tau))
d3 <- (log(St * exp(-q * Tau) / (St * exp(-r * Tau))) + (Sigma^2 / 2) * Tau) / (Sigma * sqrt(Tau))

# Calcolo di C_Fx
C_Fx <- St * exp(-q * Tau) * pnorm(d3) - K * exp(-r * Tau) * pnorm(d3 - Sigma * sqrt(Tau)) +
  ((St * exp(-q * Tau) * pnorm(d3) - St * exp(-r * Tau) * (K / St)^(2 * eta) * pnorm(d3 - 2 * eta * Sigma * sqrt(Tau))) / (2 * eta))

# Stampare il risultato
cat("Il valore di C_Fx è:", C_Fx, "\n")





