# Install and load necessary packages
install.packages(c("forecast", "ggplot2"))
library(forecast)
library(ggplot2)
library(stats)

# Set seed for reproducibility
set.seed(123)

# ----------------------------------------

# ------- Exercise 1.4 ---------

# Defining AR(2) parameters
phi1 <- -0.7
phi2 <- -0.2
ar_params <- c(phi1, phi2)

# Number of lags
n_lag <- 30

# Compute theoretical autocorrelation function (ACF) using ARMA model
acf_values <- ARMAacf(ar = ar_params, lag.max = n_lag)

# Save plot
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer14.png", width = 900, height = 650)

# Plot the autocorrelation function
plot(0:n_lag, acf_values, type = "h", lwd = 2, col = "black",
     xlab = "Lag k", ylab = expression(rho(k)),
     main = "Autocorrelation Function of AR(2) Process")
points(0:n_lag, acf_values, pch = 16, col = "red")
grid()
dev.off()

# ------- Exercise 2 ---------

rm(list = ls())

# Function to plot time series, ACF, and PACF
plotit <- function(x) {
  layout(matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))  
  par(mar = c(3, 3, 5, 2), mgp = c(2, 0.7, 0), oma = c(0, 0, 0, 0))
  
  # Time series plot (spanning 2 columns)
  plot.ts(x, ylab = "X", main = "Simulated Time Series", col = "black", type = "l", cex.main = 1.5)
  
  # ACF plot
  acf(x, lag.max = 50, main = "Autocorrelation Function (ACF)", lwd = 2, cex.main = 1.5)
  
  # PACF plot
  pacf(x, lag.max = 50, main = "Partial Autocorrelation Function (PACF)", lwd = 2, cex.main = 1.5)
}

# Function to simulate an ARMA process
sim <- function(model, n, nburnin = 100) {
  n <- n + nburnin  # Extend series for burn-in period
  ar <- model$ar  # AR coefficients
  ma <- model$ma  # MA coefficients
  
  p <- length(ar)  # AR order
  q <- length(ma)  # MA order
  
  y <- numeric(n)  # Initialize time series
  eps <- rnorm(n)  # Generate white noise
  
  # Run the simulation
  for(i in (max(p,q)+1):n){
    y[i] <- eps[i] + sum(y[i-(1:p)] * ar) + sum(eps[i-(1:q)] * ma)
  }
  return(y[(nburnin + 1):n])  # Return series after burn-in
}

# ------- Exercise 2.1 ---------

# AR model with phi = 0.6
model_1 <- list(ar = c(0.6))
set.seed(4)

ts_data <- sim(model_1, 200, nburnin = 100) # Increased sample size for better visualization

# Plot results
plotit(ts_data)
mtext(expression('SARIMA Model (1,0,0) × (0,0,0)'[12] ~ phi ~ "= 0.6"), side = 3, outer = TRUE, line = -2, cex = 1.2)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer21.png", width = 900, height = 650)  # Open PNG device
plotit(ts_data)  # Re-run the plot function
dev.off()  # Close the device

# ------- Exercise 2.2 ---------

AR_2  = rep(0, 12)
AR_2[12] = -0.9
model_2 <- list(ar=AR_2)
set.seed(4)

ts_data2 <- sim(model_2, 200, nburnin = 100) # Increased sample size for better visualization
plotit(ts_data2)

mtext(expression('SARIMA Model (0,0,0) × (1,0,0)'[12] ~ phi ~ "= -0.9"), side = 3, outer = TRUE, line = -0.8)#, cex = 1.2)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer22.png", width = 900, height = 650)
plotit(ts_data2)  # Re-run the plot function
dev.off()  # Close the device

# ------- Exercise 2.3 ---------

AR_3 = c(0.9)
MA_3  = rep(0, 12)
MA_3[12] = -0.7
model_3 <- list(ar=AR_3,ma = MA_3)
set.seed(4)

ts_data3 <-sim(model_3, 200, nburnin = 100)
plotit(ts_data3)

mtext(expression('SARIMA Model (1,0,0) X (0,0,1)'[12] ~  phi ~  " = 0.9" ~ Theta ~" = -0.7" ), side = 3,outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer23.png", width = 900, height = 650)
plotit(ts_data3)  # Re-run the plot function
dev.off()  # Close the device

# ------- Exercise 2.4 ---------

AR_4 = rep(0, 12)
AR_4[1]=-0.6
AR_4[12] = -0.8
model_4 <- list(ar=AR_4)
set.seed(4)

ts_data4 <-sim(model_4, 200, nburnin = 100)
plotit(ts_data4)

mtext(expression('SARIMA Model (1,0,0) X (1,0,0)'[12] ~  phi ~  " = -0.6" ~ Phi ~" = -0.8" ),outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer24.png", width = 900, height = 650)
plotit(ts_data4)  # Re-run the plot function
dev.off()  # Close the device

# ------- Exercise 2.5 ---------

MA_5 = rep(0, 12)
MA_5[1]=0.4
MA_5[12] = -0.8
model_5 <- list(ma=MA_5)
set.seed(4)

ts_dat5 <-sim(model_5, 200, nburnin = 100)
plotit(ts_dat5)

mtext(expression('SARIMA Model (0,0,1) X (0,0,1)'[12] ~  theta ~  " = 0.4" ~ theta ~" = -0.8" ),outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer25.png", width = 900, height = 650)
plotit(ts_dat5)  # Re-run the plot function
dev.off()  # Close the device

# ------- Exercise 2.6 ---------

MA_6 = c(-0.4)
AR_6 = rep(0, 12)
AR_6[12] = 0.7
model_6 <- list(ar = AR_6, ma=MA_6)
set.seed(4)

ts_dat6 <-sim(model_6, 200, nburnin = 100)
plotit(ts_dat6)

mtext(expression('SARIMA Model (0,0,1) X (1,0,0)'[12] ~  theta ~  " = -0.4" ~ Phi ~" =  0.7" ),outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer26.png", width = 900, height = 650)
plotit(ts_dat6)  # Re-run the plot function
dev.off()  # Close the device