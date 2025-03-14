# Install and load necessary packages
install.packages(c("forecast", "ggplot2"))
library(forecast)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# ----------------------------------------

# ------- Exercise 1 ---------

cat("\n------------- Exercise 1.1 -------------\n")


cat("\n------------- Exercise 1.2 -------------\n")


cat("\n------------- Exercise 1.3 -------------\n")


cat("\n------------- Exercise 1.4 -------------\n")

# ------- Exercise 2 ---------

rm(list = ls())

# Function to plot time series, ACF, and PACF
plotit <- function(x) {
  par(mfrow = c(3, 1))  # 3 rows, 1 column layout
  par(mar=c(3,3,2,2), mgp=c(2, 0.7,0))  # Adjust margins for better spacing
  
  # Time series plot
  plot.ts(x, ylab = "X", main = "Simulated Time Series", col = "black", type = "l")
  
  # ACF plot
  acf(x, lag.max = 50, main = "Autocorrelation Function (ACF)", lwd = 2)
  
  # PACF plot
  pacf(x, lag.max = 50, main = "Partial Autocorrelation Function (PACF)", lwd = 2)
}

# Function to simulate an ARMA process
sim <- function(model, n, nburnin = 100) {
  n <- n + nburnin  # Extend series for burn-in period
  ar <- model$ar  # AR coefficients
  ma <- model$ma  # MA coefficients (if any)
  
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

cat("\n------------- Exercise 2.1 -------------\n")

# AR model with phi = 0.6
model_1 <- list(ar = c(0.6))
set.seed(4)

ts_data <- sim(model_1, 400) # Increased sample size for better visualization

# Plot results
plotit(ts_data)
mtext(expression('SARIMA Model (1,0,0) × (0,0,0)'[12] ~ phi ~ "= 0.6"), side = 3, outer = TRUE, line = -2, cex = 1.2)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer21.png", width = 800, height = 800)  # Open PNG device
plotit(ts_data)  # Re-run the plot function
dev.off()  # Close the device

cat("\n------------- Exercise 2.2 -------------\n")

AR_2  = rep(0, 12)
AR_2[12] = -0.9
model_2 <- list(ar=AR_2)
set.seed(4)

ts_data2 <- sim(model_2, 500) # Increased sample size for better visualization
plotit(ts_data2)

mtext(expression('SARIMA Model (0,0,0) × (1,0,0)'[12] ~ phi ~ "= -0.9"), side = 3, outer = TRUE, line = -0.8)#, cex = 1.2)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer22.png", width = 800, height = 800)  # Open PNG device
plotit(ts_data2)  # Re-run the plot function
dev.off()  # Close the device

cat("\n------------- Exercise 2.3 -------------\n")

MA_3  = rep(0, 12)
MA_3[12] = -0.7
AR_3 = c(0.9)
model_3 <- list(ar=AR_3,ma = MA_3)
set.seed(4)

ts_data3 <-sim(model_3, 500)
plotit(ts_data3)

mtext(expression('SARIMA Model (1,0,0) X (0,0,1)'[12] ~  phi ~  " = 0.9" ~ Theta ~" = -0.7" ), side = 3,outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer23.png", width = 800, height = 800)  # Open PNG device
plotit(ts_data3)  # Re-run the plot function
dev.off()  # Close the device

cat("\n------------- Exercise 2.4 -------------\n")

AR_4 = rep(0, 12)
AR_4[1]=-0.6
AR_4[12] = -0.8
model_4 <- list(ar=AR_4)
set.seed(4)

ts_data4 <-sim(model_4, 500)
plotit(ts_data4)

mtext(expression('SARIMA Model (1,0,0) X (1,0,0)'[12] ~  phi ~  " = -0.6" ~ Phi ~" = -0.8" ),outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer24.png", width = 800, height = 800)  # Open PNG device
plotit(ts_data4)  # Re-run the plot function
dev.off()  # Close the device

cat("\n------------- Exercise 2.5 -------------\n")

MA_5 = rep(0, 12)
MA_5[1]=0.4
MA_5[12] = -0.8
model_5 <- list(ma=MA_5)
set.seed(4)

ts_dat5 <-sim(model_5, 500)
plotit(ts_dat5)

mtext(expression('SARIMA Model (0,0,1) X (0,0,1)'[12] ~  theta ~  " = 0.4" ~ theta ~" = -0.8" ),outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer25.png", width = 800, height = 800)  # Open PNG device
plotit(ts_dat5)  # Re-run the plot function
dev.off()  # Close the device

cat("\n------------- Exercise 2.6 -------------\n")

MA_6 = c(-0.4)
AR_6 = rep(0, 12)
AR_6[12] = 0.7
model_6 <- list(ar = AR_6, ma=MA_6)
set.seed(4)

ts_dat6 <-sim(model_6, 500)
plotit(ts_dat6)

mtext(expression('SARIMA Model (0,0,1) X (1,0,0)'[12] ~  theta ~  " = -0.4" ~ Phi ~" =  0.7" ),outer=TRUE,line=-0.8)
png("~/Desktop/uni BA/2nd semester/TSA/Assignment2/exer26.png", width = 800, height = 800)  # Open PNG device
plotit(ts_dat6)  # Re-run the plot function
dev.off()  # Close the device

cat("\n------------- Exercise 2.7 -------------\n")



# ------- Exercise 3 ---------

