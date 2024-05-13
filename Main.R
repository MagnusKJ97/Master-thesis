library(tidyverse)

setwd('C:/Users/Magnus/Desktop/Code')

# Load data

data <- read_delim('EURUSD6.csv', delim = ",")

l <- length(data$Price)

# Values for the different maturities

S0<- 1.0689; rd<- 0.03352; rf<-0.05202;

# Step 1: Calculating implied volatility and the relevant Greeks

source("BlackScholesMerton.R")


for(i in 1:l){
  data[i,'impliedVolatility'] <- impVol(data[[i,1]],S0,data[[i,2]],rd,rf,data[[i,3]]/252)
}

for(i in 1:l){
  data[i, 'delta'] <- BSMDelta(S0,data[[i,2]],rd,rf,data[[i,5]],data[[i,3]]/252)
  data[i, 'vega'] <- BSMVega(S0,data[[i,2]],rd,rf,data[[i,5]],data[[i,3]]/252) / 100
}

# Plottting the market implied volatility smile
impVolPlot <- ggplot(data = data , mapping = aes(delta, impliedVolatility)) + 
  geom_line(aes(colour = factor(Expiry))) + xlab('Delta') + ylab('Implied volatility') +
  labs(colour = "Expiry"); impVolPlot

# Step 2: Heston model

source("Heston.R")

#Setting up error function

lossFunction <- function(parms){
  sum <- 0
  for(i in 1:l){
    sum <- sum + ( (data[[i,1]] - HestonCall(S0,data[[i,2]],rd,rf,data[[i,3]]/252,parms[1],parms[2],parms[3],parms[4],parms[5])) / (data[[i,7]] * 100) )^2
  }
  return(sum)
}

# Calibrate parameters

par <- optim(par = c(0.2^2, 1, 2, 0.2^2, -0.25), lossFunction, method = "L-BFGS-B",
             lower = c(0.01,0.01,0.01,0.01,-1), upper = c(0.99,10,10,0.99,1))

# Enter parameter values found in par

v <- 0.01
sigma <- 1.7782
kappa <- 1.654
theta <- 0.0718
rho <- -0.8221

#Calculate Heston prices

HestonPrice = c()
for(i in 1:l){
  HestonPrice[i] = HestonCall(S0,data[[i,2]],rd,rf,data[[i,3]]/252,v,sigma,kappa,theta,rho)
}

# Find Heston implied volatilites
for(i in 1:l){
  data[i,'HestonImp'] <- impVol(HestonPrice[i],S0,data[[i,2]],rd,rf,data[[i,3]]/252)
}

#Plot the Heston implied volatilies against the market implied volatilities

impVolPlot2 <- ggplot(data = data , mapping = aes(delta, impliedVolatility)) + 
  geom_line(aes(x = delta, y = impliedVolatility)) + 
  geom_line(aes(x = delta, y = HestonImp), linetype="dashed") +
  xlab('Delta') + 
  ylab('Implied volatility'); impVolPlot2 

#Local volatility

#Choose delta t and delta K respectively

increment1 = 0.01;
increment2 = 0.01;

#Set up every relevant Call-price for us to calculate local volatilities

BSM_deltat = c()
for (i in 1:l){
  BSM_deltat[i] = BSM(S0, data[[i,2]],rd,rf, data[[i,5]],(data[[i,3]]+increment1)/252)
}

BSM_norm = c()
for (i in 1:l){
  BSM_norm[i] = BSM(S0, data[[i,2]],rd,rf, data[[i,5]],data[[i,3]]/252)
}

Calendar_Spread = c()
for (i in 1:l){
  Calendar_Spread[i] = (BSM_deltat[i] - BSM_norm[i])/(increment1/252)
}

BSM_deltak = c()
for (i in 1:l){
  BSM_deltak[i] =  BSM(S0, data[[i,2]]+increment2,rd,rf, data[[i,5]],data[[i,3]]/252)
}

Vertical_spread = c()
for (i in 1:l){
  Vertical_spread[i] = (BSM_deltak[i] - BSM_norm[i])/increment2
}

BSM_deltakm = c()
for (i in 1:l){
  BSM_deltakm[i] =  BSM(S0, data[[i,2]]-increment2,rd,rf, data[[i,5]],data[[i,3]]/252)
}

Butterfly_spread = c()
for (i in 1:l){
 Butterfly_spread[i] = (BSM_deltakm[i]+BSM_deltak[i] - 2*BSM_norm[i])/increment2^2
}

# Local variance formula

local_var = c()
for (i in 1:l){
  local_var[i] = (Calendar_Spread[i] + (rd-rf) * data[[i,2]] * Vertical_spread[i]+rf * BSM_norm[i])/(0.5 * data[[i,2]]^2 * Butterfly_spread[i])
}

# Local volatility formula

local_vol = c()
for (i in 1:l){
  local_vol[i] = sqrt(local_var2[i])
}

# Plotting local volatilites against market implied volatilities

impVolPlot3 <- ggplot(data = data , mapping = aes(delta, impliedVolatility)) + 
  geom_line(aes(x = delta, y = impliedVolatility)) + 
  geom_point(aes(x = delta, y = local_vol)) +
  xlab('Delta') + 
  ylab('Implied volatility') + 
  labs(colour = "Expiry"); impVolPlot3


