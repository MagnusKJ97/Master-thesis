charFunc <- function(u, spot, rd, rf, ttm, v, sigma, kappa, theta, rho){
  d <- sqrt( (rho*sigma*u*1i-kappa)^2 + sigma^2*(u*1i+u^2) )
  d <- -d
  g <- (kappa - rho*sigma*u*1i + d) / (kappa - rho*sigma*u*1i - d)
  tempM <- (kappa-rho*sigma*u*1i+d)*ttm-2*log((1-g*exp(d*ttm))/(1-g))
  M <- (rd-rf)*u*1i*ttm+(kappa*theta)/(sigma^2)*tempM
  N <- (kappa-rho*sigma*u*1i+d)/(sigma^2)*((1-exp(d*ttm))/(1-g*exp(d*ttm)))
  res <- exp(M+N*v+1i*u*log(spot))
  return(res)
}

HestonCall <- function(spot, strike, rd, rf, ttm, v, sigma, kappa, theta, rho){
  integrand1 <- function(u){ 
    num1 <- charFunc(u-1i, spot, rd, rf, ttm, v, sigma, kappa, theta, rho)
    den1 <- charFunc(-1i, spot, rd, rf, ttm, v, sigma, kappa, theta, rho)
    dummy1 <- exp(-1i*u*log(strike))*num1/(1i*u*den1)
    integrand1 <- Re(dummy1)
  }
  integrand2 <- function(u){ 
    dummy2 <- exp(-1i*u*log(strike))*charFunc(u, spot, rd, rf, ttm, v, sigma, kappa, theta, rho)/(1i*u)
    integtand2 <- Re(dummy2)
  }
  Pi1 <- 0.5 + 1/pi * integrate(integrand1,0,Inf,stop.on.error = FALSE)$value
  Pi2 <- 0.5 + 1/pi * integrate(integrand2,0,Inf,stop.on.error = FALSE)$value
  res <- spot*exp(-rf*ttm)*Pi1 - strike*exp(-rd*ttm)*Pi2
  return(res)
}

