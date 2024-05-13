BSM <- function(S,K,rd,rf,sigma,TTM,call=T){
  d1 <- (log(S/K) + (rd - rf + sigma^2/2) * TTM)/(sigma * sqrt(TTM))
  d2 <- d1 - sigma * sqrt(TTM)
  if(call) price <- pnorm(d1) * S * exp(-rf * TTM) - exp(-rd * TTM) * K * pnorm(d2)
  else price <- exp(-rd * TTM) * K * pnorm(-d2) - pnorm(-d1) * S * exp(-rf * TTM) 
  return(price)
}

BSMDelta <- function(S,K,rd,rf,sigma,TTM){
  d1 <- (log(S/K) + ((rd-rf) + sigma^2/2) * TTM)/(sigma * sqrt(TTM))
  return(pnorm(d1))
}

BSMVega <- function(S,K,rd,rf,sigma,TTM){
  d1 <- (log(S/K) + ((rd-rf) + sigma^2/2) * TTM)/(sigma * sqrt(TTM))
  return(S * dnorm(d1) * sqrt(TTM))
}

impVol <- function(price,S,K,rd,rf,TTM){
  f <- function(sigma) price - BSM(S,K,rd,rf,sigma,TTM)
  impVol <- uniroot(f,c(0,1), extendInt = "yes", maxiter = 10000, tol = 0.0000001)
  return(impVol$root)
}

