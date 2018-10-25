#' @title simulate discrete time model by going through all time steps
#' @param tmax maximum simulation time
#' @param pin immigration probability per time step
#' @param pout emigration probabilty per time step
#' @param n0 initial number of individuals
#' @examples system.time(simplesim_discrete(tmax = 1000000,pin = 0.01,pout = 0.01))
#' @export
simplesim_discrete <- function(pin = 0.1,pout = 0.1,n0 = 1000,tmax = 10^6){
  n <- n0
  ns <- rep(0,1 + tmax)
  ns[1] <- n
  for(t in 1:tmax)
  {
    n <- n + sample(x = c(1,-1,0), size = 1, prob = c(pin,pout,1-pin-pout),replace=TRUE)
    ns[t + 1] <- n
    if(n == 0){break}
  }
  return(plot(1:(1 + tmax),ns,type = 'l',xlab = 'time',ylab = 'population size'))
}

#' @title simulate discrete time model by Doob-Gillespie algorithm
#' @param tmax maximum simulation time
#' @param pin immigration probability per time step
#' @param pout emigration probabilty per time step
#' @param n0 initial number of individuals
#' @examples system.time(efficientsim_discrete(tmax = 1000000,pin = 0.01,pout = 0.01))
#' @export
efficientsim_discrete <- function(pin = 0.1,pout = 0.1, n0 = 1000, t0 = 0, tmax = 10^6){
  nold <- n0
  ns <- rep(0,1 + tmax)
  dt <- 1 + rgeom(n = 1,prob = pin + pout)
  told <- t0
  tnew <- told + dt
  while(tnew < tmax)
  {
    nnew <- nold + sample(x = c(-1,1),prob = c(pout,pin),replace = TRUE, size = 1) #sample immigration or emigration
    if(nnew == 0) {break}
    ns[1 + (told:(tnew - 1))] <- nold 
    told <- tnew
    dt <- 1 + rgeom(n = 1,prob = pin + pout)
    tnew <- told + dt
    nold <- nnew
  }
  ns[1 + (told:tmax)] <- nold
  return(plot(0:tmax,ns,type = 'l',xlab = 'time',ylab = 'population size'))
}
