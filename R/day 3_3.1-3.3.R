rm(list=ls()) 
library(DDD)

#EXERCISE 3.1
?dd_sim
par <- c(3,0.1,40)
t1 <- dd_sim(par,0)
t2 <- dd_sim(par,5)
t3 <- dd_sim(par,15)
plot(t1$tes)
plot(t2$tes)
plot(t3$tes)


#EXERCISE 3.2
?ltt.plot
ltt.plot(t1$tes)
ltt.plot(t2$tes)
ltt.plot(t3$tes)

# Function that plots and simulates trees for different mu
LTT_plot <- function(mu,age){
  par1 <- c(3, mu, 40)
  t <- dd_sim(par1,age)
  ltt.plot(t$tes, xlab = "Time",ylab = "N",backward = FALSE)
}
LTT_plot(0.1, 15)


#EXERCISE 3.3
#Simulate a tree with dd_sim for λ0 = 3, µ = 0.1, and K = 40 and a crown age of 15, 
#and estimate the parameters using dd_ML. Repeat this for various other values of K. 
#How well do your parameter estimates match the parameter values used to simulate? 
#Hint: Use branching.times from the package ape to obtain branching times.

# For one tree
tr <- dd_sim(pars = par,age = 15)
dd_ML(brts = branching.times(tr$tes),initparsopt = par,idparsopt = 1:3)

K_vec <- c(10, 20, 30)


#' Simulate and run MLE on trees of varying K. (What we wrote in class)
#'
#' @param K_vec vector of values of K. Length of vector determines how many trees are generated
#'
#' @return list contaning ML results
#' @export
#'
#' @examples simulate_estimate_K(c(10, 15, 20))
Lpar <- function(pars) {
  trees <- list()
  ML_results <- list()
  for (i in 1:length(K_vec)) {
    trees[i] <- dd_sim(pars = c(3, 0.1, K_vec[i]), age = 15, ddmodel = 1)
    ML_results[[i]] <- dd_ML(brts = branching.times(trees[[i]]),
                             initparsopt = par,
                             idparsopt = 1:3)
  }
  return(ML_results)
}


#' Runs MLE on one simulation
#'
#' @param lambda speciation rate
#' @param mu extinction rate
#' @param age crown age
#' @param K carrying capacity
#'
#' @return output of dd_ML for one simulation
#' @export
#'
#' @examples Lpar(3,0.1,15,40)
Lpar <- function(lambda, mu, age, K) {
  par1 <- c(lambda, mu, K)
  Ltr <- dd_sim(pars = par1, age = age)
  ddml <- dd_ML(brts = branching.times(Ltr$tes),
                initparsopt = par1,
                idparsopt = 1:3)
  return(ddml)
}
#function above can be used to estimate the parameters but will
#use data from previous running of the function to calculate the new estimated parameters. 
#results will therefore change every time we run the function. 
#This gives a idea of the parameter uncertainty 


par31 <- c(3,0.1,10)
tr <- dd_sim(pars = par31,age = 15)
dd_ML(brts = branching.times(tr$tes), initparsopt = par, idparsopt = 1:3)
#when changing the K with the code lines above, we can see that when K gets lower,
#the difference between the initial lambda and the estimated gets bigger. 

