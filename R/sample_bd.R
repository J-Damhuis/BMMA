# Exercise 1

# This function contains P0 and Pnot0
probs <- function(b,d,t) {
  P0 <- (d * (exp((b-d)*t) -1) / (b * exp((b-d)*t) - d))
  Pnot0 <- 1 - P0
  return(cbind(P0,Pnot0)) 
}

# This function contains Ut
fut <- function(b,d,t) {
  ut <- (b * (exp((b-d)*t) -1) / (b * exp((b-d)*t) - d))
  return(ut)
}

#' @title Solution to exercise 1
#' @param b birth rate
#' @param d death rate
#' @param t time
#' @param size number of samples to take
#' @examples sample_bd(b = 0.9,d = 0.6,t = 10,size = 20)
#' @export
sample_bd <- function(b,d,t,size = 1)
{
  # Calculates functions with values
  prob1 <- probs(b = b,d = d,t = t)
  ut1 <- fut(t = t,b = b,d = d)
  
  result <- rep(NA,size)
  for(i in 1:size)
  {
    # calculates outcome
    if (sample(size = 1, x = c("0","not0"),replace = TRUE,prob = prob1) == "0"){
      result[i] <- 0
      #print("The population size is 0")
    } else {
      number_of_throws <- 1 + rgeom(n = 1,prob = 1 - ut1)
      #print(paste("The population size is ", number_of_throws))
      result[i] <- number_of_throws
    }
  }
  return(result)
}



