# Exercise 1
age <- 1
pars <- c(3, 0.1, 40)
tree <- dd_sim(pars = pars, age = age)
plot(tree$tes)

# Exercise 2
ltt.plot(phy = tree$tes)

# Exercise 3
age <- 15
pars <- c(3, 0.1, 10)
tree <- dd_sim(pars = pars, age = age)
ml <- dd_ML(brts = tree$brts)
# Estimates are close, but not exact, lower K gives overestimate of lambda

# Exercise 4
ml <- data.frame(matrix(ncol = 2))
colnames(ml) <- c("diversity dependent", "diversity independent")
aic <- ml
for (i in 1:5) {
  tree1 <- dd_sim(pars = c(3, 0.1, 40), age = 5)
  maxlikdd <- dd_ML(brts = tree1$brts)
  ml[i,1] <- maxlikdd$loglik
  tree2 <- dd_sim(pars = c(3, 0.1, Inf), age = 5)
  maxlikdi <- dd_ML(brts = tree2$brts)
  ml[i,2] <- maxlikdi$loglik
  aic[i] <- akaike.weights(x = c(6 - 2 * ml[i,1], 4 - 2 * ml[i,2]))$weights
}