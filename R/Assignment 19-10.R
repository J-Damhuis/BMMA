# Question 1
# a0 and a1 are competition terms. They indicate how much species with a certain trait value 
# negatively affect the speciation rate of all species. This could be due to the size of the
# niches that they fill

#' Simulation using the model described in the assignment
#'
#' @param lambda intrinsic speciation rate
#' @param a0 factor for how much each species with trait 0 negatively affects the speciation rate
#' @param a1 factor for how much each species with trait 1 negatively affects the speciation rate
#' @param mu extinction rate
#' @param q01 rate at which species with trait 0 convert to having trait 1
#' @param q10 rate at which species with trait 1 convert to having trait 0
#' @param age crown age
#'
#' @return a list of the L-table and data on the number of living species over time
#' @export
#'
#' @examples bd_sim(lambda = 0.8, mu = 0.1, q01 = 0.1, q10 = 0.1, a0 = 0.1, a1 = 0.1, age = 15)
bd_sim <- function(lambda, a0, a1, mu, q01, q10, age) {
  test <- 0 # If this is 1, both crown lineages have at least 1 surviving species
  while (test == 0) { # Keep running the simulation until both crown lineages have at least 1 surviving species
    id <- 1 # Set id of first species
    L <- data.frame(matrix(c(age, 0, -id, -1), ncol = 4)) # Create L-table with values for first species
    colnames(L) <- c("speciation_time", "parent_id", "id", "extinction_time") # Give columns names
    trait_list <- data.frame(matrix(c(id, 0), ncol = 2)) # Create data frame to keep track of trait values, also add first species
    colnames(trait_list) <- c("id", "trait") # Give columns names
    id <- id + 1 # Set id of next species
    L <- rbind(L, c(age, -1, id, -1)) # Add second species to L-table
    trait_list <- rbind(trait_list, c(id, 1)) # Add second species to trait data frame
    id <- id + 1 # Set id of next species
    species_number <- data.frame(matrix(c(-age, id - 1), ncol = 2)) # Create data frame to keep track of number of species. Only used for plot
    colnames(species_number) <- c("time", "species_number") # Give columns names
    tcurrent <- age - rexp(n = 1, rate = (max(lambda * (1 - a0 * length(which(trait_list$trait == 0)) - a1 * length(which(trait_list$trait == 1))), 0)) + 
                             (mu * (length(which(L$extinction_time == -1)) - 1)) + 
                             (q01 * length(which(trait_list$trait == 0))) +
                             (q10 * length(which(trait_list$trait == 1)))) # Next moment an event happens
    while (tcurrent > 0) {
      event <- sample(x = 1:4, size = 1, prob = c(max(lambda * (1 - a0 * length(which(trait_list$trait == 0)) - a1 * length(which(trait_list$trait == 1))), 0),
                                                  mu * (length(which(L$extinction_time == -1)) - 1),
                                                  q01 * length(which(trait_list$trait == 0)),
                                                  q10 * length(which(trait_list$trait == 1)))) # Which event happens
      if (event == 1) { # Speciation event
        parent <- sample(x = which(L$extinction_time == -1), size = 1) # Choose a random parent
        if (L[parent, 3] > 0) { # Check which side of the tree the parent is on
          L <- rbind(L, c(tcurrent, parent, id, -1)) # Add new species' values to L-table
        } else {
          L <- rbind(L, c(tcurrent, -parent, -id, -1)) # Add new species' values to L-table
        }
        trait_list <- rbind(trait_list, c(id, trait_list[parent, 2])) # Give new species mother's trait value
        id <- id + 1 # Set id for next species
      }
      if (event == 2) { # Extinction event
        extinct_species <- sample(x = which(L$extinction_time == -1), size = 1) # Choose random species to go extinct
        L[extinct_species, 4] <- tcurrent # Set extinction time
        trait_list[extinct_species, 2] <- -1 # Set trait value to -1 to remove this species from n0 or n1
      }
      if (event == 3) { # Species with trait 0 changes to trait 1
        changing_species <- sample(x = which(trait_list$trait == 0), size = 1) # Choose random species to change trait value
        trait_list[changing_species, 2] <- 1 # Change trait value in trait data frame
      }
      if (event == 4) { # Species with trait 1 changes to trait 0
        changing_species <- sample(x = which(trait_list$trait == 1), size = 1) # Choose random species to change trait value
        trait_list[changing_species, 2] <- 0 # Change trait value in trait data frame
      }
      species_number <- rbind(species_number, c(-tcurrent, length(which(L$extinction_time == -1)))) # Adds new species number to data frame
      tcurrent <- tcurrent - rexp(n = 1, rate = (max(lambda * (1 - a0 * length(which(trait_list$trait == 0)) - a1 * length(which(trait_list$trait == 1))), 0)) + 
                               (mu * (length(which(L$`extinction time` == -1)) - 1)) + 
                               (q01 * length(which(trait_list$trait == 0))) +
                               (q10 * length(which(trait_list$trait == 1)))) # Next moment an event happens
    }
    species_number <- rbind(species_number, c(0, length(which(L$extinction_time == -1)))) # Adds final species number to data frame
    if (length(which(L$extinction_time == -1 & L$id > 0)) > 0) { # Check if one of the crown lineages has at least 1 surviving species
      if (length(which(L$extinction_time == -1 & L$id < 0)) > 0) { # Check if the other crown lineage has at least 1 surviving species
        test <- 1 # End the while loop and move on to output results
      }
    }
  }
  return(list(L = L, species = species_number)) # Output L-table and species number plot in a list
}

# Question 2
plot(bd_sim(lambda = 0.8, mu = 0.1, q01 = 0.1, q10 = 0.1, a0 = 0.1, a1 = 0.1, age = 15)$species)

# Question 4
a0 <- 0.01
a1 <- 0.1
out <- bd_sim(lambda = 0.8, mu = 0.1, q01 = 0.1, q10 = 0.1, a0 = a0, a1 = a1, age = 15)$L
phylogeny <- L2phylo(L = out)
plot.phylo(x = phylogeny)
ltt.plot(phy = phylogeny)
# The lower a0 and a1 are, the more species there will be

# Question 5
#' Runs multiple simulations using the model described in the assignment and turns them into phylogenies
#'
#' @param pars a vector of all the parameters used in bd_sim
#' @param n number of simulations
#'
#' @return a list of the phylogenies
#' @export
#'
#' @examples multiphylos(pars = c(0.8, 0.025, 0.05, 0.1, 0.1, 0.1, 15), n = 100)
multiphylos <- function(pars, n) {
  L_tables <- list() # Create list to store L-tables
  phylogenies <- list() # Create list to store phylogenies
  for (i in 1:n) {
    L_tables[[i]] <- bd_sim(pars[1], pars[2], pars[3], pars[4], pars[5], pars[6], pars[7])$L # Simulate L-tables
    phylogenies[[i]] <- L2phylo(L = L_tables[[i]]) # Convert L-tables to phylogenies
  }
  return(phylogenies)
}

pars <- c(0.8, 0.025, 0.05, 0.1, 0.1, 0.1, 15) # Every species with trait value 1 negatively affects the speciation rate twice as much as those with trait value 0
phylos <- multiphylos(pars = pars, n = 100)
mltt.plot(phy = phylos, legend = FALSE)

# Question 6
# The model I made is a general version of the diversity dependence model. Assuming the diversity dependence 
# model uses a logistic growth function, my model turns into the diversity dependence model for q01 = 0 and 
# a0 = 1/K. This model has the term 1-(n0*a0+n1*a1), and the dd model has the term 1-(n/K). You can change 
# a0 and a1 to 1/K0 and 1/K1 respectively. The term you get then is really similar to the one from the dd 
# model and therefore using the dd model to estimate parameters is still useful.

# Question 7
#' Runs maximum likelihood parameter estimation tests on a list of phylogenies
#'
#' @param list list of phylogenies
#'
#' @return data frame with all parameter estimates, loglikelihood, df and conv
#' @export
#'
#' @examples multiml(multiphylos(pars = c(0.8, 0.025, 0.05, 0.1, 0.1, 0.1, 15), n = 100))
multiml <- function(list) {
  par_est <- data.frame()
  for (i in 1:length(list)) {
    par_est <- rbind(par_est, dd_ML(brts = branching.times(list[[i]])))
  }
  return(par_est)
}

multiml(phylos)
# The estimated values for lambda can be extremely high, which apparently indicates coalescescent
# diversification processes. There are also very small estimated values for mu. All in all, this
# estimation of parameters was not very accurate whatsoever.

# Question 8
# Fitting ML parameters with both a diversity dependent model and a diversity independent model and then
# compare the AIC weights.