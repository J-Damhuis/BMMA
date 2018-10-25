# Exercise 1
dd_sim_new <- function(lambda0, lambda1, q01, q10, mu, age) {
  id <- 1 # Set id of first species
  L <- data.frame(matrix(c(age, 0, -id, -1), ncol = 4)) # Create L-table with values for first species
  colnames(L) <- c("speciation time", "parent id", "id", "extinction time") # Give columns names
  lambda0species <- c() # Create list of species with lambda 0
  lambda1species <- c() # Create list of species with lambda 1
  lambda0species[length(lambda0species) + 1] <- id # Add species to lambda 0 list
  id <- id + 1 # Set id of next species
  
  L <- rbind(L, c(age, -1, id, -1)) # Set values for next species
  lambda1species[length(lambda1species) + 1] <- id # Add species to lambda 1 list
  id <- id + 1 # Set id of next species
  
  tcurrent <- age - rexp(n = 1, rate = (lambda0 * length(lambda0species)) + 
                            (lambda1 * length(lambda1species)) + 
                            (mu * (id - 1)) + 
                            (q01 * length(lambda0species)) +
                            (q10 * length(lambda1species))) # Next moment an event happens
  
  while (tcurrent > 0) {
    event <- sample(x = 1:5, size = 1, prob = c(lambda0 * length(lambda0species), 
                                                lambda1 * length(lambda1species), 
                                                mu * (id - 1), 
                                                q01 * length(lambda0species),
                                                q10 * length(lambda1species))) # Which event happens
    
    if (event == 1) { # Species with lambda 0 gives rise to new species
      parent <- sample(x = lambda0species, size = 1) # Randomly assigns parent
      if (L[parent, 3] > 0) { # Check what side of the tree the parent is on
        L <- rbind(L, c(tcurrent, parent, id, -1)) # Add values of new species to L-table
      } else {
        L <- rbind(L, c(tcurrent, -parent, -id, -1)) # Add values of new species to L-table
      }
      if (length(lambda0species) > 0) { # Make sure there are species with lambda 0
        for (i in 1:length(lambda0species)) { # Look for parent in lambda 0 list
          if (lambda0species[i] == parent) {
            lambda0species[length(lambda0species) + 1] <- id # Give new species lambda 0
          }
        }
      }
      if (length(lambda1species) > 0) { # Make sure there are species with lambda 1
        for (i in 1:length(lambda1species)) { # Look for parent in lambda 1 list
          if (lambda1species[i] == parent) {
            lambda1species[length(lambda1species) + 1] <- id # Give new species lambda 1
          }
        }
      }
      id <- id + 1
    }
    
    if (event == 2) { # Species with lambda 1 gives rise to new species
      parent <- sample(x = lambda1species, size = 1) # Randomly assigns parent
      if (L[parent, 3] > 0) { # Check what side of the tree the parent is on
        L <- rbind(L, c(tcurrent, parent, id, -1)) # Add values of new species to L-table
      } else {
        L <- rbind(L, c(tcurrent, -parent, -id, -1)) # Add values of new species to L-table
      }
      if (length(lambda0species) > 0) { # Make sure there are species with lambda 0
        for (i in 1:length(lambda0species)) { # Look for parent in lambda 0 list
          if (lambda0species[i] == parent) {
            lambda0species[length(lambda0species) + 1] <- id # Give new species lambda 0
          }
        }
      }
      if (length(lambda1species) > 0) { # Make sure there are species with lambda 1
        for (i in 1:length(lambda1species)) { # Look for parent in lambda 1 list
          if (lambda1species[i] == parent) {
            lambda1species[length(lambda1species) + 1] <- id # Give new species lambda 1
          }
        }
      }
      id <- id + 1
    }
    
    if (event == 3) { # Species goes extinct
      extinct_species <- sample(x = 1:(id - 1), size = 1) # Which species goes extinct
      L[extinct_species, 4] <- tcurrent # Set extinction time
      if (length(lambda0species) > 0) { # Make sure there are species with lambda 0
        for (i in 1:length(lambda0species)) { # Look for extinct species in lambda 0 list
          if (lambda0species[i] == extinct_species) {
            lambda0species[i] <- NA
          }
        }
        lambda0species <- na.omit(lambda0species) # Remove extinct species from lambda 0 list
      }
      if (length(lambda1species) > 0) { # Make sure there are species with lambda 1
        for (i in 1:length(lambda1species)) { # Look for extinct species in lambda 1 list
          if (lambda1species[i] == extinct_species) {
            lambda1species[i] <- NA
          }
        }
        lambda1species <- na.omit(lambda1species) # Remove extinct species from lambda 1 list
      }
    }
    
    if (event == 4) { # Species changes from lambda0 to lambda1
      changing_species <- sample(x = lambda0species, size = 1)
      for (i in 1:length(lambda0species)) { # Look for changing species in lambda 0 list
        if (lambda0species[i] == changing_species) {
          lambda0species[i] <- NA
        }
      }
      lambda0species <- na.omit(lambda0species) # Remove changing species from lambda 0 list
      lambda1species[length(lambda1species) + 1] <- changing_species # Add changing species to lambda 1 list
    }
    
    if (event == 5) { # Species changes from lambda1 to lambda0
      changing_species <- sample(x = lambda1species, size = 1)
      for (i in 1:length(lambda1species)) { # Look for changing species in lambda 1 list
        if (lambda1species[i] == changing_species) {
          lambda1species[i] <- NA
        }
      }
      lambda1species <- na.omit(lambda1species) # Remove changing species from lambda 1 list
      lambda0species[length(lambda0species) + 1] <- changing_species # Add changing species to lambda 0 list
    }
    
    tcurrent <- tcurrent - rexp(n = 1, rate = (lambda0 * length(lambda0species)) + 
                             (lambda1 * length(lambda1species)) + 
                             (mu * (id - 1)) + 
                             (q01 * length(lambda0species)) +
                             (q10 * length(lambda1species))) # Next moment an event happens
  }
  return(L)
}

tree <- dd_sim_new(lambda0 = 0.1, lambda1 = 1, mu = 0, q01 = 0, q10 = 0, age = 3)

# Exercise 2
phylogeny <- L2phylo(L = tree)
plot.phylo(x = phylogeny)
ltt.plot(phy = phylogeny)
# One lineage has way more species than the other