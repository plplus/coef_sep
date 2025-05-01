library(proxy)
library(RANN)
library(cubature)
library(tidyverse)

source("Code/00-Core_Functions.R")

sim_fun <- function(seed, n){
  set.seed(seed)
  x_1_sample <- sample(x = c(1, 2), size = n, replace = T)
  x_2_sample <- sample(x = c(1, 2), size = n, replace = T)
  
  x_sample <- data.frame(x_1 = x_1_sample, x_2 = x_2_sample)
  
  y_sample <- x_1_sample * cos(x_1_sample * x_2_sample)
  
  y_1_sample <- y_sample + rnorm(mean = 0, sd = sqrt(1/2), n = nrow(x_sample))
  y_2_sample <- y_sample + runif(min = -0.5, max = 0.5, n = nrow(x_sample))
  
  est_1 <- tau_fast_Z(X = x_sample, Y = y_1_sample)
  est_2 <- tau_fast_Z(X = x_sample, Y = y_2_sample)
  print(paste0(seed, ", ", n))
  return(data.frame(seed = seed, n = n, tau_1 = est_1, tau_2 = est_2))
}

seed_vec <- 1:1000
n_vec <- c(30, 100, 500, 1000, 5000, 10000, 50000)

base_grid <- expand.grid(seed = seed_vec, n = n_vec)

results <- pmap_dfr(base_grid, sim_fun)

save(results, file = "Simulation Results/Simu_4b.RData")