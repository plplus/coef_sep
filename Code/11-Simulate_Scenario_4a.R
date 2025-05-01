library(proxy)
library(RANN)
library(cubature)
library(tidyverse)

source("Code/00-Core_Functions.R")

sim_fun <- function(seed, n, sigma){
  set.seed(seed)
  x_1_sample <- sample(x = c(1, 2), size = n, replace = T)
  x_2_sample <- sample(x = c(1, 2), size = n, replace = T)
  
  x_sample <- data.frame(x_1 = x_1_sample, x_2 = x_2_sample)
  
  x_group <- case_when(x_1_sample == 1 & x_2_sample == 1 ~ 1,
                       x_1_sample == 1 & x_2_sample == 2 ~ 2,
                       x_1_sample == 2 & x_2_sample == 1 ~ 3,
                       x_1_sample == 2 & x_2_sample == 2 ~ 4)
  
  y_sample <- runif(min = x_group - 1, max = x_group, n = length(x_group))
  y_sample <- y_sample + runif(min = -sigma, max = sigma, n = length(y_sample))
  est <- tau_fast_Z(X = x_sample, Y = y_sample)
  print(paste0(seed, ", ", n, ", ", sigma, ", "))
  return(data.frame(seed = seed, n = n, sigma = sigma, tau = est))
}

seed_vec <- 1:1000
n_vec <- c(100, 500, 1000, 5000, 10000, 50000)
sigma_vec <- c(1 / 2^(0:3), 0)

base_grid <- expand.grid(seed = seed_vec, n = n_vec, sigma = sigma_vec)

results <- pmap_dfr(base_grid, sim_fun)

save(results, file = "Simulation Results/Simu_4a.RData")