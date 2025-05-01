library(proxy)
library(RANN)
library(cubature)
library(tidyverse)

source("Code/00-Core_Functions.R")

sim_fun <- function(seed, n, nsplits){
  set.seed(seed)
  x_sample <- runif(n = n, min = -1, max = 1)
  y_sample <- x_sample^2 + rnorm(n = n, mean = 0, sd = 0.25)
  x_sample <- ((x_sample + 1) / 2 * nsplits) %/% 1
  
  est <- tau_fast_Z(X = x_sample, Y = y_sample)
  
  y_list <- split(y_sample, x_sample)
  rte_mat <- matrix(NA_real_, nrow = length(y_list), ncol = length(y_list))
  for(i in 1:length(y_list)){
    for(j in 1:length(y_list)){
      rte_mat[i, j] <- conventional_rte(y_list[[i]], y_list[[j]])
    }
  }
  
  prob_table <- table(x_sample) / n
  prob_mat <- outer(prob_table, prob_table, FUN = "*")
  
  alpha_hat <- 1 - sum((table(x_sample) * (table(x_sample) - 1)) / (n * (n - 1)))
  
  tau_conventional <- sum(prob_mat * (2 * rte_mat - 1)^2) / alpha_hat
  print(paste0(seed, ", ", n, ", ", nsplits))
  return(data.frame(seed = seed, n = n, nsplits = nsplits, tau_conventional = tau_conventional, tau_nn = est))
}

seed_vec <- 1:1000
n_vec <- c(256)
nsplits_vec <- c(4, 8, 16, 32, 64, 128, 256)

base_grid <- expand.grid(seed = seed_vec, n = n_vec, nsplits = nsplits_vec)

results <- pmap_dfr(base_grid, sim_fun)

save(results, file = "Simulation Results/Simu_3.RData")

# Calculate real values of Lambda
library(cubature)

int_fun <- function(x){
  y_val <-  0.25 * pnorm((x[2]^2 - x[1]^2) / (0.25 * sqrt(2)))^2
  return(y_val)
}

real_value <- 4 * cubintegrate(f = int_fun, lower = c(-1, -1), upper = c(1, 1))$integral - 1

real_grid <- expand.grid(nsplits = nsplits_vec)
real_grid$real_value <- real_value

write.csv2(real_grid, "Simulation Results/Simu_3_Real_Values.csv", row.names = F)