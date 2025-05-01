library(proxy)
library(RANN)
library(cubature)
library(tidyverse)

source("Code/00-Core_Functions.R")

sim_fun <- function(seed, n_1, n_2, mean_1, mean_2, sd_1, sd_2, subscen){
  set.seed(seed)
  n <- n_1 + n_2
  x_sample <- rep(c(0, 1), times = c(n_1, n_2))
  y_sample <- c(rnorm(n_1, mean_1, sd_1), rnorm(n_2, mean_2, sd_2))
  y_list <- split(y_sample, x_sample)
  
  est_z <- tau_fast_Z(X = x_sample, Y = y_sample)
  
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
  print(paste0(seed, ", ", n, ", ", subscen))
  return(data.frame(seed = seed, n_1 = n_1, n_2 = n_2, mean_1 = mean_1, mean_2 = mean_2, sd_1 = sd_1, sd_2 = sd_2, subscen = subscen, tau_conventional = tau_conventional, tau_nn_z = est_z))
}

seed_vec <- 1:1000
n_vec <- c(30, 100, 500, 1000, 5000, 10000, 50000)

scen_A_frame <- expand.grid(seed = seed_vec, n = n_vec)
scen_A_frame$n_1 <- scen_A_frame$n / 2
scen_A_frame$n_2 <- scen_A_frame$n / 2
scen_A_frame$mean_1 <- 0
scen_A_frame$mean_2 <- 2
scen_A_frame$sd_1 <- 1
scen_A_frame$sd_2 <- 1
scen_A_frame$subscen <- "A"

scen_B_frame <- expand.grid(seed = seed_vec, n = n_vec)
scen_B_frame$n_1 <- scen_B_frame$n / 2
scen_B_frame$n_2 <- scen_B_frame$n  / 2
scen_B_frame$mean_1 <- 0
scen_B_frame$mean_2 <- 2
scen_B_frame$sd_1 <- 1
scen_B_frame$sd_2 <- 2
scen_B_frame$subscen <- "B"

base_grid <- bind_rows(scen_A_frame, scen_B_frame)
base_grid <- base_grid %>%
  select(-n)

results <- pmap_dfr(base_grid, sim_fun)

results <- results %>%
  mutate(n = n_1 + n_2)

save(results, file = "Simulation Results/Simu_2.RData")

# Calculate real values of Lambda
library(cubature)

real_grid <- base_grid %>%
  select(-seed, -n_1, -n_2) %>%
  unique()

real_grid <- real_grid %>%
  mutate(real_value = 4 * (pnorm((mean_1 - mean_2) / sqrt(sd_1^2 + sd_2^2)) - 0.5)^2) %>%
  select(subscen, real_value)

write.csv2(real_grid, "Simulation Results/Simu_2_Real_Values.csv", row.names = F)