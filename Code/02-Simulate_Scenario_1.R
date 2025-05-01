library(proxy)
library(RANN)
library(cubature)
library(mvtnorm)
library(tidyverse)

source("Code/00-Core_Functions.R")

sim_fun <- function(seed, n, rho){
  set.seed(seed)
  temp_sample <- rmvnorm(n = n, mean = c(0, 0), sigma = matrix(c(1, rho, rho, 1), nrow = 2, byrow = T))
  x_sample <- temp_sample[,1]
  y_sample <- temp_sample[,2]
  est <- tau_fast_Z(X = x_sample, Y = y_sample)
  print(paste0(seed, ", ", n, ", ", rho, ", "))
  return(data.frame(seed = seed, n = n, rho = rho, tau = est))
}

seed_vec <- 1:1000
n_vec <- c(30, 100, 500, 1000, 5000, 10000, 50000)
rho_vec <- c(0, 0.4, 0.75, 1)

base_grid <- expand.grid(seed = seed_vec, n = n_vec, rho = rho_vec)

results <- pmap_dfr(base_grid, sim_fun)

save(results, file = "Simulation Results/Simu_1.RData")

# Calculate real values of Lambda
real_grid <- expand.grid(rho = rho_vec)
real_grid$real_value <- (2 / pi) * asin(real_grid$rho^2)

ggplot(results, aes(x = factor(n), y = tau, fill = factor(rho))) +
  geom_boxplot() +
  geom_hline(data = real_grid, aes(yintercept = real_value, col = factor(rho))) +
  theme_bw()

write.csv2(real_grid, "Simulation Results/Simu_1_Real_Values.csv", row.names = F)