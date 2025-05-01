library(proxy)
library(XICOR)
library(RANN)
library(cubature)
library(mvtnorm)
library(tidyverse)

source("Code/00-Core_Functions.R")

sim_fun_A <- function(seed, n, rho){
  set.seed(seed)
  x_sample <- runif(n)
  
  y_sample_1 <- runif(n, min = -1, max = 1)
  
  ymin_2 <- -abs(1 - 2 * x_sample)
  ymax_2 <- -ymin_2
  y_sample_2 <- runif(n, min = ymin_2 , max = ymax_2)
  
  ymax_3 <- (-x_sample^2 + 2 * x_sample)^(0.9)
  ymin_3 <- 0.5 * (-x_sample^2 + 2 * x_sample)^(0.9)
  yhelp_3 <- ifelse(runif(n) < 0.5, 1, -1)
  y_sample_3 <- yhelp_3 * runif(n, min = ymin_3, max = ymax_3)
  
  tau_1 <- tau_fast_Z(X = x_sample, Y = y_sample_1)
  tau_2 <- tau_fast_Z(X = x_sample, Y = y_sample_2)
  tau_3 <- tau_fast_Z(X = x_sample, Y = y_sample_3)
  
  xi_1 <- xicor(x = x_sample, y = y_sample_1)
  xi_2 <- xicor(x = x_sample, y = y_sample_2)
  xi_3 <- xicor(x = x_sample, y = y_sample_3)
  print(paste0(seed, ", ", n))
  return(data.frame(seed = seed, n = n, tau_1 = tau_1, tau_2 = tau_2, tau_3 = tau_3,
                    xi_1 = xi_1, xi_2 = xi_2, xi_3 = xi_3))
}

seed_vec <- 1:1000
n_vec <- c(30, 100, 500, 1000, 5000, 10000, 50000, 100000)

base_grid <- expand.grid(seed = seed_vec, n = n_vec)

results_A <- pmap_dfr(base_grid, sim_fun_A)

sim_fun_B <- function(seed, n){
  set.seed(seed)
  
  x_sample <- runif(n)
  
  y_sample <- 3 * (x_sample^4 - x_sample^3 - x_sample^2 + x_sample + 0.5) + runif(n, min = -0.5, max = 0.5)

  x_sample <- runif(n)
  
  y_sample <- temp_coefs[4] * x_sample^3 + temp_coefs[3] * x_sample^2 + temp_coefs[2] * x_sample + temp_coefs[1] + runif(n, min = -0.5, max = 0.5)
  
  temp_model <- lm(data = data.frame(x = x_sample, y = y_sample), formula = y ~ x)
  
  tau <- tau_fast_Z(X = x_sample, Y = temp_model$residuals)
  
  xi <- xicor(x = x_sample, y = temp_model$residuals)
  
  print(paste0(seed, ", ", n))
  return(data.frame(seed = seed, n = n, tau_3 = tau, xi_4 = xi))
}

results_B <- pmap_dfr(base_grid, sim_fun_B)

results <- results_A %>%
  left_join(results_B)

save(results, file = "Simulation Results/Simu_5.RData")