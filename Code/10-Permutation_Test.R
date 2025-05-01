library(plyr)
library(proxy)
library(RANN)
library(cubature)
library(mvtnorm)
library(tidyverse)

source("Code/00-Core_Functions.R")

my_data <- read_csv2("Data/NHANES_subsample.csv") %>%
  mutate(across(c(starts_with("age"), -age), .fns = function(x) factor(x, levels = 0:(length(unique(x)) - 1))))

lambda_vec <- rep(NA_real_, times = 1000)
x_var <- my_data$age2
y_var <- my_data$bp_diastolic
lambda_original <- tau_fast_Z(X = x_var, Y = y_var)

for(i in 1:1000){
  temp_x <- x_var[sample(1:length(x_var))]
  temp_lambda <- tau_fast_Z(temp_x, y_var)
  lambda_vec[i] <- temp_lambda
}

hist(lambda_vec)
abline(v = lambda_original)

permtest_fun <- function(x_name, y_name, nperm = 1000){
  set.seed(738465)
  lambda_vec <- rep(NA_real_, times = nperm)
  x_var <- unname(unlist(my_data[, x_name]))
  y_var <- unname(unlist(my_data[, y_name]))
  lambda_original <- tau_fast_Z(X = x_var, Y = y_var)
  
  for(i in 1:nperm){
    temp_x <- x_var[sample(1:length(x_var))]
    temp_lambda <- tau_fast_Z(temp_x, y_var)
    lambda_vec[i] <- temp_lambda
  }
  p_val <- mean(lambda_vec >= lambda_original)
  
  temp_frame <- data.frame(x_var = x_name, y_var = y_name, nperm = nperm, p_val = p_val)
  
  return(temp_frame)
}

x_vars <- c("age2", "age4", "age8", "age16", "age32", "age")
y_vars <- c("bmi", "bp_systolic", "bp_diastolic")

perm_grid <- expand.grid(x_name = x_vars, y_name = y_vars, nperm = 1000)

results <- pmap_dfr(perm_grid, permtest_fun)
save(results, file = "Simulation Results/Permutation_Test.RData")