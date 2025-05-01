library(tidyverse)

source("Code/00-Core_Functions.R")

load("Data/chelsea.RData")

chelsea <- chelsea %>%
  mutate(across(c(long, lat), .fns = function(x) as.numeric(str_replace(x, ",", "\\."))))

chelsea <- chelsea %>%
  na.omit()

pred_vars <- names(chelsea %>% select(-AP_R))
pred_vars <- c("PWeQ_R", "PDQ_R", "PWaQ_R", "PCQ_R", "MTWaQ_R", "MTCQ_R", "MTWeQ_R", "MTDQ_R")

remaining_vars <- pred_vars
chosen_vars <- c()

forward_frame <- data.frame(chosen = character(), lambda = numeric())
while(length(remaining_vars) > 0){
  temp_lambda_vec <- c()
  for(i in 1:length(remaining_vars)){
    X <- chelsea[, c(chosen_vars, remaining_vars[i])]
    set.seed(738465)
    temp_lambda <- tau_fast_Z(Y = chelsea$AP_R, X = X)
    temp_lambda_vec <- c(temp_lambda_vec, temp_lambda)
    print(paste0("Ausgewählte Variablen: ", length(chosen_vars), "; Verbleibende Variablen: ", length(remaining_vars), "; Gerade überprüfte Variable: ", remaining_vars[i]))
  }
  chosen_index <- which.max(temp_lambda_vec)
  forward_frame <- bind_rows(forward_frame, data.frame(chosen = remaining_vars[chosen_index], lambda = max(temp_lambda_vec)))
  chosen_vars <- c(chosen_vars, remaining_vars[chosen_index])
  remaining_vars <- remaining_vars[-chosen_index]
}

save(forward_frame, file = "Simulation Results/forward_results.RData")

best_subsample_frame <- expand.grid(PWeQ_R = c(0, 1), PDQ_R = c(0, 1), PWaQ_R = c(0, 1), PCQ_R = c(0, 1),
                                    MTWaQ_R = c(0, 1), MTCQ_R = c(0, 1), MTWeQ_R = c(0, 1), MTDQ_R = c(0, 1))

best_subsample_frame <- best_subsample_frame[-1,]

best_subsample_lambda <- c()
for(i in 1:nrow(best_subsample_frame)){
  chosen_vars <- names(best_subsample_frame)[which(unlist(best_subsample_frame[i,]) == 1)]
  X <- chelsea %>% select(all_of(chosen_vars))
  set.seed(738465)
  temp_lambda <- tau_fast_Z(Y = chelsea$AP_R, X = X)
  best_subsample_lambda <- c(best_subsample_lambda, temp_lambda)
  print(i)
}

best_subsample_frame$lambda <- best_subsample_lambda

best_subsample_frame %>%
  ggplot(aes(x = 1, y = lambda)) +
  geom_point() +
  theme_bw()

save(best_subsample_frame, file = "Simulation Results/best_subsample_results.RData")

tau_fast_Z(Y = chelsea$AP_R, X = chelsea %>% select(PWeQ_R, PDQ_R, PWaQ_R, PCQ_R, MTWaQ_R, MTCQ_R, MTWeQ_R))
chelsea %>% select(PWeQ_R, PDQ_R, PWaQ_R, PCQ_R, MTWaQ_R, MTCQ_R, MTWeQ_R)  %>% nrow()
chelsea %>% select(PWeQ_R, PDQ_R, PWaQ_R, PCQ_R, MTWaQ_R, MTCQ_R, MTWeQ_R)  %>% unique() %>% nrow()
chelsea %>% select(PWeQ_R, PDQ_R, PWaQ_R, PCQ_R, MTWaQ_R, MTCQ_R, MTWeQ_R, MTDQ_R)  %>% nrow()
chelsea %>% select(PWeQ_R, PDQ_R, PWaQ_R, PCQ_R, MTWaQ_R, MTCQ_R, MTWeQ_R, MTDQ_R)  %>% unique() %>% nrow()

#### Chatterjee
library(tidyverse)
library(FOCI)

load("Data/chelsea.RData")

chelsea <- chelsea %>%
  mutate(across(c(long, lat), .fns = function(x) as.numeric(str_replace(x, ",", "\\."))))

chelsea <- chelsea %>%
  na.omit()

pred_vars <- names(chelsea %>% select(-AP_R))
pred_vars <- c("PWeQ_R", "PDQ_R", "PWaQ_R", "PCQ_R", "MTWaQ_R", "MTCQ_R", "MTWeQ_R", "MTDQ_R")

remaining_vars <- pred_vars
chosen_vars <- c()

forward_frame <- data.frame(chosen = character(), xi = numeric())
while(length(remaining_vars) > 0){
  temp_xi_vec <- c()
  for(i in 1:length(remaining_vars)){
    Z <- chelsea[, c(chosen_vars, remaining_vars[i])]
    Z <- as.matrix(Z)
    set.seed(738465)
    temp_xi <- codec(Y = chelsea$AP_R, Z = Z)
    temp_xi_vec <- c(temp_xi_vec, temp_xi)
    print(paste0("Ausgewählte Variablen: ", length(chosen_vars), "; Verbleibende Variablen: ", length(remaining_vars), "; Gerade überprüfte Variable: ", remaining_vars[i]))
  }
  chosen_index <- which.max(temp_xi_vec)
  forward_frame <- bind_rows(forward_frame, data.frame(chosen = remaining_vars[chosen_index], xi = max(temp_xi_vec)))
  chosen_vars <- c(chosen_vars, remaining_vars[chosen_index])
  remaining_vars <- remaining_vars[-chosen_index]
}

save(forward_frame, file = "Simulation Results/forward_results.RData")

best_subsample_frame <- expand.grid(PWeQ_R = c(0, 1), PDQ_R = c(0, 1), PWaQ_R = c(0, 1), PCQ_R = c(0, 1),
                                    MTWaQ_R = c(0, 1), MTCQ_R = c(0, 1), MTWeQ_R = c(0, 1), MTDQ_R = c(0, 1))

best_subsample_frame <- best_subsample_frame[-1,]

best_subsample_xi <- c()
for(i in 1:nrow(best_subsample_frame)){
  chosen_vars <- names(best_subsample_frame)[which(unlist(best_subsample_frame[i,]) == 1)]
  Z <- chelsea %>% select(all_of(chosen_vars))
  Z <- as.matrix(Z)
  set.seed(738465)
  temp_xi <- codec(Y = chelsea$AP_R, Z = Z)
  best_subsample_xi <- c(best_subsample_xi, temp_xi)
  print(i)
}

best_subsample_frame$xi <- best_subsample_xi