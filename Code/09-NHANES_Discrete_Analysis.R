library(tidyverse)
library(GGally)
library(rlist)

source("Code/00-Core_Functions.R")

my_data <- read_csv2("Data/NHANES_subsample.csv") %>%
  mutate(across(c(starts_with("age"), -age), .fns = function(x) factor(x, levels = 0:(length(unique(x)) - 1))))

varnames_age <- c("age2", "age4", "age8", "age16", "age32", "age")
varnames_response <- c("bp_systolic", "bp_diastolic", "bmi")


extract_lambda <- function(variable_1, variable_2){
  temp_data <- my_data %>%
    select(all_of(c(variable_1, variable_2)))
  
  set.seed(85729)
  estimate <- ifelse(ncol(temp_data) == 2, tau_fast_Z(X = unlist(temp_data[,1]), Y = unlist(temp_data[,2])), 1)
  
  return(data.frame(variable_1 = variable_1, variable_2 = variable_2, lambda = estimate))
}

my_grid <- expand_grid(variable_1 = varnames_age, variable_2 = varnames_response)

lambda_frame <- pmap_dfr(.f = extract_lambda, .l = my_grid)

lambda_frame$ycoef <- c(rep(0.95, 18))

lambda_frame$textpos_x <- rep(c(1.5, 2, 4, 8, 16, 45), each = 3)


extract_plots <- function(variable_1, variable_2){
  temp_data <- my_data %>%
    select(all_of(c(variable_1, variable_2)))
  
  var1 <- sym(variable_1)
  var2 <- sym(variable_2)
  
  if(ncol(temp_data) == 2){
  temp_lambda <- lambda_frame %>%
    filter(variable_1 == .env$variable_1, variable_2 == .env$variable_2) %>%
    select(lambda) %>%
    unlist()
  
  temp_ycoef <- lambda_frame %>%
    filter(variable_1 == .env$variable_1, variable_2 == .env$variable_2) %>%
    select(ycoef) %>%
    unlist()
  
  temp_textpos_x <- lambda_frame %>%
    filter(variable_1 == .env$variable_1, variable_2 == .env$variable_2) %>%
    select(textpos_x) %>%
    unlist()
  
  yspan <- range(temp_data[,2])[2] - range(temp_data[,2])[1]
  ymin <- range(temp_data[,2])[1]
  
  textpos_x <- temp_textpos_x
  textpos_y <-  ymin + temp_ycoef * yspan
  
  text_data <- data.frame(temp_lambda = temp_lambda, textpos_x = textpos_x, textpos_y = textpos_y)
  
  if(variable_1 == "age"){
  temp_plot <- temp_data %>%
    ggplot(aes(x = !!var1, y = !!var2)) +
    geom_point(alpha = 0.3, size = 0.5) +
    geom_text(data = text_data, aes(label = paste0("Lambda[n] == ", round(temp_lambda, 3)), x = textpos_x, y = textpos_y), size = 3, parse = T) +
    theme_bw(base_size = 8)
  } else {
  temp_plot <- temp_data %>%
    ggplot(aes(x = !!var1, y = !!var2)) +
    geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
    geom_text(data = text_data, aes(label = paste0("Lambda[n] == ", round(temp_lambda, 3)), x = textpos_x, y = textpos_y), size = 3, parse = T) +
    theme_bw(base_size = 8)
  }
  }
  return(temp_plot)
}

plot_frame <- pmap(.f = extract_plots, .l = my_grid)

my_data <- read_csv2("Data/NHANES_subsample.csv")

lambda_limit_grid <- expand.grid(variable_1 = "age", variable_2 = varnames_response)
lambda_limit_frame <- pmap_dfr(.f = extract_lambda, .l = lambda_limit_grid)

for(i in 1:length(varnames_response)){
  temp_data <- lambda_frame %>%
    filter(variable_2 == varnames_response[i]) %>%
    mutate(variable_1 = factor(variable_1, levels = c("age2", "age4", "age8", "age16", "age32"), labels = c(2, 4, 8, 16, 32)))
  
  temp_plot <- temp_data %>%
    ggplot(aes(x = variable_1, y = lambda)) +
    geom_point() +
    geom_hline(yintercept = lambda_limit_frame$lambda[lambda_limit_frame$variable_2 == varnames_response[i]]) +
    ylim(0, 1) +
    theme_bw()
  
  plot_frame <- list.append(plot_frame, temp_plot)
}


ggmatrix(plots = plot_frame, ncol = 6, nrow = 3, byrow = F, xAxisLabels = c("Age 2", "Age 4", "Age 8", "Age 16", "Age 32", "Age"), yAxisLabels = c("Systolic BP", "Diastolic BP", "BMI"))

ggsave("Figures/pairwise_plot_discrete.pdf", units = "mm", height = 170, width = 170)