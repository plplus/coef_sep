library(tidyverse)
library(GGally)

source("Code/00-Core_Functions.R")

my_data <- read_csv2("Data/NHANES_subsample.csv") %>%
  select(-age2, -age4, -age8, -age16, -age32)

my_data2 <- my_data %>%
  gather(key = "variable_1", value = "value_1", -SEQN)

my_data2 <- my_data2 %>%
  left_join(my_data)

my_data2 <- my_data2 %>%
  gather(key = "variable_2", value = "value_2", -SEQN, -variable_1, -value_1)

my_data2 %>%
  ggplot(aes(x = value_1, y = value_2)) +
  geom_point() +
  geom_smooth() +
  facet_grid(variable_2 ~ variable_1, scales = "free") +
  theme_bw()


varnames <- c("age", "bp_systolic", "bp_diastolic", "bmi")


extract_lambda <- function(variable_1, variable_2){
  temp_data <- my_data %>%
    select(all_of(c(variable_1, variable_2)))
  
  
  set.seed(85729)
  estimate <- ifelse(ncol(temp_data) == 2, tau_fast_Z(X = unlist(temp_data[,1]), Y = unlist(temp_data[,2])), 1)
  
  return(data.frame(variable_1 = variable_1, variable_2 = variable_2, lambda = estimate))
}

my_grid <- expand_grid(variable_1 = varnames, variable_2 = varnames)

lambda_frame <- pmap_dfr(.f = extract_lambda, .l = my_grid)

lambda_frame

lambda_frame$xcoef <- c(0, 0.2, 0.2, 0.2,
                        0.8, 0, 0.2, 0.2,
                        0.8, 0.8, 0, 0.2,
                        0.8, 0.8, 0.8, 0)

lambda_frame$ycoef <- c(0, 0.975, 0.975, 0.975,
                        0.025, 0, 0.975, 0.975,
                        0.025, 0.025, 0, 0.975,
                        0.025, 0.025, 0.025, 0)


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
  
  temp_xcoef <- lambda_frame %>%
    filter(variable_1 == .env$variable_1, variable_2 == .env$variable_2) %>%
    select(xcoef) %>%
    unlist()
  
  temp_ycoef <- lambda_frame %>%
    filter(variable_1 == .env$variable_1, variable_2 == .env$variable_2) %>%
    select(ycoef) %>%
    unlist()
  
  xspan <- range(temp_data[,1])[2] - range(temp_data[,1])[1]
  xmin <- range(temp_data[,1])[1]
  
  yspan <- range(temp_data[,2])[2] - range(temp_data[,2])[1]
  ymin <- range(temp_data[,2])[1]
  
  textpos_x <-  xmin + temp_xcoef * xspan
  textpos_y <-  ymin + temp_ycoef * yspan
  
  text_data <- data.frame(temp_lambda = temp_lambda, xmin = xmin, ymin = ymin)
  
  temp_plot <- temp_data %>%
    ggplot(aes(x = !!var1, y = !!var2)) +
    geom_point(alpha = 0.3, size = 0.3) +
    geom_smooth(se = F) +
    geom_text(data = text_data, aes(label = paste0("Lambda[n] == ", round(temp_lambda, 3)), x = textpos_x, y = textpos_y), size = 2, parse = T) +
    theme_bw(base_size = 8)
    
  } else{
    temp_plot <- temp_data %>%
      ggplot(aes(x = !!var1)) +
      geom_histogram() +
      theme_bw(base_size = 8)
  }
  
  
  return(temp_plot)
}

plot_frame <- pmap(.f = extract_plots, .l = my_grid)

ggmatrix(plots = plot_frame, ncol = 4, nrow = 4, byrow = F, xAxisLabels = c("Age", "Systolic BP", "Diastolic BP", "BMI"), yAxisLabels = c("Age", "Systolic BP", "Diastolic BP", "BMI"))

ggsave("Figures/pairwise_plot.pdf", units = "mm", height = 170, width = 170)