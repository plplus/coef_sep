library(tidyverse)
source("Core_Functions.R")
library(gtable)
library(GGally)

set.seed(172045)
n <- 512
x_sample <- runif(n, min = -1, max = 1)

y_sample <- -x_sample^2 + runif(n, min = -0.1, max = 0.1)

my_data <- data.frame(x = x_sample, y = y_sample)

my_data <- my_data %>%
  mutate(x2 = if_else(x == max(x), 1, floor((x - min(x)) / (max(x) - min(x)) * 2))) %>%
  mutate(x3 = if_else(x == max(x), 2, floor((x - min(x)) / (max(x) - min(x)) * 3))) %>%
  mutate(x4 = if_else(x == max(x), 3, floor((x - min(x)) / (max(x) - min(x)) * 4))) %>%
  mutate(x8 = if_else(x == max(x), 7, floor((x - min(x)) / (max(x) - min(x)) * 8)))
  #mutate(x8 = floor((x - min(x)) / (max(x) - min(x)) * 8)) %>%
  #mutate(x32 = floor((x - min(x)) / (max(x) - min(x)) * 32)) %>%
  #mutate(x128 = floor((x - min(x)) / (max(x) - min(x)) * 128)) %>%

varnames <- c("x2", "x3", "x4", "x8")

extract_lambda <- function(variable){
  temp_data <- my_data %>%
    select(all_of(c(variable, "y")))
  
  estimate <- ifelse(ncol(temp_data) == 2, tau_fast_Z(X = unlist(temp_data[,1]), Y = unlist(temp_data[,2])), 1)
  
  return(data.frame(variable = variable, lambda = estimate))
}

lambda_frame <- map_dfr(.f = extract_lambda, .x = c(varnames, "x"))

my_data <- my_data %>%
  mutate(across(c(x2, x3, x4, x8), .fns = function(x) factor(x, levels = 0:(length(unique(x)) - 1))))

plot_frame <- list()

plot_frame[[1]] <- my_data %>%
  ggplot(aes(x = x2, y = y)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  facet_wrap(~1, labeller = label_bquote(Lambda[n]==.(round(unlist(lambda_frame[1,2]), 3)))) +
  theme_bw(base_size = 8) +
  xlab("X") +
  ylab("Y") +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_frame[[2]] <- my_data %>%
  ggplot(aes(x = x3, y = y)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  facet_wrap(~1, labeller = label_bquote(Lambda[n]==.(round(unlist(lambda_frame[2,2]), 3)))) +
  theme_bw(base_size = 8) +
  xlab("X") +
  ylab(NULL) +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_frame[[3]] <- my_data %>%
  ggplot(aes(x = x4, y = y)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  facet_wrap(~1, labeller = label_bquote(Lambda[n]==.(round(unlist(lambda_frame[3,2]), 3)))) +
  theme_bw(base_size = 8) +
  xlab("X") +
  ylab(NULL) +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_frame[[4]] <- my_data %>%
  ggplot(aes(x = x8, y = y)) +
  geom_boxplot(outlier.alpha = 0.3, outlier.size = 0.5) +
  facet_wrap(~1, labeller = label_bquote(Lambda[n]==.(round(unlist(lambda_frame[4,2]), 3)))) +
  theme_bw(base_size = 8) +
  xlab("X") +
  ylab(NULL) +
  scale_x_discrete(breaks = NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_frame[[5]] <- my_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5, alpha = 0.3) +
  facet_wrap(~1, labeller = label_bquote(Lambda[n]==.(round(unlist(lambda_frame[5,2]), 3)))) +
  theme_bw(base_size = 8) +
  xlab("X") +
  ylab(NULL) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_frame[[1]] <- ggplotGrob(plot_frame[[1]])
plot_frame[[2]] <- ggplotGrob(plot_frame[[2]])
plot_frame[[3]] <- ggplotGrob(plot_frame[[3]])
plot_frame[[4]] <- ggplotGrob(plot_frame[[4]])
plot_frame[[5]] <- ggplotGrob(plot_frame[[5]])


layout_mat <- matrix(list(plot_frame[[1]], plot_frame[[2]], plot_frame[[3]], plot_frame[[4]], plot_frame[[5]]), nrow = 1)

my_table <- gtable_matrix(name = "test", grobs = layout_mat, widths = unit(c(4, 4, 4, 4, 4), "cm"), heights = unit(c(4), "cm"))
ggsave(plot = my_table, filename = "Figures_09_01_2025/Intro.pdf", height = 40, width = 200, units = "mm")

plot(my_table)
my_table
