library(tidyverse)
library(gtable)
library(scales)

load("Simulation Results/Simu_2.RData")

real_values <- read_csv2("Simulation Results/Simu_2_Real_Values.csv")
real_values <- real_values %>%
  mutate(subscen = factor(subscen, levels = c("A", "B")))

results <- results %>%
  gather(key = "type", value = "estimator", tau_nn_z, tau_conventional)

# Scenario A
set.seed(1)

n_1 <- 250
n_2 <- 250
mean_1 <- 0
mean_2 <- 1
sd_1 <- 2
sd_2 <- 1

x_sample <- rep(c(0, 1), times = c(n_1, n_2))

y_sample <- c(rnorm(n_1, mean_1, sd_1), rnorm(n_2, mean_2, sd_2))

example_data <- data.frame(x = x_sample, y = y_sample)

example_plot <- example_data %>%
  mutate(x = factor(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_violin(size = 0.5) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("Scenario 2A, n = 500")) +
  theme_bw(base_size = 6)

results_plot <- results %>%
  filter(subscen == "A") %>%
  mutate(n = factor(n, levels = c(30, 100, 500, 1000, 5000, 10000, 50000), labels = c("30", "100", "500", "1,000", "5,000", "10,000", "50,000"))) %>%
  mutate(type = factor(type, levels = c("tau_nn_z", "tau_conventional"), labels = c("Lambda[n]", "hat(Lambda)[n]"))) %>%
  ggplot(aes(x = n, y = estimator, fill = type)) +
  geom_boxplot(outlier.size = 0.3) +
  geom_hline(yintercept = 0) +
  geom_hline(data = real_values %>% filter(subscen == "A"), aes(yintercept = real_value), lty = 2) +
  facet_wrap(~1, labeller = label_bquote("Results")) +
  ylab("Estimate") +
  scale_fill_discrete(name = "Estimator", labels = parse_format()) +
  theme_bw(base_size = 6)

example_plot <- ggplotGrob(example_plot)
results_plot <- ggplotGrob(results_plot)
layout_mat <- matrix(list(example_plot, results_plot), nrow = 1)

my_table <- gtable_matrix(name = "test", grobs = layout_mat, widths = unit(c(5, 10), "cm"), heights = unit(c(5), "cm"))
ggsave(plot = my_table, filename = "Figures/Simu_2A_Results.pdf", height = 50, width = 150, units = "mm")

# Scenario B
set.seed(1)

n_1 <- 250
n_2 <- 250
mean_1 <- 0
mean_2 <- 1
sd_1 <- 2
sd_2 <- 2

x_sample <- rep(c(0, 1), times = c(n_1, n_2))

y_sample <- c(rnorm(n_1, mean_1, sd_1), rnorm(n_2, mean_2, sd_2))

example_data <- data.frame(x = x_sample, y = y_sample)

example_plot <- example_data %>%
  mutate(x = factor(x)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_violin(size = 0.5) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("Scenario 2B, n = 500")) +
  theme_bw(base_size = 6)

results_plot <- results %>%
  filter(subscen == "B") %>%
  mutate(n = factor(n, levels = c(30, 100, 500, 1000, 5000, 10000, 50000), labels = c("30", "100", "500", "1,000", "5,000", "10,000", "50,000"))) %>%
  mutate(type = factor(type, levels = c("tau_nn_z", "tau_conventional"), labels = c("Lambda[n]", "hat(Lambda)[n]"))) %>%
  ggplot(aes(x = n, y = estimator, fill = type)) +
  geom_boxplot(outlier.size = 0.3) +
  geom_hline(yintercept = 0) +
  geom_hline(data = real_values %>% filter(subscen == "B"), aes(yintercept = real_value), lty = 2) +
  facet_wrap(~1, labeller = label_bquote("Results")) +
  ylab("Estimate") +
  scale_fill_discrete(name = "Estimator", labels = parse_format()) +
  theme_bw(base_size = 6)

example_plot <- ggplotGrob(example_plot)
results_plot <- ggplotGrob(results_plot)
layout_mat <- matrix(list(example_plot, results_plot), nrow = 1)

my_table <- gtable_matrix(name = "test", grobs = layout_mat, widths = unit(c(5, 10), "cm"), heights = unit(c(5), "cm"))
ggsave(plot = my_table, filename = "Figures/Simu_2B_Results.pdf", height = 50, width = 150, units = "mm")