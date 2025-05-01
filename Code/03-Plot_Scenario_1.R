library(tidyverse)
library(mvtnorm)
library(gtable)

load("Simulation Results/Simu_1.RData")

results <- results %>%
  mutate(n = factor(n, levels = c(30, 100, 500, 1000, 5000, 10000, 50000), labels = c("30", "100", "500", "1,000", "5,000", "10,000", "50,000")))

real_values <- read_csv2("Simulation Results/Simu_1_Real_Values.csv")
real_values <- real_values %>%
  mutate(rho = factor(rho, levels = c(0, 0.4, 0.75, 1)))

n <- 500
rho <- 0.4

set.seed(1)
temp_sample <- rmvnorm(n = n, mean = c(0, 0), sigma = matrix(c(1, rho, rho, 1), nrow = 2, byrow = T))

x_sample <- temp_sample[,1]
y_sample <- temp_sample[,2]

example_data <- data.frame(x = x_sample, y = y_sample)

example_plot <- example_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("rho = 0.4, n = 500")) +
  theme_bw(base_size = 6)


results_plot <- results %>%
  mutate(rho = factor(rho), n = factor(n)) %>%
  ggplot(aes(x = n, y = tau, fill = rho)) +
  geom_boxplot(outlier.size = 0.3) +
  geom_hline(data = real_values, aes(yintercept = real_value, col = rho), lty = 2) +
  facet_wrap(~1, labeller = label_bquote("Results")) +
  ylab(bquote(Lambda[n]~"("~Y~"|"~X~")")) +
  theme_bw(base_size = 6)

example_plot <- ggplotGrob(example_plot)
results_plot <- ggplotGrob(results_plot)
layout_mat <- matrix(list(example_plot, results_plot), nrow = 1)

my_table <- gtable_matrix(name = "test", grobs = layout_mat, widths = unit(c(5, 10), "cm"), heights = unit(c(5), "cm"))
ggsave(plot = my_table, filename = "Figures/Simu_1_Results.pdf", height = 50, width = 150, units = "mm")
plot(my_table)