library(tidyverse)
library(gtable)
library(scales)

load("Simulation Results/Simu_3.RData")

real_values <- read_csv2("Simulation Results/Simu_3_Real_Values.csv")
real_values <- real_values %>%
  mutate(nsplits = factor(nsplits, levels = c(4, 8, 16, 32, 64, 128, 256)))

results <- results %>%
  gather(key = "type", value = "estimator", tau_nn, tau_conventional)

# First Example
n <- 256
nsplits_1 <- 8

set.seed(1)
x_sample_1 <- runif(n = n, min = -1, max = 1)

y_sample_1 <- x_sample_1^2 + rnorm(n = n, mean = 0, sd = 0.25)

# discretize x
x_sample_1 <- ((x_sample_1 + 1) / 2 * nsplits_1) %/% 1

example_data_1 <- data.frame(x = x_sample_1, y = y_sample_1)

example_plot_1 <- example_data_1 %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("k = 8, n = 256")) +
  theme_bw(base_size = 6)

results_plot_1 <- results %>%
  mutate(nsplits = factor(nsplits)) %>%
  mutate(type = factor(type, levels = c("tau_nn", "tau_conventional"), labels = c("Lambda[n]", "hat(Lambda)[n]"))) %>%
  ggplot(aes(x = type, y = estimator, fill = nsplits)) +
  geom_boxplot(outlier.size = 0.2) +
  geom_hline(data = real_values, aes(yintercept = real_value), lty = 2) +
  facet_wrap(~1, labeller = label_bquote("Results for n = 256")) +
  ylab(bquote(Lambda~"("~Y~"|"~X~")")) +
  scale_fill_discrete(name = "k") +
  scale_color_discrete(name = "k") +
  scale_x_discrete(name = "Estimator", labels = parse_format()) +
  ylab("Estimate") +
  ylim(0, 1) +
  theme_bw(base_size = 6)

example_plot_1 <- ggplotGrob(example_plot_1)
results_plot_1 <- ggplotGrob(results_plot_1)
layout_mat <- matrix(list(example_plot_1, results_plot_1), nrow = 1)

my_table <- gtable_matrix(name = "test", grobs = layout_mat, widths = unit(c(5, 10), "cm"), heights = unit(c(5), "cm"))
ggsave(plot = my_table, filename = "Figures/Simu_3_Results.pdf", height = 50, width = 150, units = "mm")
