library(tidyverse)
library(gtable)
library(gridExtra)


load("Simulation Results/Simu_4b.RData")

results <- results %>%
  select(-tau_1) %>%
  rename(tau = tau_2) %>%
  mutate(n = factor(n, levels = c(30, 100, 500, 1000, 5000, 10000, 50000, 100000), labels = c("30", "100", "500", "1,000", "5,000", "10,000", "50,000", "100,000")))

# Example Values
set.seed(73649)

n <- 5000

x_1_sample <- sample(x = c(-1, 1), size = n, replace = T)
x_2_sample <- runif(n = n, min = 0, max = 2*pi)

x_sample <- data.frame(x_1 = x_1_sample, x_2 = x_2_sample)

y_sample <- x_1_sample * cos(x_1_sample * x_2_sample)

y_sample <- y_sample + runif(min = -0.5, max = 0.5, n = nrow(x_sample))

# Plot Example 1
example_frame <- data.frame(x_1 = x_1_sample, x_2 = x_2_sample, y = y_sample) %>%
  mutate(x_1 = factor(x_1, levels = c(-1, 1)))

example_plot_1 <- example_frame %>%
  ggplot() +
  geom_point(aes(x = x_1, y = y, col = x_1), size = 0.5, alpha = 0.3) +
  xlab("X") +
  facet_wrap(~1, labeller = label_bquote("Example, n = 5,000")) +
  scale_color_discrete(name = "X") +
  ylab("Y") +
  theme_bw(base_size = 6) +
  theme(legend.position = "bottom")

example_plot_1

# Plot Example 2

example_plot_2 <- example_frame %>%
  ggplot() +
  geom_point(aes(x = x_2, y = y, col = x_1), size = 0.5, alpha = 0.3) +
  xlab("Z") +
  facet_wrap(~1, labeller = label_bquote("Example, n = 5,000")) +
  scale_color_discrete(name = "X") +
  ylab("Y") +
  theme_bw(base_size = 6) +
  theme(legend.position = "bottom")

example_plot_2

# Boxplot
boxplot <- results %>%
  ggplot(aes(x = n, y = tau)) +
  geom_boxplot(outlier.size = 0.3) +
  ylab(bquote(Lambda[n]~"("~Y~"|"~X~")")) +
  facet_wrap(~1, labeller = label_bquote("Boxplots")) +
  theme_bw(base_size = 6)

example_plot_1 <- ggplotGrob(example_plot_1)
example_plot_2 <- ggplotGrob(example_plot_2)
boxplot <- ggplotGrob(boxplot)

layout_mat <- matrix(c(1, 2, 3), nrow = 1, byrow = T)

my_table <- grid.arrange(grobs = list(example_plot_1, example_plot_2, boxplot), widths = unit(c(5, 5, 5), "cm"), heights = unit(c(5), "cm"), layout_matrix = layout_mat)

ggsave(plot = my_table, filename = "Figures/Two_Dimensional_B.pdf", height = 50, width = 150, units = "mm")