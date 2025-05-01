library(tidyverse)
library(gtable)
library(gridExtra)

source("Code/00-Core_Functions.R")

load("Simulation Results/Simu_4a.RData")

results <- results %>%
  mutate(n = factor(n, levels = c(100, 500, 1000, 5000, 10000, 50000), labels = c("100", "500", "1,000", "5,000", "10,000", "50,000"))) %>%
  mutate(sigma = factor(sigma, levels = c(1, 0.5, 0.25, 0.125, 0)))

# Example Values
set.seed(73649)

n <- 5000
sigma <- 0.5

x_1_sample <- sample(x = c(1, 2), size = n, replace = T)
x_2_sample <- sample(x = c(1, 2), size = n, replace = T)

x_sample <- data.frame(x_1 = x_1_sample, x_2 = x_2_sample)

x_group <- case_when(x_1_sample == 1 & x_2_sample == 1 ~ 1,
                     x_1_sample == 1 & x_2_sample == 2 ~ 2,
                     x_1_sample == 2 & x_2_sample == 1 ~ 3,
                     x_1_sample == 2 & x_2_sample == 2 ~ 4)

y_sample <- runif(min = x_group - 1, max = x_group, n = length(x_group))
y_sample <- y_sample + runif(min = -sigma, max = sigma, n = length(y_sample))
est <- tau_fast_Z(X = x_sample, Y = y_sample)


# Plot Example
example_frame <- data.frame(x_1 = x_1_sample, x_2 = x_2_sample, y = y_sample) %>%
  mutate(x_1 = factor(x_1, levels = c(1, 2))) %>%
  mutate(x_2 = factor(x_2, levels = c(1, 2))) %>%
  mutate(x = case_when(x_1 == 1 & x_2 == 1 ~ "(1, 1)",
                       x_1 == 1 & x_2 == 2 ~ "(1, 2)",
                       x_1 == 2 & x_2 == 1 ~ "(2, 1)",
                       x_1 == 2 & x_2 == 2 ~ "(2, 2)")) %>%
  mutate(x = factor(x, levels = c("(1, 1)", "(1, 2)", "(2, 1)", "(2, 2)")))

example_plot <- example_frame %>%
  ggplot() +
  geom_boxplot(aes(x = x, y = y)) +
  xlab("X") +
  facet_wrap(~1, labeller = label_bquote("Example, n = 5,000, "~sigma~" = 0.5")) +
  ylab("Y") +
  theme_bw(base_size = 6)

example_plot

# Boxplot
boxplot <- results %>%
  ggplot(aes(x = n, y = tau, fill = sigma)) +
  geom_boxplot(outlier.size = 0.3) +
  ylab(bquote(Lambda[n]~"("~Y~"|"~X~")")) +
  facet_wrap(~1, labeller = label_bquote("Boxplots")) +
  scale_fill_discrete(name = bquote(sigma)) +
  theme_bw(base_size = 6)

boxplot

example_plot <- ggplotGrob(example_plot)
boxplot <- ggplotGrob(boxplot)

layout_mat <- matrix(c(1, 2), nrow = 1, byrow = T)

my_table <- grid.arrange(grobs = list(example_plot, boxplot), widths = c(5, 10), layout = layout_mat)

ggsave(plot = my_table, filename = "Figures_09_01_2025/Two_Dimensional_A.pdf", height = 50, width = 150, units = "mm")
