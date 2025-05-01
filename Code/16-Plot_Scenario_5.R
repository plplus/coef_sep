library(tidyverse)
library(gtable)
library(gridExtra)

load("Simulation Results/Simu_5.RData")

results <- results %>%
  gather(key = "scenario", value = "estimate", tau_1:xi_4) %>%
  separate(scenario, into = c("functional", "scenario")) %>%
  mutate(n = factor(n, levels = c(30, 100, 500, 1000, 5000, 10000, 50000, 100000), labels = c("30", "100", "500", "1,000", "5,000", "10,000", "50,000", "100,000")))

# Example Values
set.seed(73649)
n <- 5000

temp_coefs <- solve(matrix(c(1,0,0,0, 1,1/4,1/16,1/(4^3), 1,3/4,(3/4)^2,(3/4)^3, 1,1,1,1), nrow = 4, byrow = T), c(0, 1, -1, 1/2))

x_sample <- runif(n)
  
y_sample_1 <- runif(n, min = -1, max = 1)

ymin_2 <- -abs(1 - 2 * x_sample)
ymax_2 <- -ymin_2
y_sample_2 <- runif(n, min = ymin_2 , max = ymax_2)

ymax_3 <- (-x_sample^2 + 2 * x_sample)^(0.9)
ymin_3 <- 0.5 * (-x_sample^2 + 2 * x_sample)^(0.9)
yhelp_3 <- ifelse(runif(n) < 0.5, 1, -1)
y_sample_3 <- yhelp_3 * runif(n, min = ymin_3, max = ymax_3)

y_sample_4 <- temp_coefs[4] * x_sample^3 + temp_coefs[3] * x_sample^2 + temp_coefs[2] * x_sample + temp_coefs[1] + runif(n, min = -0.5, max = 0.5)
  
temp_model <- lm(data = data.frame(x = x_sample, y = y_sample_4), formula = y ~ x)

# Scenario 1
# Create Example Plot 1
example_1_frame <- data.frame(x = x_sample, y = y_sample_1)

example_plot_1 <- example_1_frame %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.3, alpha = 0.3) +
  xlim(0, 1) +
  ylim(-1, 1) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("Example, n = 5,000")) +
  theme_bw(base_size = 6)

# Create Boxplots 1
boxplot_1_frame <- results %>%
  filter(scenario == 1)

boxplot_1 <- boxplot_1_frame %>%
  ggplot(aes(x = n, y = estimate, fill = functional)) +
  geom_boxplot(outlier.size = 0.3) +
  ylab("Estimate") +
  facet_wrap(~1, labeller = label_bquote("Boxplots")) +
  scale_fill_discrete(name = "Coefficient", breaks = c("tau", "xi"), labels = c(bquote(Lambda[n](Y~"|"~X)), bquote("Chatterjee's"~xi[n]))) +
  theme_bw(base_size = 6)

# Scenario 2
# Create Example Plot 2
example_2_frame <- data.frame(x = x_sample, y = y_sample_2)

example_plot_2 <- example_2_frame %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.3, alpha = 0.3) +
  xlim(0, 1) +
  ylim(-1, 1) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("Example, n = 5,000")) +
  theme_bw(base_size = 6)

# Create Boxplots 2
boxplot_2_frame <- results %>%
  filter(scenario == 2)

boxplot_2 <- boxplot_2_frame %>%
  ggplot(aes(x = n, y = estimate, fill = functional)) +
  geom_boxplot(outlier.size = 0.3) +
  ylab("Estimate") +
  facet_wrap(~1, labeller = label_bquote("Boxplots")) +
  scale_fill_discrete(name = "Coefficient", breaks = c("tau", "xi"), labels = c(bquote(Lambda[n](Y~"|"~X)), bquote("Chatterjee's"~xi[n]))) +
  theme_bw(base_size = 6)

# Scenario 3
# Create Example Plot 3
example_3_frame <- data.frame(x = x_sample, y = y_sample_3)

example_plot_3 <- example_3_frame %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(size = 0.3, alpha = 0.3) +
  xlim(0, 1) +
  ylim(-1, 1) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("Example, n = 5,000")) +
  theme_bw(base_size = 6)

# Create Boxplots 3
boxplot_3_frame <- results %>%
  filter(scenario == 3)

boxplot_3 <- boxplot_3_frame %>%
  ggplot(aes(x = n, y = estimate, fill = functional)) +
  geom_boxplot(outlier.size = 0.3) +
  ylab("Estimate") +
  facet_wrap(~1, labeller = label_bquote("Boxplots")) +
  scale_fill_discrete(name = "Coefficient", breaks = c("tau", "xi"), labels = c(bquote(Lambda[n](Y~"|"~X)), bquote("Chatterjee's"~xi[n]))) +
  theme_bw(base_size = 6)

# Scenario 4
# Create Example Plot 4
# Plot Modell
example_4_frame <- data.frame(x = x_sample, y = y_sample_4, resid = temp_model$residuals)
example_4_frame$pred <- predict(temp_model, newdata = example_4_frame)

example_plot_4 <- example_4_frame %>%
  ggplot() +
  geom_point(aes(x = x, y = y), size = 0.3, alpha = 0.3) +
  geom_line(aes(x = x, y = pred), col = "red") +
  xlim(0, 1) +
  ylim(-1.5, 1.5) +
  xlab("X") +
  ylab("Y") +
  facet_wrap(~1, labeller = label_bquote("Example, n = 5,000")) +
  theme_bw(base_size = 6)

# Create Boxplots 4
boxplot_4_frame <- results %>%
  filter(scenario == 4)

boxplot_4 <- boxplot_4_frame %>%
  ggplot(aes(x = n, y = estimate, fill = functional)) +
  geom_boxplot(outlier.size = 0.3) +
  ylab("Estimate") +
  facet_wrap(~1, labeller = label_bquote("Boxplots")) +
  scale_fill_discrete(name = "Coefficient", breaks = c("tau", "xi"), labels = c(bquote(Lambda[n](Y~"|"~X)), bquote("Chatterjee's"~xi[n]))) +
  theme_bw(base_size = 6)

example_plot_1 <- ggplotGrob(example_plot_1)
example_plot_2 <- ggplotGrob(example_plot_2)
example_plot_3 <- ggplotGrob(example_plot_3)
example_plot_4 <- ggplotGrob(example_plot_4)

boxplot_1 <- ggplotGrob(boxplot_1)
boxplot_2 <- ggplotGrob(boxplot_2)
boxplot_3 <- ggplotGrob(boxplot_3)
boxplot_4 <- ggplotGrob(boxplot_4)

layout_mat <- matrix(list(example_plot_1, example_plot_2, example_plot_3, example_plot_4, boxplot_1, boxplot_2, boxplot_3, boxplot_4), nrow = 4)

my_table <- gtable_matrix(name = "test", grobs = layout_mat, widths = unit(c(5, 10), "cm"), heights = unit(c(5, 5, 5, 5), "cm"))

ggsave(plot = my_table, filename = "Figures/Heteroscedasticity.pdf", height = 200, width = 150, units = "mm")