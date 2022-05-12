library(plotly)
library(latex2exp)
library(ggplot2)

# Import file
reg_sum <- read_excel("../../Data/Regression/reg_sum.xlsx", sheet = "reg_list") 
colnames(reg_sum)


# Plot
p1 <- ggplot(data = reg_sum, aes(x = mean_age_rel, y = mean_age_2)) +
  xlab("b1") +
  ylab("b2") +
  geom_abline(intercept = 0, slope = 1/23, colour = 'red') +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=0, linetype="dashed", color = "red") +
  scale_x_continuous(limit = c(-0.02,0.02),breaks = seq(-0.02,0.02,0.005)) +
  scale_y_continuous(limit = c(-0.0005,0.0005),breaks = seq(-0.001,0.001,0.0001)) +
  geom_point(aes(shape = chem_cat, color = chem_cat), size = 3, show.legend = TRUE) +
  scale_shape_manual(values=c(0,1,2,3,4,5)) +
  geom_abline(intercept = 0.000569, slope = 1/23, color = "grey") +
  geom_abline(intercept = -0.00057, slope = 1/23, color = "grey")
  
ggplotly(p1)
  
