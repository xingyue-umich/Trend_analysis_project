library(plotly)
library(latex2exp)
library(ggplot2)
# Merge into one reg df
reg_list <- import_list("./Regression/reg_sum.xlsx", which = c(1,2,3,4,5),
                             setclass = "tbl", rbind = TRUE)
reg_list_g5 <- import_list("reg_sum_0422.xlsx", which = c(1,2,3,4,5),
                        setclass = "tbl", rbind = TRUE)
reg_list_l5 <- import_list("reg_sum_l5_0422.xlsx", which = c(1,2,3,4,5),
                           setclass = "tbl", rbind = TRUE)

write.xlsx(reg_list
           , file = "./Regression/reg_list.xlsx"
           , sheetName = "reg_list")

write.xlsx(reg_list_g5
           , file = "reg_list_g5.xlsx"
           , sheetName = "reg_list")
write.xlsx(reg_list_l5
           , file = "reg_list_l5.xlsx"
           , sheetName = "reg_list")

# Import file
reg_sum <- read_excel("./Regression/reg_list.xlsx", sheet = "reg_list") %>% select(-c(1,2))
colnames(reg_sum)

reg_sum_g5 <- read_excel("reg_list_g5.xlsx", sheet = "reg_list")
colnames(reg_sum_g5)

reg_sum_g5 <- reg_sum_g5[,-c(1,2)]

reg_sum_l5 <- read_excel("reg_list_l5.xlsx", sheet = "reg_list")
colnames(reg_sum_l5)
reg_sum_l5 <- reg_sum_l5[,-c(1,2)]

# Plot
p1 <- ggplot(data = reg_sum, aes(x = mean_age_rel, y = mean_age_2)) +
  labs(x = expression(beta[(centered~age)]), y = expression(beta^{2}~(centered~age))) +
  geom_abline(intercept = 0, slope = 1/23, color = 'red') +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=0, linetype="dashed", color = "red") +
  scale_x_continuous(breaks = seq(-0.02,0.02,0.005)) +
  # scale_y_continuous(breaks = round(seq(min(reg_sum$mean_age_2), max(reg_sum$mean_age_2), by = 0.1),2)) +
  scale_y_continuous(limits = c(-0.0001,0.0004)) +
  geom_point(aes(shape = chem_cat, color = chem_cat), size = 3) +
  scale_shape_manual(values=c(0,1,2,3,4,5)) +
  guides(shape = guide_legend(order=1), color = "none")

print(p1)
ggplotly(p1)


# Finer axis
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


p_g5 <- ggplot(data = reg_sum_g5, aes(x = mean_age_rel, y = mean_age_2)) +
  labs(x = expression(beta[(centered~age)]), y = expression(beta^{2}~(centered~age))) +
  # geom_abline(intercept = 0, slope = 1/26.9, colour = 'red') +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=0, linetype="dashed", color = "red") +
  scale_x_continuous(limit = c(-0.02,0.02),breaks = seq(-0.02,0.02,0.005)) +
  scale_y_continuous(limit = c(-0.0005,0.0005),breaks = seq(-0.001,0.001,0.0001)) +
  geom_point(aes(shape = chem_cat, color = chem_cat), size = 3, show.legend = TRUE) +
  scale_shape_manual(values=c(0,1,2,3,4,5))
ggplotly(p_g5)
print(p_g5)

p_l5 <- ggplot(data = reg_sum_l5, aes(x = mean_age_rel, y = mean_age_2)) +
  xlab("??1") +
  ylab("??2") +
  # geom_abline(intercept = 0, slope = 1/26.9, colour = 'red') +
  geom_hline(yintercept=0, linetype="dashed", color = "red") +
  geom_vline(xintercept=0, linetype="dashed", color = "red") +
  scale_x_continuous(limit = c(-1.5,1.2)) +
  scale_y_continuous(limit = c(-0.03,0.12)) +
  geom_point(aes(shape = chem_cat, color = chem_cat), size = 3, show.legend = TRUE) +
  scale_shape_manual(values=c(0,1,2,3,4,5))
ggplotly(p_l5)

Time_trend <- ggplot(data = reg_list[reg_list$`sample_year_rel _Pr`< 0.05,]) +
  xlab("chem") +
  ylab("??(sample year)")+
  geom_bar(aes(x = reorder(chem,- sample_year_rel), y = sample_year_rel, fill = chem_cat), stat="identity") +
 # geom_errorbar(aes(x = chem, ymin=sample_year_rel - sd, ymax=sample_year_rel + sd)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(Time_trend)

# Gender seperated 

  
