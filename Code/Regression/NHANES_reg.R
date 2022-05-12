library("ggplot2")
library("tidyr")
library("ggpmisc")
library("stringr")
source("../code/functions.R")
setwd("~/Documents/Research/Data")

# Import file
NHANES <- read.csv(file = "../../Data/NHANES/NHANES - Mean PFAS Concentrations by Age, Gender, and Cycle Year.csv") %>% 
  select(-1)
colnames(NHANES)[2] <- 'mean_age'

# Calculate age group
NHANES <- age_group(NHANES)
NHANES$age_group <- factor(NHANES$age_group,levels = c("0-5","5-15","15-30","30-45","45-60",">60"))
colnames(NHANES)

# Use actual sample year
NHANES$sample_year <- NHANES$SDDSRVYR * 2 + 1997

# Calculate mean age for pool samples
NHANES$total_age_pooled <- NHANES$mean_age * NHANES$num_participants
NHANES$total_mean_pooled <- NHANES$mean * NHANES$num_participants

pooled <- NHANES %>% group_by(chemical_codename_use, SDDSRVYR, gender, age_group, sample_year) %>% 
  summarise(num = sum(num_participants), 
            mean_age_pooled = sum(total_age_pooled)/sum(num_participants),
            mean_pooled = sum(total_mean_pooled)/sum(num_participants))


NHANES <- left_join(NHANES, pooled, by = c("chemical_codename_use","SDDSRVYR", "gender", "age_group","sample_year"))

# Use log value for measurement
NHANES$log_value <- log10(NHANES$mean_pooled)

# NHANES <- NHANES %>% mutate(sex_rel = ifelse(gender == 'females',1,0))
# NHANES$mean_age_rel <- NHANES$mean_age_pooled - mean(NHANES$mean_age_pooled)
# NHANES$mean_age_2 <- (NHANES$mean_age_pooled)^2
# NHANES$sample_year_rel <- NHANES$SDDSRVYR
# NHANES$sample_year_2 <- (NHANES$sample_year_rel)*(NHANES$sample_year_rel)
# 
# NHANES_reg_list <- NHANES %>% 
#   group_by(chem) %>% 
#   (mean(sample_year_rel) != 8) %>% 
#   ungroup() %>% 
#   split(f=.$chem)
# 
#   
# reg_estimate <- lapply(NHANES_reg_list, function(x){
#       reg_i <-  lm(mean ~ sex_rel + mean_age_rel + mean_age_2 + sample_year_2 + sample_year_rel,
#                    data = x)
#       return(summary(reg_i)$coefficient[,1])})
#   
# estimate_df <- as.data.frame(do.call("rbind",reg_estimate))
# estimate_df$chem <- row.names(estimate_df)
#   
# rownames(estimate_df) <- NULL
# 
# p1 <- ggplot(data = estimate_df, aes(x = mean_age_rel, y = mean_age_2)) +
#   geom_hline(yintercept=0, linetype="dashed", color = "red") +
#   geom_vline(xintercept=0, linetype="dashed", color = "red") +
#   geom_point(aes(color = chem), size = 3)
# 
# print(p1)
# ggplotly(p1)
# 
# p2 <- ggplot(data = estimate_df, aes(x = mean_age_rel, y = mean_age_2)) +
#   geom_hline(yintercept=0, linetype="dashed", color = "red") +
#   geom_vline(xintercept=0, linetype="dashed", color = "red") +
#   scale_x_continuous(limit = c(-0.015,0.015), breaks = seq(-0.015,0.015,0.005)) +
#   scale_y_continuous(limits = c(-0.0002,0.0004)) +
#   geom_point(aes(color = chem), size = 3)
# 
# print(p2)
# ggplotly(p2)

### Plot Trends
# Subset PFOA, PFNA, PFHxS, PFOS 
NHANES <- NHANES %>% filter(cas_num %in% c('335-67-1','375-95-1','355-46-4','1763-23-1'))

# Remove VNURX..., LBXB
NHANES <- NHANES %>% filter(str_detect(chemical_codename_use,'^LBXP'))

NHANES$chem <- case_when(
  NHANES$cas_num == '335-67-1' ~ 'PFOA',
  NHANES$cas_num == '375-95-1' ~ 'PFNA',
  NHANES$cas_num == '355-46-4' ~ 'PFHxS',
  NHANES$cas_num == '1763-23-1' ~ 'PFOS'
)

NHANES$chem <- factor(NHANES$chem,levels = c('PFOA','PFNA','PFHxS','PFOS'))
# 
# NHANES_list <- NHANES %>% split(f=.$chem)
# NHANES_1 <- bind_rows(NHANES_list[1:4]) 
# NHANES_2 <- bind_rows(NHANES_list[6:10])
# NHANES_3 <- bind_rows(NHANES_list[11:15])
# NHANES_4 <- bind_rows(NHANES_list[16:20])
# NHANES_5 <- bind_rows(NHANES_list[21:25])
# NHANES_6 <- bind_rows(NHANES_list[26:31])

# NHANES_list <- list(NHANES_1, NHANES_2, NHANES_3, NHANES_4, NHANES_5, NHANES_6)
NHANES_pooled <- NHANES[, c('chemical_codename_use','gender','sample_year','chem','cas_num','age_group','num','mean_age_pooled','mean_pooled','log_value')]
NHANES_pooled <- unique(NHANES_pooled)
NHANES_list <- list(NHANES_pooled)


p3 <- lapply(NHANES_list,function(x){
    ggplot(x, aes(x= as.factor(sample_year), group = age_group)) +
      xlab("sample year") +
      ylab("log(value)") +
      geom_point(aes(y = log_value, color = gender), size = 1, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=16) +
      stat_smooth(aes(y = log_value, group = gender, color = gender),linetype = "twodash",size = 0.65,
                  method = "lm", formula = y ~ x + I(x^2), se = F, show.legend = FALSE) +
      facet_grid(chem ~ age_group, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})


pdf("../Plots/NHANES_subset_year.pdf")
print(p3)
dev.off() 

p4 <- lapply(NHANES_list,function(x){
  ggplot(x, aes(x= as.numeric(mean_age_pooled), group = sample_year)) +
    xlab("mean age") +
    ylab("log(value)") +
    geom_point(aes(y = log_value, color = gender), size = 1, show.legend = TRUE) +
    scale_color_manual(values=c("turquoise","tomato")) +
    scale_shape_manual(values=16) +
    stat_smooth(aes(y = log_value, group = gender, color = gender),linetype = "twodash",size = 0.65,
                method = "lm", formula = y ~ x + I(x^2), se = F, show.legend = FALSE) +
    facet_grid(chem ~ sample_year, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})

pdf("../Plots/NHANES_subset_age.pdf")
print(p4)
dev.off() 
