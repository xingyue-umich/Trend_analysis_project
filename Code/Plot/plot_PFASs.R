setwd("~/Documents/Research/Data")
library("ggplot2")
library('plotly')
library("tidyr")
library("dplyr")
library("ggpmisc")
library("plyr")
library("readxl")
library("xlsx")
library('stringr')
source("../code/functions.R")


# Import file
PFASs_merged <- read_excel("./PFAS/PFASs_merged.xlsx", sheet = "PFASs_merged") %>% select(-1)

######################### first check #############################
# check colnames and types
colnames(PFASs_merged)
str(PFASs_merged)

# # Drop '_file' column
# PFASs_merged <- PFASs_merged %>% select(-c("_file","_PFASs","lab_sample_code2_PFASs","WEIGHT_SAMPLE_PFASs",
#                                          "sample_code_original_PFASs","gender_PFASs","number_in_pool_PFASs"))

# Drop rows where sample year = 0, those are cord blood or bovine or sample id starts with 999
PFASs_merged <- PFASs_merged %>% subset(!(sample_year == 0))

# Convert mean age, sample_year column to numeric
PFASs_merged$mean_age <- as.numeric(PFASs_merged$mean_age)
PFASs_merged$sample_year <- as.numeric(PFASs_merged$sample_year)
# Delete mean age is NA (bovine or cord blood)
PFASs_merged <- PFASs_merged %>% subset(!is.na(mean_age))

# create age_group according to the mean_age(fun)
PFASs_merged <- age_group(PFASs_merged)

######################### Subset children #############################
# Use average age for samples below 5 y/o
# PFASs_l5 <- PFASs %>% 
#   filter(mean_age < 5.01) %>%
#   group_by(sample_year,gender) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   select(-c('sample_id','region','mean_age_PFASs','collection_period_PFASs'))
# 
# PFASs_l15 <- PFASs %>% 
#   filter(mean_age >= 5.01 & mean_age <= 15.01) %>%
#   group_by(sample_year,gender) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) %>%
#   select(-c('sample_id','region','mean_age_PFASs','collection_period_PFASs'))
# 
# PFASs_g15 <- PFASs %>% filter(mean_age > 15.01) %>%
#   select(-c('sample_id','region','mean_age_PFASs','collection_period_PFASs',
#             "globally_unique_sample_id","sample_code_modified_PFAS","lab_sample_code1_PFAS",
#             "sample_code_original_PFAS","gender_PFASs"))
# 
# PFASs <- rbind.fill(PFASs_g15,PFASs_l15,PFASs_l5)
# 
# # create age_group based on mean age
# PFASs$mean_age <- as.numeric(PFASs$mean_age)
# PFASs <- PFASs %>% subset(!is.na(mean_age))
# PFASs <- age_group(PFASs)
# 
# # check
# count(PFASs$age_group)

######################### create SWV, SWD, SWN #############################
SWVcols_PFASs <- select_SWV(PFASs_merged)
colnames(SWVcols_PFASs)

SWDcols_PFASs <- select_SWD(PFASs_merged) 
colnames(SWDcols_PFASs)

# MAKE THEM LONG 
SWVcols_long_PFASs <- SWVcols_PFASs %>%
  gather(.,chem, value,  `SWVEt-PFOSA-AcOH`:`SWVTotalPFOS`)

SWDcols_long_PFASs <- SWDcols_PFASs %>%
  gather(.,chem, value,  `SWDEt-PFOSA-AcOH`:`SWDTotalPFOS`)


# Convert age group to factor 
list <- list(SWVcols_long_PFASs,SWDcols_long_PFASs)
list <- lapply(list, 
               function(x){x$age_group <- factor(x$age_group,levels = c("0-5","5-15","15-30","30-45","45-60",">60"));
               return(x)})

SWVcols_long_PFASs <- list[[1]]
SWDcols_long_PFASs <- list[[2]]


# Create chem name column, e.g. 'PBDE1','PBDE10'
list <- lapply(list, 
               function(x){x %>% mutate(chem_name = substr(as.character(chem), 4, nchar(chem)))})
SWVcols_long_PFASs <- unique(list[[1]])
SWDcols_long_PFASs <- unique(list[[2]])


# Merge SWV, SWD
PFASs_all <- merge(SWVcols_long_PFASs, SWDcols_long_PFASs, 
                  by = c("sample_id","sample_year","mean_age","chem_name","sex","age_group")) 

colnames(PFASs_all)
colnames(PFASs_all)[7:10] <- c("chem_SWV","SWV","chem_SWD","SWD")
PFASs_all <- unique(PFASs_all)


# Create column 'detected'
# If SWV >= SWD, it is 'detected', otherwise, it is "Non-detected"
PFASs_all <- PFASs_all %>% 
  mutate(detected = case_when(
    SWV == 0 & SWD == 0 ~ 'delete',
    is.na(SWV) & is.na(SWD) ~ 'delete',
    SWV >= SWD & SWV != 0 ~ 'detected',
    SWV > 0 & is.na(SWD) ~ 'detected',
    SWV < SWD ~ 'non-detected',
    is.na(SWV) ~ 'non-detected',
    SWV == 0 ~ 'non-detected'
  ))

PFASs_all <- PFASs_all %>% subset(detected != 'delete')


# We give each chem an order
PFASs_order <- read_excel("./Order/PFASs_order.xlsx")
PFASs_all <- merge(PFASs_all, PFASs_order, by.x  = "chem_name")
PFASs_sorted <- PFASs_all[order(PFASs_all$order, decreasing = FALSE),]
PFASs_sorted <- unique(PFASs_sorted)

# Create SLC,value,log_value,value_detected, log_value_detected
PFASs_sorted <- SWC(PFASs_sorted)
PFASs_sorted <- log_value(PFASs_sorted)

########################################## Detection Rate ##########################################
detect_rate(PFASs_sorted)
detection_rate <- read_excel("Rate of Detection.xlsx",sheet = "PFASs") %>% select(-1)
PFASs_sorted <- merge(PFASs_sorted, detection_rate, by = c("chem_name"), all.x = T) %>%
  select(-c("detected.y","detected_num","total_num"))
colnames(PFASs_sorted)
colnames(PFASs_sorted)[11] <- 'detected'

############################## Regression ###################################################
PFASs_sorted <- PFASs_sorted %>% mutate(sex_rel = ifelse(sex == "F",1,0))
PFASs_sorted$mean_age_rel <- PFASs_sorted$mean_age - 28
PFASs_sorted$mean_age_2 <- (PFASs_sorted$mean_age_rel)^2
PFASs_sorted$sample_year_rel <- PFASs_sorted$sample_year - 2002
PFASs_sorted$sample_year_2 <- (PFASs_sorted$sample_year_rel)*(PFASs_sorted$sample_year_rel)

PFASs_sorted_reg <- PFASs_sorted %>% subset(detection_rate >= 0.5)
PFASs_sorted_reg_list <- PFASs_sorted_reg %>% split(f=.$chem_name)

PFASs_reg_coef <- reg_estimate(PFASs_sorted_reg_list)
PFASs_reg_pr <- reg_Pr(PFASs_sorted_reg_list)

colnames(PFASs_reg_pr) <- paste(colnames(PFASs_reg_pr), "_Pr")
colnames(PFASs_reg_pr)[7] <- "chem"
PFASs_reg_pr$chem_cat <- 'PFASs'
PFASs_reg <- merge(PFASs_reg_coef, PFASs_reg_pr,
                  by = "chem")

write.xlsx(PFASs_sorted_reg
           , file = "data_reg.xlsx"
           , sheetName = "PFASs"
           , append = TRUE)


write.xlsx(PFASs_reg
           , file = "reg_sum.xlsx"
           , sheetName = "PFASs"
           , append = TRUE)

########################################### Plot ######################################                        
PFASs_sorted_list <- PFASs_sorted %>% split(f=.$order)

PFASs_sorted_1 <- bind_rows(PFASs_sorted_list[1:5]) 
PFASs_sorted_2 <- bind_rows(PFASs_sorted_list[6:10])
PFASs_sorted_3 <- bind_rows(PFASs_sorted_list[11:15])
PFASs_sorted_4 <- bind_rows(PFASs_sorted_list[16:20])
PFASs_sorted_5 <- bind_rows(PFASs_sorted_list[21:25])
PFASs_sorted_6 <- bind_rows(PFASs_sorted_list[26:30])

PFASs_sorted_list <- list(PFASs_sorted_1,PFASs_sorted_2,
                          PFASs_sorted_3,PFASs_sorted_4,
                          PFASs_sorted_5,PFASs_sorted_6)

########################################## Year trend ##########################################
pdf("../Plots/PFASs/PFASs_year.pdf")
print(plot_year_log_regD_W(PFASs_sorted_list))
dev.off() 

########################################## Age trend ##########################################
# Print plots to a pdf file
pdf("../Plots/PFASs/PFASs_age.pdf")
print(plot_age_log_regD_W(PFASs_sorted_list))
dev.off() 

########################################## Subset ##########################################
# Only plot PFOA, PFNA, PFHxS, PFOS
# Remove two non-detected values for PFHxS since the axis scale is hard to adjust
PFASs_subset <- bind_rows(PFASs_sorted) %>%
  subset(chem_name %in% c("PFOA","PFNA","PFHxS","PFOS")) %>%
  subset(detected == "detected")

pdf("../Plots/PFASs/PFASs_subset_year.pdf")
print(plot_year_log_regD_W(list(PFASs_subset)))
dev.off()                       

# Print plots to a pdf file
pdf("../Plots/PFASs/PFASs_subset_age.pdf")
print(plot_age_log_regD_W(list(PFASs_subset)))
dev.off()   

# ####################################### Plot ###################################################3
# # Create List
# PFASs_reg <- PFASs_reg %>% split(f=.$order)
# 
# PFASs_reg_1 <- bind_rows(PFASs_reg[1:4])
# PFASs_reg_2 <- bind_rows(PFASs_reg[5:8])
# PFASs_reg_3 <- bind_rows(PFASs_reg[9:12])
# PFASs_reg_4 <- bind_rows(PFASs_reg[13:16])
# PFASs_reg_5 <- bind_rows(PFASs_reg[17:20])
# PFASs_reg_6 <- bind_rows(PFASs_reg[21:24])
# PFASs_reg_7 <- bind_rows(PFASs_reg[25:28])
# PFASs_reg_8 <- bind_rows(PFASs_reg[29:32])
# 
# PFASs_reg_lst <- list(PFASs_reg_1, PFASs_reg_2, PFASs_reg_3)
# 
# # Only plot PFOA, PFNA, PFHxS, PFOS
# PFASs_subset <- bind_rows(PFASs_reg_lst) %>%
#   subset(chem_name %in% c("PFOA","PFNA","PFHxS","PFOS")) %>%
#   subset(detected == "Detected")
# 
# pdf("D:\\Research\\Australian Project\\Plots\\PFASs_year_subset_0225.pdf")
# print(plot_year_log_regD_W(list(PFASs_subset)))
# dev.off()                       
#                       
# # Print plots to a pdf file
# pdf("D:\\Research\\Australian Project\\Plots\\PFASs_age_subset_0225.pdf")
# print(plot_age_log_regD_W(list(PFASs_subset)))
# dev.off() 
# 
# # Regression coefficient
# PFASs_estimate <-  reg_estimate(PFASs_reg)
# PFASs_pr <- reg_Pr(PFASs_reg)
# 
# colnames(PFASs_pr) <- paste(colnames(PFASs_pr), "_Pr")
# colnames(PFASs_pr)[6] <- "chem"
# 
# PFASs_regression <- merge(PFASs_estimate,PFASs_pr,
#                           by = "chem")
# 
# PFASs_regression <- merge(PFASs_regression, PFASs_order, by.x = "chem", by.y = "order", all.x = F, all.y = F)
# 
# # Gender seperated
# # Female 
# PFASs_Female <- PFASs_reg_table %>% subset(gender=='F')
# PFASs_Reg_F <- PFASs_Female %>% split(f=.$order)
# PFASs_Reg_F_1 <- bind_rows(PFASs_Reg_F[1:4])
# PFASs_Reg_F_2 <- bind_rows(PFASs_Reg_F[5:8])
# PFASs_Reg_F_3 <- bind_rows(PFASs_Reg_F[9:12])
# PFASs_Reg_F_lst <- list(PFASs_Reg_F_1,PFASs_Reg_F_2,PFASs_Reg_F_3)
# 
# PFASs_Estimate_F <-  reg_estimate(PFASs_Reg_F)
# PFASs_Pr_F <- reg_Pr(PFASs_Reg_F)
# colnames(PFASs_Pr_F) <- paste(colnames(PFASs_Estimate_F), "_Pr")
# colnames(PFASs_Pr_F)[6] <- "chem"
# 
# PFASs_regression_F <- merge(PFASs_Estimate_F, PFASs_Pr_F,
#                           by = "chem")
# 
# PFASs_regression_F <- merge(PFASs_regression_F, PFASs_order, 
#                             by.x = "chem", by.y = "order", all.x = F, all.y = F)
# 
# PFASs_regression_F$gender <- 'F'
# 
# # Male 
# PFASs_Male <- PFASs_reg_table %>% subset(gender=='M')
# PFASs_Reg_M <- PFASs_Male %>% split(f=.$order)
# PFASs_Reg_M_1 <- bind_rows(PFASs_Reg_M[1:4])
# PFASs_Reg_M_2 <- bind_rows(PFASs_Reg_M[5:8])
# PFASs_Reg_M_3 <- bind_rows(PFASs_Reg_M[9:12])
# PFASs_Reg_M_lst <- list(PFASs_Reg_M_1,PFASs_Reg_M_2,PFASs_Reg_M_3)
# 
# PFASs_Estimate_M <-  reg_estimate(PFASs_Reg_M)
# PFASs_Pr_M <- reg_Pr(PFASs_Reg_M)
# colnames(PFASs_Pr_M) <- paste(colnames(PFASs_Estimate_M), "_Pr")
# colnames(PFASs_Pr_M)[6] <- "chem"
# 
# PFASs_regression_M <- merge(PFASs_Estimate_M, PFASs_Pr_M,
#                             by = "chem")
# 
# PFASs_regression_M <- merge(PFASs_regression_M, PFASs_order, 
#                             by.x = "chem", by.y = "order", all.x = F, all.y = F)
# PFASs_regression_M$gender <- 'M'
# 
# PFASs_regression <- rbind(PFASs_regression_F, PFASs_regression_M)
# 
# p <- ggplot(data = PFASs_regression, aes(x = mean_age_rel, y = mean_age_2, shape = chem_name, color = gender)) +
#   geom_abline(intercept = 0, slope = 1/26.9, colour = 'red') +
#   geom_hline(yintercept=0, linetype="dashed", color = "red") +
#   geom_vline(xintercept=0, linetype="dashed", color = "red") +
#   # scale_x_continuous(limit = c(-1.5,1.2)) +
#   # scale_y_continuous(limit = c(-0.03,0.12)) +
#   geom_point(size = 3, show.legend = TRUE) +
#   scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
#   
# 
# ggplotly(p)
