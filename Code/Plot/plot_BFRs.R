# Plot for BFRs
library("ggplot2")
library("tidyr")
library("ggpmisc")
source("../code/functions.R")

# Import file
BFRs_merged <- read_excel("./BFRs/BFRs_merged.xlsx", sheet = "BFRs_merged") %>% select(-1)

######################### first check #############################
# check colnames and types
colnames(BFRs_merged)
str(BFRs_merged)

# Drop '_file' column
BFRs_merged <- BFRs_merged %>% select(-c("_file"))

# Drop rows where sample year = 0, those are cord blood or bovine or sample id starts with 999
BFRs_merged <- BFRs_merged %>% subset(!(sample_year == 0))

# Convert mean age, sample_year column to numeric
BFRs_merged$mean_age <- as.numeric(BFRs_merged$mean_age)
BFRs_merged$sample_year <- as.numeric(BFRs_merged$sample_year)
# Delete mean age is NA (bovine or cord blood)
BFRs_merged <- BFRs_merged %>% subset(!is.na(mean_age))

# create age_group according to the mean_age(fun)
BFRs_merged <- age_group(BFRs_merged)

# Delete  "SWNPBDE17PBDE66"  "SWNPBDE17PBDE100", they do not have any lod or value info
BFRs_merged <- BFRs_merged %>% select(-c("SWNPBDE17PBDE66","SWNPBDE17PBDE100"))

# Delete SWN=3 [Non reported]
# BFRs_merged_NP <- BFRs_merged %>% dplyr::select(grep("SWN",names(BFRs_merged)))

######################### create SLV, SLD, SWN #############################
SLVcols_BFRs <- select_SLV(BFRs_merged)
colnames(SLVcols_BFRs)

SLDcols_BFRs <- select_SLD(BFRs_merged) 
colnames(SLDcols_BFRs)

# SWNcols_BFRs <- select_SWN(BFRs_merged)
# colnames(SWNcols_BFRs)

# MAKE THEM LONG 
SLVcols_long_BFRs <- SLVcols_BFRs %>%
  gather(.,chem, value,  SLVPBDE1:SLVBB153)

SLDcols_long_BFRs <- SLDcols_BFRs %>%
  gather(.,chem, value,  SLDPBDE1:SLDBB153)

# SWNcols_long_BFRs <- SWNcols_BFRs %>%
#   gather(.,chem, value,  SWNPBDE1:SWNBB153)

# Convert age group to factor 
list <- list(SLVcols_long_BFRs,SLDcols_long_BFRs)
list <- lapply(list, 
               function(x){x$age_group <- factor(x$age_group,levels = c("0-5","5-15","15-30","30-45","45-60",">60"));
               return(x)})
SLVcols_long_BFRs <- list[[1]]
SLDcols_long_BFRs <- list[[2]]


# Create chem name column, e.g. 'PBDE1','PBDE10'
list <- lapply(list, 
               function(x){x %>% mutate(chem_name = substr(as.character(chem), 4, nchar(chem)))})
SLVcols_long_BFRs <- unique(list[[1]])
SLDcols_long_BFRs <- unique(list[[2]])


# Merge SLV, SLD
BFRs_all <- merge(SLVcols_long_BFRs, SLDcols_long_BFRs, 
                 by = c("sample_id","sample_year","mean_age","chem_name","sex","age_group")) 

colnames(BFRs_all)
colnames(BFRs_all)[7:10] <- c("chem_SLV","SLV","chem_SLD","SLD")
BFRs_all <- unique(BFRs_all)


# Create column 'detected'
# If SLV >= SLD, it is 'detected', otherwise, it is "Non-detected"
BFRs_all <- BFRs_all %>% 
  mutate(detected = case_when(
    SLV == 0 & SLD == 0 ~ 'delete',
    SLV >= SLD & SLV != 0 ~ 'detected',
    SLV > 0 & is.na(SLD) ~ 'detected',
    SLV < SLD ~ 'non-detected',
    is.na(SLV) ~ 'non-detected',
    SLV == 0 ~ 'non-detected'
  ))

# BFRs_all <- merge(BFRs_all, SWNcols_long_BFRs,
#                   by = c("sample_id","sample_year","mean_age","chem_name","sex","age_group"))
# colnames(BFRs_all)
# colnames(BFRs_all)[11:12] <- c("chem_SWN","SWN")
# colnames(BFRs_all)


# We give each chem an order
BFRs_order <- read_excel("./Order/BFRs_order.xlsx")
BFRs_all <- merge(BFRs_all, BFRs_order, by = "chem_name")
BFRs_sorted <- BFRs_all[order(BFRs_all$order, decreasing = FALSE),]
BFRs_sorted <- unique(BFRs_sorted)


# Create SLC,value,log_value,value_detected, log_value_detected
BFRs_sorted <- SLC(BFRs_sorted)
BFRs_sorted <- log_value(BFRs_sorted)

########################################## Detection Rate ##########################################
# detect_rate(BFRs_sorted)
detection_rate <- read_excel("Rate of Detection.xlsx",sheet = "BFRs") %>% select(-1)
BFRs_sorted <- merge(BFRs_sorted, detection_rate, by = c("chem_name"), all.x = T) %>%
  select(-c("detected.y","detected_num","total_num"))
colnames(BFRs_sorted)
colnames(BFRs_sorted)[11] <- 'detected'

############################## Regression ###################################################
BFRs_sorted <- BFRs_sorted %>% mutate(sex_rel = ifelse(sex == "F",1,0))
BFRs_sorted$mean_age_rel <- BFRs_sorted$mean_age - 28
BFRs_sorted$mean_age_2 <- (BFRs_sorted$mean_age_rel)^2
BFRs_sorted$sample_year_rel <- BFRs_sorted$sample_year - 2002
BFRs_sorted$sample_year_2 <- (BFRs_sorted$sample_year_rel)*(BFRs_sorted$sample_year_rel)

BFRs_sorted_reg <- BFRs_sorted %>% subset(detection_rate >= 0.5)
BFRs_sorted_reg_list <- BFRs_sorted_reg %>% split(f=.$chem_name)

# write.xlsx(BFRs_sorted_reg
#            , file = "data_reg.xlsx"
#            , sheetName = "BFRs"
#            , append = TRUE)


BFRs_reg_coef <- reg_estimate(BFRs_sorted_reg_list)
BFRs_reg_pr <- reg_Pr(BFRs_sorted_reg_list)

colnames(BFRs_reg_pr) <- paste(colnames(BFRs_reg_pr), "_Pr")
colnames(BFRs_reg_pr)[7] <- "chem"
BFRs_reg_pr$chem_cat <- 'BFRs'
BFRs_reg <- merge(BFRs_reg_coef, BFRs_reg_pr,
                  by = "chem")

# write.xlsx(BFRs_reg
#            , file = "reg_sum.xlsx"
#            , sheetName = "BFRs"
#            , append = TRUE)

# subset children
# BFRs_sorted_reg_g5 <- BFRs_sorted_reg %>% subset(mean_age>5)
# BFRs_sorted_reg_list_g5 <- BFRs_sorted_reg_g5 %>% split(f=.$chem_name)
# estimate_BFRs_g5_df <- reg_estimate(BFRs_sorted_reg_list_g5)
# Pr_BFRs_g5_df <- reg_Pr(BFRs_sorted_reg_list_g5)
# 
# colnames(Pr_BFRs_g5_df) <- paste(colnames(Pr_BFRs_g5_df), "_Pr")
# colnames(Pr_BFRs_g5_df)[7] <- "chem"
# estimate_BFRs_g5_df$chem_cat <- 'BFRs_g5'
# Pr_BFRs_g5_df$chem_cat <- 'BFRs_g5'
# reg_BFRs_g5 <- merge(estimate_BFRs_g5_df, Pr_BFRs_g5_df,
#                     by = c("chem","chem_cat"))
# write.xlsx(reg_BFRs_g5
#            , file = "reg_sum_0422.xlsx"
#            , sheetName = "BFRs"
#            , append = TRUE)
# 
# BFRs_sorted_reg_l5 <- BFRs_sorted_reg %>% subset(mean_age<5)
# BFRs_sorted_reg_list_l5 <- BFRs_sorted_reg_l5 %>% split(f=.$chem_name)
# estimate_BFRs_l5_df <- reg_estimate(BFRs_sorted_reg_list_l5)
# Pr_BFRs_l5_df <- reg_Pr(BFRs_sorted_reg_list_l5)
# 
# colnames(Pr_BFRs_l5_df) <- paste(colnames(Pr_BFRs_l5_df), "_Pr")
# colnames(Pr_BFRs_l5_df)[7] <- "chem"
# estimate_BFRs_l5_df$chem_cat <- 'BFRs_l5'
# Pr_BFRs_l5_df$chem_cat <- 'BFRs_l5'
# reg_BFRs_l5 <- merge(estimate_BFRs_l5_df, Pr_BFRs_l5_df,
#                      by = c("chem","chem_cat"))
# 
# write.xlsx(reg_BFRs_l5
#            , file = "reg_sum_l5_0422.xlsx"
#            , sheetName = "BFRs"
#            , append = TRUE)
########################################### Plot ######################################                        
BFRs_sorted_list <- BFRs_sorted %>% split(f=.$order)

BFRs_sorted_1 <- bind_rows(BFRs_sorted_list[1:4]) 
BFRs_sorted_2 <- bind_rows(BFRs_sorted_list[5:8])
BFRs_sorted_3 <- bind_rows(BFRs_sorted_list[9:12])
BFRs_sorted_4 <- bind_rows(BFRs_sorted_list[13:16])
BFRs_sorted_5 <- bind_rows(BFRs_sorted_list[17:20])
BFRs_sorted_6 <- bind_rows(BFRs_sorted_list[21:24])
BFRs_sorted_7 <- bind_rows(BFRs_sorted_list[25:28])
BFRs_sorted_8 <- bind_rows(BFRs_sorted_list[29:32])
BFRs_sorted_9 <- bind_rows(BFRs_sorted_list[33:36])
BFRs_sorted_10 <- bind_rows(BFRs_sorted_list[37:40])
BFRs_sorted_11 <- bind_rows(BFRs_sorted_list[41:44])
BFRs_sorted_12 <- bind_rows(BFRs_sorted_list[45:48])
BFRs_sorted_13 <- bind_rows(BFRs_sorted_list[49:52])


BFRs_sorted_list <- list(BFRs_sorted_1,BFRs_sorted_2, BFRs_sorted_3,BFRs_sorted_4,
                      BFRs_sorted_5,BFRs_sorted_6,BFRs_sorted_7,BFRs_sorted_8,BFRs_sorted_9,
                      BFRs_sorted_10,BFRs_sorted_11,BFRs_sorted_12,BFRs_sorted_13)

# Subset
BFRs_subset <- BFRs_sorted %>%
  subset(chem_name %in% c("PBDE47","PBDE99","PBDE100","PBDE153")) 

BFRs_subset_list <- BFRs_subset %>% split(f=.$order)
  
BFRs_subset_1 <- bind_rows(BFRs_subset_list[1:4]) 

BFRs_subset_list <- list(BFRs_subset_1)
########################################## Year trend ##########################################
pdf("../Plots/BFRs/BFRs_year.pdf")
print(plot_year_log_regD(BFRs_sorted_list))
dev.off() 

pdf("../Plots/BFRs/BFRs_year_subset.pdf")
print(plot_year_log_regD(BFRs_subset_list))
dev.off() 


########################################## Age trend ##########################################
# Print plots to a pdf file
pdf("../Plots/BFRs/BFRs_age.pdf")
print(plot_age_log_regD(BFRs_sorted_list))
dev.off() 

pdf("../Plots/BFRs/BFRs_age_subset.pdf")
print(plot_age_log_regD(BFRs_subset_list))
dev.off() 
