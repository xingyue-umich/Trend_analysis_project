library("ggplot2")
library("tidyr")
library("ggpmisc")
source("../code/functions.R")


# Import file
PCB_merged <- read_excel("../../Data/Chem_merged/PCB_merged.xlsx", sheet = "PCB_merged") %>% select(-1)
colnames(PCB_merged)

# Create age group based on the mean age
PCB_merged$mean_age <- as.numeric(PCB_merged$mean_age)
PCB_merged <- age_group(PCB_merged)

# Delete age group is NA
PCB_merged <- PCB_merged %>% subset(!is.na(age_group)) %>% select(-c("_file"))

######################### create SLV, SLD, SWN #############################
SLVcols_PCB <- select_SLV(PCB_merged)
colnames(SLVcols_PCB)

SLDcols_PCB <- select_SLD(PCB_merged) 
colnames(SLDcols_PCB)

SWNcols_PCB <- select_SWN(PCB_merged)
colnames(SWNcols_PCB)

######################### Make them Long #############################
SLVcols_long_PCB <- SLVcols_PCB %>%
  gather(.,chem, value,  'SLVPCB101':'SLVPCB123')

SLDcols_long_PCB <- SLDcols_PCB %>%
  gather(.,chem, value,  'SLDPCB101':'SLDPCB123')

SWNcols_long_PCB <- SWNcols_PCB %>%
  gather(.,chem, value,  'SWNPCB18':'SWNPCB123')

# Convert "sample year" as an integer
SLVcols_long_PCB <- transform(SLVcols_long_PCB, sample_year = as.integer(sample_year))
SLDcols_long_PCB <- transform(SLDcols_long_PCB, sample_year = as.integer(sample_year))
SWNcols_long_PCB <- transform(SWNcols_long_PCB, sample_year = as.integer(sample_year))

# Convert age group to factor
list <- list(SLVcols_long_PCB,SLDcols_long_PCB,SWNcols_long_PCB)
list <- lapply(list, 
               function(x){x$age_group <- factor(x$age_group,levels = c("0-5","5-15","15-30","30-45","45-60",">60"));return(x)})
SLVcols_long_PCB <- list[[1]]
SLDcols_long_PCB <- list[[2]]
SWNcols_long_PCB <- list[[3]]

# Create chem name column
list <- list(SLVcols_long_PCB,SLDcols_long_PCB,SWNcols_long_PCB)
list <- lapply(list, 
               function(x){x %>% mutate(chem_name = substr(as.character(chem), 4, nchar(chem)))})
SLVcols_long_PCB <- list[[1]]
SLDcols_long_PCB <- list[[2]]
SWNcols_long_PCB <- list[[3]]

######################### Merge SLV,SLD then SWN #############################
PCB_all <- merge(SLVcols_long_PCB,SLDcols_long_PCB, 
                  by = c("sample_id","sample_year","mean_age","chem_name","sex","age_group"))

colnames(PCB_all)
colnames(PCB_all)[7:10] <- c("chem_SLV","SLV","chem_SLD","SLD")
PCB_all <- unique(PCB_all)

PCB_all <- merge(PCB_all, SWNcols_long_PCB,
                  by = c("sample_id","sample_year","mean_age","chem_name","sex","age_group"))
colnames(PCB_all)
colnames(PCB_all)[11:12] <- c("chem_SWN","SWN")
colnames(PCB_all)

PCB_all <- unique(PCB_all)

# We give each chem an order
PCB_order <- read_excel("./Order/Order_PCB.xlsx")
PCB_all <- merge(PCB_all, PCB_order, by = "chem_name")
PCB_sorted <- PCB_all[order(PCB_all$order, decreasing = FALSE),]
PCB_sorted <- unique(PCB_sorted)

# Create a column 'detected'
# If SWN = O OR 4, it is 'detected', otherwise, it is "Non-detected"
PCB_sorted <- PCB_sorted %>% mutate(.,detected = as.factor(ifelse(SWN==0|SWN==4, "Detected","Non-detected")))

# Create SLC,value,log_value,value_detected, log_value_detected
PCB_sorted <- SLC(PCB_sorted)
PCB_sorted <- log_value(PCB_sorted)

########################################## Detection Rate ##########################################
# detect_rate(PCB_sorted)
detection_rate <- read_excel("Rate of detection.xlsx", sheet = "PCB") %>% select(-1)
PCB_sorted <- merge(PCB_sorted, detection_rate, by = c("chem_name")) %>%
  select(-c("detected.y","detected_num","total_num"))
colnames(PCB_sorted)
colnames(PCB_sorted)[14] <- 'detected'

############################## Regression ###################################################
PCB_sorted <- PCB_sorted %>% mutate(sex_rel = ifelse(sex == "F",1,0))
PCB_sorted$mean_age_rel <- PCB_sorted$mean_age - 28
PCB_sorted$mean_age_2 <- (PCB_sorted$mean_age_rel)^2
PCB_sorted$sample_year_rel <- PCB_sorted$sample_year - 2002
PCB_sorted$sample_year_2 <- (PCB_sorted$sample_year_rel)*(PCB_sorted$sample_year_rel)

# PCB_sorted_detected <- PCB_sorted %>% subset(SWN == 0)
PCB_sorted_reg <- PCB_sorted %>% subset(detection_rate >= 0.5)
PCB_sorted_reg_list <- PCB_sorted_reg %>% split(f=.$chem_name)

write.xlsx(PCB_sorted_reg
           , file = "data_reg.xlsx"
           , sheetName = "PCB"
           , append = TRUE)

PCB_reg_coef <- reg_estimate(PCB_sorted_reg_list)
PCB_reg_pr <- reg_Pr(PCB_sorted_reg_list)

colnames(PCB_reg_pr) <- paste(colnames(PCB_reg_pr), "_Pr")
colnames(PCB_reg_pr)[7] <- "chem"
PCB_reg_pr$chem_cat <- 'PCB'
PCB_reg <- merge(PCB_reg_coef, PCB_reg_pr,
                  by = "chem")

write.xlsx(PCB_reg
           , file = "reg_sum.xlsx"
           , sheetName = "PCB"
           , append = TRUE)

# subset children
# PCB_sorted_reg_g5 <- PCB_sorted_reg %>% subset(mean_age>5)
# PCB_sorted_reg_list_g5 <- PCB_sorted_reg_g5 %>% split(f=.$chem_name)
# estimate_PCB_g5_df <- reg_estimate(PCB_sorted_reg_list_g5)
# Pr_PCB_g5_df <- reg_Pr(PCB_sorted_reg_list_g5)
# 
# colnames(Pr_PCB_g5_df) <- paste(colnames(Pr_PCB_g5_df), "_Pr")
# colnames(Pr_PCB_g5_df)[7] <- "chem"
# estimate_PCB_g5_df$chem_cat <- 'PCB_g5'
# Pr_PCB_g5_df$chem_cat <- 'PCB_g5'
# reg_PCB_g5 <- merge(estimate_PCB_g5_df, Pr_PCB_g5_df,
#                     by = c("chem","chem_cat"))
# write.xlsx(reg_PCB_g5
#            , file = "reg_sum_0422.xlsx"
#            , sheetName = "PCB"
#            , append = TRUE)
# 
# PCB_sorted_reg_l5 <- PCB_sorted_reg %>% subset(mean_age<5)
# PCB_sorted_reg_list_l5 <- PCB_sorted_reg_l5 %>% split(f=.$chem_name)
# estimate_PCB_l5_df <- reg_estimate(PCB_sorted_reg_list_l5)
# Pr_PCB_l5_df <- reg_Pr(PCB_sorted_reg_list_l5)
# 
# colnames(Pr_PCB_l5_df) <- paste(colnames(Pr_PCB_l5_df), "_Pr")
# colnames(Pr_PCB_l5_df)[7] <- "chem"
# estimate_PCB_l5_df$chem_cat <- 'PCB_l5'
# Pr_PCB_l5_df$chem_cat <- 'PCB_l5'
# reg_PCB_l5 <- merge(estimate_PCB_l5_df, Pr_PCB_l5_df,
#                      by = c("chem","chem_cat"))
# 
# write.xlsx(reg_PCB_l5
#            , file = "reg_sum_l5_0422.xlsx"
#            , sheetName = "PCB"
#            , append = TRUE)

########################################### Plot ######################################                        
PCB_sorted_list <- PCB_sorted %>% split(f=.$order)

PCB_sorted_1 <- bind_rows(PCB_sorted_list[1:4]) 
PCB_sorted_2 <- bind_rows(PCB_sorted_list[5:8])
PCB_sorted_3 <- bind_rows(PCB_sorted_list[9:12])
PCB_sorted_4 <- bind_rows(PCB_sorted_list[13:16])
PCB_sorted_5 <- bind_rows(PCB_sorted_list[17:20])
PCB_sorted_6 <- bind_rows(PCB_sorted_list[21:24])
PCB_sorted_7 <- bind_rows(PCB_sorted_list[25:28])
PCB_sorted_8 <- bind_rows(PCB_sorted_list[29:32])
PCB_sorted_9 <- bind_rows(PCB_sorted_list[33:36])


PCB_sorted_list <- list(PCB_sorted_1,PCB_sorted_2, PCB_sorted_3,PCB_sorted_4,
                         PCB_sorted_5,PCB_sorted_6,PCB_sorted_7,PCB_sorted_8,PCB_sorted_9)

# Subset
PCB_subset <- PCB_sorted %>%
  subset(chem_name %in% c("PBDE47","PBDE99","PBDE100","PBDE153","PBDE183")) 

PCB_subset_list <- PCB_subset %>% split(f=.$order)

PCB_subset_1 <- bind_rows(PCB_subset_list[1:2]) 
PCB_subset_2 <- bind_rows(PCB_subset_list[3:5])

PCB_subset_list <- list(PCB_subset_1,PCB_subset_2)
########################################## Year trend ##########################################
pdf("../Plots/PCB_year.pdf")
print(plot_year_log_regD(PCB_sorted_list))
dev.off() 

########################################## Age trend ##########################################
pdf("../Plots/PCB_age.pdf")
print(plot_age_log_regD(PCB_sorted_list))
dev.off() 




