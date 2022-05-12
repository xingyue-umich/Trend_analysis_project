# Plot for DOX
library("ggplot2")
library("tidyr")
library("ggpmisc")
source("../functions.R")

# Import file
DOX_merged <- read_excel("../../Data/Chem_merged/DOX_merged.xlsx", sheet = "DOX_merged") %>% select(-1)

######################### first check #############################
# check colnames and types
colnames(DOX_merged)
str(DOX_merged)

# Drop '_file' column
DOX_merged <- DOX_merged %>% select(-c("_file"))

# Drop rows where sample year = 0, those are cord blood or bovine or sample id starts with 999
DOX_merged <- DOX_merged %>% subset(!(sample_year == 0))

# Convert mean age, sample_year column to numeric
DOX_merged$mean_age <- as.numeric(DOX_merged$mean_age)
DOX_merged$sample_year <- as.numeric(DOX_merged$sample_year)
# Delete mean age is NA (bovine or cord blood)
DOX_merged <- DOX_merged %>% subset(!is.na(mean_age))

# create age_group according to the mean_age(fun)
DOX_merged <- age_group(DOX_merged)


######################### create SLV, SLD, SWN #############################
SLVcols_DOX <- select_SLV(DOX_merged)
colnames(SLVcols_DOX)

SLDcols_DOX <- select_SLD(DOX_merged) 
colnames(SLDcols_DOX)

# MAKE THEM LONG 
SLVcols_long_DOX <- SLVcols_DOX %>%
  gather(.,chem, value,  SLV1234678D:SLV2378D)

SLDcols_long_DOX <- SLDcols_DOX %>%
  gather(.,chem, value,  SLD1234678D:SLD2378D)


# Convert age group to factor 
list <- list(SLVcols_long_DOX,SLDcols_long_DOX)
list <- lapply(list, 
               function(x){x$age_group <- factor(x$age_group,levels = c("0-5","5-15","15-30","30-45","45-60",">60"));
               return(x)})
SLVcols_long_DOX <- list[[1]]
SLDcols_long_DOX <- list[[2]]


# Create chem name column, e.g. 'PBDE1','PBDE10'
list <- lapply(list, 
               function(x){x %>% mutate(chem_name = substr(as.character(chem), 4, nchar(chem)))})
SLVcols_long_DOX <- unique(list[[1]])
SLDcols_long_DOX <- unique(list[[2]])


# Merge SLV, SLD
DOX_all <- merge(SLVcols_long_DOX, SLDcols_long_DOX, 
                  by = c("sample_id","sample_year","mean_age","chem_name","sex","age_group")) 

colnames(DOX_all)
colnames(DOX_all)[7:10] <- c("chem_SLV","SLV","chem_SLD","SLD")
DOX_all <- unique(DOX_all)


# Create column 'detected'
# If SLV >= SLD, it is 'detected', otherwise, it is "Non-detected"
DOX_all <- DOX_all %>% 
  mutate(detected = case_when(
    SLV == 0 & SLD == 0 ~ 'delete',
    SLV >= SLD & SLV != 0 ~ 'detected',
    SLV > 0 & is.na(SLD) ~ 'detected',
    SLV < SLD ~ 'non-detected',
    is.na(SLV) ~ 'non-detected',
    SLV == 0 ~ 'non-detected'
  ))

DOX_all <- DOX_all %>% subset(detected != 'delete')

# We give each chem an order
DOX_order <- read_excel("./Order/DOX_order.xlsx")
DOX_all <- merge(DOX_all, DOX_order, by = "chem_name")
DOX_sorted <- DOX_all[order(DOX_all$order, decreasing = FALSE),]
DOX_sorted <- unique(DOX_sorted)

# Create SLC,value,log_value,value_detected, log_value_detected
DOX_sorted <- SLC(DOX_sorted)
DOX_sorted <- log_value(DOX_sorted)

########################################## Detection Rate ##########################################
detect_rate(DOX_sorted)
detection_rate <- read_excel("Rate of Detection.xlsx",sheet = "DOX") %>% select(-1)
DOX_sorted <- merge(DOX_sorted, detection_rate, by = c("chem_name"), all.x = T) %>%
  select(-c("detected.y","detected_num","total_num"))
colnames(DOX_sorted)
colnames(DOX_sorted)[11] <- 'detected'

############################## Regression ###################################################
DOX_sorted <- DOX_sorted %>% mutate(sex_rel = ifelse(sex == "F",1,0))
DOX_sorted$mean_age_rel <- DOX_sorted$mean_age - 28
DOX_sorted$mean_age_2 <- (DOX_sorted$mean_age_rel)^2
DOX_sorted$sample_year_rel <- DOX_sorted$sample_year - 2002
DOX_sorted$sample_year_2 <- (DOX_sorted$sample_year_rel)*(DOX_sorted$sample_year_rel)

DOX_sorted_reg <- DOX_sorted %>% subset(detection_rate >= 0.5)
DOX_sorted_reg_list <- DOX_sorted_reg %>% split(f=.$chem_name)


DOX_reg_coef <- reg_estimate(DOX_sorted_reg_list)
DOX_reg_pr <- reg_Pr(DOX_sorted_reg_list)

colnames(DOX_reg_pr) <- paste(colnames(DOX_reg_pr), "_Pr")
colnames(DOX_reg_pr)[7] <- "chem"
DOX_reg_pr$chem_cat <- 'DOX'
DOX_reg <- merge(DOX_reg_coef, DOX_reg_pr,
                  by = "chem")

write.xlsx(DOX_sorted_reg
           , file = "data_reg.xlsx"
           , sheetName = "DOX"
           , append = TRUE)


write.xlsx(DOX_reg
           , file = "reg_sum.xlsx"
           , sheetName = "DOX"
           , append = TRUE)

########################################### Plot ######################################                        
DOX_sorted_list <- DOX_sorted %>% split(f=.$order)

DOX_sorted_1 <- bind_rows(DOX_sorted_list[1:4]) 
DOX_sorted_2 <- bind_rows(DOX_sorted_list[5:8])
DOX_sorted_3 <- bind_rows(DOX_sorted_list[9:12])
DOX_sorted_4 <- bind_rows(DOX_sorted_list[13:16])
DOX_sorted_5 <- bind_rows(DOX_sorted_list[17:20])
DOX_sorted_6 <- bind_rows(DOX_sorted_list[21:24])
DOX_sorted_7 <- bind_rows(DOX_sorted_list[25:29])


DOX_sorted_list <- list(DOX_sorted_1,DOX_sorted_2, DOX_sorted_3,DOX_sorted_4,
                         DOX_sorted_5,DOX_sorted_6,DOX_sorted_7)

########################################## Year trend ##########################################
pdf("../Plots/DOX/DOX_year.pdf")
print(plot_year_log_regD(DOX_sorted_list))
dev.off() 

########################################## Age trend ##########################################
# Print plots to a pdf file
pdf("../Plots/DOX/DOX_age.pdf")
print(plot_age_log_regD(DOX_sorted_list))
dev.off() 

