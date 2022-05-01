# Plot for OCPs
library("ggplot2")
library("tidyr")
library("ggpmisc")
source("../code/functions.R")

# Import file
OCPs_merged <- read_excel("./OCPs/OCPs_merged.xlsx", sheet = "OCPs_merged") %>% select(-1)

######################### first check #############################
# check colnames and types
colnames(OCPs_merged)
str(OCPs_merged)

# Drop '_file' column
OCPs_merged <- OCPs_merged %>% select(-c("_file","_OCPs","lab_sample_code2_OCPs","WEIGHT_SAMPLE_OCPs",
                                         "sample_code_original_OCPs","gender_OCPs","number_in_pool_OCPs"))

# Drop rows where sample year = 0, those are cord blood or bovine or sample id starts with 999
OCPs_merged <- OCPs_merged %>% subset(!(sample_year == 0))

# Convert mean age, sample_year column to numeric
OCPs_merged$mean_age <- as.numeric(OCPs_merged$mean_age)
OCPs_merged$sample_year <- as.numeric(OCPs_merged$sample_year)
# Delete mean age is NA (bovine or cord blood)
OCPs_merged <- OCPs_merged %>% subset(!is.na(mean_age))

# create age_group according to the mean_age(fun)
OCPs_merged <- age_group(OCPs_merged)


######################### create SLV, SLD, SWN #############################
SLVcols_OCPs <- select_SLV(OCPs_merged)
colnames(SLVcols_OCPs)

SLDcols_OCPs <- select_SLD(OCPs_merged) 
colnames(SLDcols_OCPs)

# MAKE THEM LONG 
SLVcols_long_OCPs <- SLVcols_OCPs %>%
  gather(.,chem, value,  SLVBHCCH:SLVGHCH)

SLDcols_long_OCPs <- SLDcols_OCPs %>%
  gather(.,chem, value,  SLDBHCCH:SLDGHCH)


# Convert age group to factor 
list <- list(SLVcols_long_OCPs,SLDcols_long_OCPs)
list <- lapply(list, 
               function(x){x$age_group <- factor(x$age_group,levels = c("0-5","5-15","15-30","30-45","45-60",">60"));
               return(x)})
SLVcols_long_OCPs <- list[[1]]
SLDcols_long_OCPs <- list[[2]]


# Create chem name column, e.g. 'PBDE1','PBDE10'
list <- lapply(list, 
               function(x){x %>% mutate(chem_name = substr(as.character(chem), 4, nchar(chem)))})
SLVcols_long_OCPs <- unique(list[[1]])
SLDcols_long_OCPs <- unique(list[[2]])


# Merge SLV, SLD
OCPs_all <- merge(SLVcols_long_OCPs, SLDcols_long_OCPs, 
                 by = c("sample_id","sample_year","mean_age","chem_name","sex","age_group")) 

colnames(OCPs_all)
colnames(OCPs_all)[7:10] <- c("chem_SLV","SLV","chem_SLD","SLD")
OCPs_all <- unique(OCPs_all)


# Create column 'detected'
# If SLV >= SLD, it is 'detected', otherwise, it is "Non-detected"
OCPs_all <- OCPs_all %>% 
  mutate(detected = case_when(
    SLV == 0 & SLD == 0 ~ 'delete',
    SLV >= SLD & SLV != 0 ~ 'detected',
    SLV > 0 & is.na(SLD) ~ 'detected',
    SLV < SLD ~ 'non-detected',
    is.na(SLV) ~ 'non-detected',
    SLV == 0 ~ 'non-detected'
  ))

OCPs_all <- OCPs_all %>% subset(detected != 'delete')


# We give each chem an order
OCPs_order <- read_excel("./Order/OCPs_order.xlsx")
OCPs_all <- merge(OCPs_all, OCPs_order, by.x  = "chem_name")
OCPs_sorted <- OCPs_all[order(OCPs_all$order, decreasing = FALSE),]
OCPs_sorted <- unique(OCPs_sorted)

# Create SLC,value,log_value,value_detected, log_value_detected
OCPs_sorted <- SLC(OCPs_sorted)
OCPs_sorted <- log_value(OCPs_sorted)

########################################## Detection Rate ##########################################
detect_rate(OCPs_sorted)
detection_rate <- read_excel("Rate of Detection.xlsx",sheet = "OCPs") %>% select(-1)
OCPs_sorted <- merge(OCPs_sorted, detection_rate, by = c("chem_name"), all.x = T) %>%
  select(-c("detected.y","detected_num","total_num"))
colnames(OCPs_sorted)
colnames(OCPs_sorted)[11] <- 'detected'

############################## Regression ###################################################
OCPs_sorted <- OCPs_sorted %>% mutate(sex_rel = ifelse(sex == "F",1,0))
OCPs_sorted$mean_age_rel <- OCPs_sorted$mean_age - 28
OCPs_sorted$mean_age_2 <- (OCPs_sorted$mean_age_rel)^2
OCPs_sorted$sample_year_rel <- OCPs_sorted$sample_year - 2002
OCPs_sorted$sample_year_2 <- (OCPs_sorted$sample_year_rel)*(OCPs_sorted$sample_year_rel)

OCPs_sorted_reg <- OCPs_sorted %>% subset(detection_rate >= 0.5)
OCPs_sorted_reg_list <- OCPs_sorted_reg %>% split(f=.$chem_name)


OCPs_reg_coef <- reg_estimate(OCPs_sorted_reg_list)
OCPs_reg_pr <- reg_Pr(OCPs_sorted_reg_list)

colnames(OCPs_reg_pr) <- paste(colnames(OCPs_reg_pr), "_Pr")
colnames(OCPs_reg_pr)[7] <- "chem"
OCPs_reg_pr$chem_cat <- 'OCPs'
OCPs_reg <- merge(OCPs_reg_coef, OCPs_reg_pr,
                 by = "chem")

write.xlsx(OCPs_sorted_reg
           , file = "data_reg.xlsx"
           , sheetName = "OCPs"
           , append = TRUE)


write.xlsx(OCPs_reg
           , file = "reg_sum.xlsx"
           , sheetName = "OCPs"
           , append = TRUE)

########################################### Plot ######################################                        
OCPs_sorted_list <- OCPs_sorted %>% split(f=.$order)

OCPs_sorted_1 <- bind_rows(OCPs_sorted_list[1:4]) 
OCPs_sorted_2 <- bind_rows(OCPs_sorted_list[5:8])
OCPs_sorted_3 <- bind_rows(OCPs_sorted_list[9:12])


OCPs_sorted_list <- list(OCPs_sorted_1,OCPs_sorted_2, OCPs_sorted_3)

########################################## Year trend ##########################################
pdf("../Plots/OCPs/OCPs_year.pdf")
print(plot_year_log_regD(OCPs_sorted_list))
dev.off() 

########################################## Age trend ##########################################
# Print plots to a pdf file
pdf("../Plots/OCPs/OCPs_age.pdf")
print(plot_age_log_regD(OCPs_sorted_list))
dev.off() 

