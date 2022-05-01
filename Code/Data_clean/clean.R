library(tidyverse)
library(tidyr)
library(dplyr)
library(openxlsx)
library(readxl)
library(rJava)
library(xlsx)
library(XLConnect)
library(rio)

setwd("~/Documents/Research/Data")

options(java.parameters = "-Xmx512M")
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))


# Load Functions
source("../Code/clean_A.R")
source("../Code/clean_B.R")
source("../Code/clean_C.R")
source("../Code/clean_D.R")

################ PCB CLEAN ###################
clean_B("PCBs_2006-2007")
clean_B("PCBs_2002&2008")
clean_A("PCBs_2010-2011")
# clean_C("PCBscheck_2010-2011")
clean_B("PCBs_2012-2013")
# clean_C("PCBscheck_2012-2013")

data_list_PCB <- import_list("QAEHS Updated Chemical Biomarker Datasets_back.xlsx", which = c(1,2,3,4),
                             setclass = "tbl",rbind = TRUE) %>% select(-1)
data_list_PCB <- select(data_list_PCB , -1) # Drop the first column i.e. id

write.xlsx(data_list_PCB
           , file = "PCB_merged_back.xlsx"
           , sheetName = "PCB_merged")

################ BFR CLEAN ###################
clean_A("BFRs_2002-2003")
clean_A("BFRs_2004-2005")
# clean_B("BFRs_2002_2008") # This is the BFRs data for year 2002 and 2008
clean_B("BFRs_2006-2007")
clean_B("BFRs_2010-2011")
clean_B("BFRs_2012-2013")

# Merge all sheets
data_list_BFRs <- import_list("QAEHS Updated Chemical Biomarker Datasets_back.xlsx", which = c(5,6,7,8,9),
                              setclass = "tbl",rbind = TRUE) %>% select(-1)

write.xlsx(data_list_BFRs
           , file = "BFRs_merged.xlsx"
           , sheetName = "BFRs_merged")

################ DOX CLEAN ###################
clean_A("DOX_2002-2003")
clean_A("DOX_2004-2005")
clean_A("DOX_2002-2008")
clean_A("DOX_2010-2013")

data_list_DOX <- import_list("QAEHS Updated Chemical Biomarker Datasets_back.xlsx", which = c(10,11,12,13),
                             setclass = "tbl", rbind = TRUE) %>% select(-1)

data_list_DOX <- select(data_list_DOX, -1)

write.xlsx(data_list_DOX
           , file = "DOX_merged.xlsx"
           , sheetName = "DOX_merged")

################ OCP CLEAN ###################
clean_B("OCPs_2002&2008")
clean_B("OCPs_2006-2007")
clean_A("OCPs_2010-2011")
clean_B("OCPs_2012-2013")

data_list_OCPs <- import_list("QAEHS Updated Chemical Biomarker Datasets_back.xlsx", which = c(14,15,16,17),
                             setclass = "tbl", rbind = TRUE) %>% select(-1)
data_list_OCPs <- select(data_list_OCPs, -1)
write.xlsx(data_list_OCPs
           , file = "OCPs_merged.xlsx"
           , sheetName = "OCPs_merged")

################ PFASs CLEAN ###################
clean_B("PFAS_2006-2007")
clean_B("PFAS_2002&2009")
clean_B("PFAS_2010-2011")
# clean_C("PFASscheck_2010-2011")
clean_B("PFAS_2012-2013")
clean_D("PFAS_2015&2017")

data_list_PFASs <- import_list("QAEHS Updated Chemical Biomarker Datasets_back.xlsx", which = c(18,19,20,21,22),
                              setclass = "tbl", rbind = TRUE) %>% select(-1)


write.xlsx(data_list_PFASs
           , file = "PFASs_merged.xlsx"
           , sheetName = "PFASs_merged")
