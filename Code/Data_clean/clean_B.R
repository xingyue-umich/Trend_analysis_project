clean_B <- function(file_name_of_datasets)
{
  dataset <- read_excel("../../Data/Chemical concentration database QAEHS7d.xlsx"
                                       , sheet = file_name_of_datasets) %>%
    subset(!is.na(globally_unique_sample_id)) 
  
  # print(colnames(dataset))
  # # Only for PCN 2011 - 2013
  # # file_name_of_datasets <- select(file_name_of_datasets,-c(58,59,60,61)) 
  # # colnames(dataset_chem_uncleaned)
  
  # ####################################
  # # Transform GND,G,NR to numeric
  subset_columns <- c(colnames(dataset)[grep("SWN|SVN", colnames(dataset))])
  subset <- dataset[,c(subset_columns)]
  #  According to "Variable Type List"
  #  lod values are calculated as 0,1,2,3,4 when SWN/SVN is G,GND,NQ,NR,GBLS
  subset[subset == "G"] <- "0"
  subset[subset == "GND"] <- "1"
  subset[subset == "NQ"] <- "2"
  subset[subset == "NR"] <- "3"
  subset[subset == "GBLS"] <- "4"
  subset$id <- 1:nrow(subset)
  subset <- as.data.frame(sapply(subset, as.numeric))
  
  left_columns <- c(colnames(dataset)[!grepl("SWN|SVN", colnames(dataset))])
  leftset <- dataset[,c(left_columns)]
  leftset$id <- 1:nrow(leftset)
  
  dataset <- merge(leftset,subset,by='id') %>% select(-id)

  # Create lod columns
  # # For next step, corrected(C) and H columns are calculated based on lod
  lod_columns <- colnames(dataset)[grep("SWD|SLD", colnames(dataset))]
  print(lod_columns)

  value_columns <- colnames(dataset)[grep("SWV|SLV", colnames(dataset))]
  print(value_columns)
  
  detected_columns <- colnames(dataset)[grep("SWN|SLN", colnames(dataset))]
  print(value_columns)
  
  dataset <- dataset[, colnames(dataset) %in% c(lod_columns,value_columns, detected_columns,'sample_id')]
  
  lod <- dataset %>%
    dplyr::select(all_of(lod_columns)) %>%
    gather(key = "lod"
           , value = "lod_value"
           , all_of(lod_columns))

  dataset_long <- dataset %>%
    gather(key = "chem"
           , value = "chem_value"
           , value_columns) %>%
    cbind(lod)


  # Create new columns basesd on C,H,D

  dataset_long <- dataset_long %>%
    mutate(H = ifelse(chem_value < lod_value
                      , as.numeric(lod_value)/(2)
                      , as.numeric(chem_value))) %>%
    mutate(C = ifelse(chem_value < lod_value
                      , as.numeric(lod_value)/sqrt(2)
                      , as.numeric(chem_value))) %>%
    mutate(D = lod_value)

  # change Column names

    new_dataset <- dataset_long %>%
    dplyr::select(-c(lod,lod_value)) %>%
    gather(key = "type", value = "value", chem_value:D) %>%
    # gsub(pattern, replacement, x)
    mutate(chem = case_when(type == "chem_value" ~ chem
                            , type == "C" ~ gsub("V", "C", chem)
                            , type == "H" ~ gsub("V", "H", chem)
                            , type == "D" ~ gsub("V","D",chem))) %>%

    dplyr::select(-c(type)) %>%
    spread(chem, value)

    write.xlsx(new_dataset
             , file = "QAEHS Updated Chemical Biomarker Datasets.xlsx"
             , sheetName = file_name_of_datasets
             , append = TRUE)
}
