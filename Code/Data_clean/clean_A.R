clean_A <- function(file_name_of_datasets)
{
  dataset <- read_excel("../../Data/Chemical concentration database QAEHS7d.xlsx"
                                       , sheet = file_name_of_datasets) %>%
                          subset(!is.na(globally_unique_sample_id)) 
  colnames(dataset)
  # Create lod columns
  # For next step, corrected(C) and (H) columns are calculated based on lod
  lod_columns <- colnames(dataset)[grep("SWD|SLD", colnames(dataset))]
  print(lod_columns)
  
  value_columns <- colnames(dataset)[grep("SWV|SLV", colnames(dataset))]
  print(value_columns)
  
  detected_columns <- colnames(dataset)[grep("SWN|SLN", colnames(dataset))]
  print(value_columns)
  
  dataset <- dataset[, colnames(dataset) %in% c(lod_columns,value_columns,detected_columns,'sample_id')]
  
  lod <- dataset %>%
    dplyr::select(lod_columns) %>%
    gather(key = "lod"
           , value = "lod_value"
           , lod_columns)
  
  dataset_long <- dataset %>%
    gather(key = "chem"
           , value = "chem_value"
           , value_columns) %>%
    cbind(lod)
  
  
  # Create new columns based on C,H,D
  
  dataset_long <- dataset_long %>%
    mutate(H = ifelse(chem_value < lod_value
                      , as.numeric(lod_value)/(2)
                      , as.numeric(chem_value))) %>%
    mutate(C = ifelse(chem_value < lod_value
                      , as.numeric(lod_value)/sqrt(2)
                      , as.numeric(chem_value))) %>%
    mutate(D = lod_value)
  
  # change Column names
  
    new_dataset  <- dataset_long %>%
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
