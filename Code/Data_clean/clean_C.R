clean_C <- function(file_name_of_datasets)
{
  dataset <- readxl::read_excel("Chemical concentration database QAEHS7d.xlsx"
                                       , sheet = file_name_of_datasets)
  colnames(dataset)
  num_rows <- nrow(dataset)
  print(num_rows)
  
  # Find LOD of each chemical
  lods <- dataset[num_rows,] %>%
    select_if(function(x) any(!is.na(x))) 
  # The last row of 'sample_code_original' is 'LOD'
  # dplyr::select(-sample_code_original_PFAS) %>%
  # unlist()
  print(lods)
  
  num_lods <- ncol(lods)
  chem_codenames <- names(lods[2:num_lods])
  print(chem_codenames)
  
  dataset_long <- dataset %>%
    # select rows except the last one
    subset(!is.na(globally_unique_sample_id)) %>%
    gather(key = "chem"
           , value = "chem_value"
           , all_of(chem_codenames)) 
  
  # Create new columns basesd on C,H,D
  new_dataset <- dataset_long %>%
    mutate(D = ifelse(grepl("<"
                            , chem_value)
                      , as.numeric(gsub("<"
                                        , ""
                                        , chem_value))
                      , as.numeric(lods))) %>%
    mutate(N = ifelse(grepl("<"
                            , chem_value)
                      , 1
                      , 0)) %>%
    mutate(H = ifelse(grepl("<"
                            , chem_value)
                      , as.numeric(D)/(2)
                      , as.numeric(chem_value))) %>%
    mutate(C = ifelse(grepl("<"
                            , chem_value)
                      , as.numeric(D)/sqrt(2)
                      , as.numeric(chem_value))) %>%
    mutate(chem_value = ifelse(grepl("<"
                                     , chem_value)
                               , 0
                               , as.numeric(chem_value)))
  
  new_dataset <- new_dataset %>%
    gather(key = "type", value = "value", chem_value:C) %>%
    # gsub(pattern, replacement, x)
    # mutate "chem" = change the name in the old "chem" columns, you change change [mutate "chem"] to [mutate "chem2"]
    mutate(chem = case_when(type == "chem_value" ~ chem
                            , type == "C" ~ gsub("V", "C", chem)
                            , type == "N" ~ gsub("V", "N", chem)
                            , type == "H" ~ gsub("V", "H", chem)
                            , type == "D" ~ gsub("V","D",chem))) %>%
    
    dplyr::select(-c(type)) %>%
    spread(chem, value) 
  
  
  write.xlsx(new_dataset
             , file = "QAEHS Updated Chemical Biomarker Datasets_back.xlsx"
             , sheetName = file_name_of_datasets
             , append = TRUE)
  
}

