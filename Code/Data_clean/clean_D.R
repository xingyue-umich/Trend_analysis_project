clean_D <- function(file_name_of_datasets)
{
  dataset_chem_uncleaned <- readxl::read_excel("../../Data/Chemical concentration database QAEHS7d.xlsx"
                                               , sheet = file_name_of_datasets)
  colnames(dataset_chem_uncleaned)
  num_rows <- nrow(dataset_chem_uncleaned)
  print(num_rows)
  # Find LOD of each chemical
  
  lods <- dataset_chem_uncleaned[num_rows,] %>%
    select_if(function(x) any(!is.na(x))) %>%
    # The last row of 'sample_code_original' is 'LOD'
    # dplyr::select(-sample_code_original_PFAS) %>%
    unlist()
  print(lods)
  
  
  chem_codenames <- names(lods)
  print(chem_codenames)

  
  dataset_chem_uncleaned_long <- dataset_chem_uncleaned %>%
    # select rows except the last one
    subset(!is.na(globally_unique_sample_id)) %>%
    gather(key = "chem"
           , value = "chem_value"
           , chem_codenames) 
  
  # Create new columns basesd on C,H,D
  new_dataset <- dataset_chem_uncleaned_long %>%
    mutate(D = ifelse(grepl("<"
                            , chem_value)
                      , as.numeric(gsub("<"
                                        , ""
                                        , as.numeric(lods)))
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
             , file = "QAEHS Updated Chemical Biomarker Datasets.xlsx"
             , sheetName = file_name_of_datasets
             , append = TRUE)
}

