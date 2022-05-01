age_group <- function(chem_merged){
  chem_merged <- chem_merged %>% mutate(
    age_group = as.factor(case_when(
      mean_age <= 4.999999  & mean_age > 0 ~ "0-5",
      mean_age <= 14.999999 & mean_age >= 5 ~"5-15",
      mean_age <= 29.999999  & mean_age >= 15 ~"15-30",
      mean_age <= 44.999999  & mean_age >= 30 ~"30-45",
      mean_age <= 59.999999  & mean_age >= 45 ~"45-60",
      mean_age >= 60 ~">60")))
  return(chem_merged)
}

select_SLV <- function(chem_merged){
  SLV_cols <- chem_merged %>% 
    dplyr:: select(c("sample_id"),c("age_group"), c("sample_year"), c("sex"),c("mean_age"), grep("SLV", names(chem_merged)))
  return(SLV_cols)
}

select_SWV <- function(chem_merged){
  SLV_cols <- chem_merged %>% 
    dplyr:: select(c("sample_id"), c("age_group"), c("sample_year"), c("sex"), c("mean_age"), 
                   grep("SWV", names(chem_merged)))
  return(SLV_cols)
}

select_SLD <- function(chem_merged){
  SLD_cols <- chem_merged %>% 
    dplyr:: select(c("sample_id"), c("age_group"), c("sample_year"), c("sex"),c("mean_age"), grep("SLD", names(chem_merged)))
  return(SLD_cols)
}

select_SWD <- function(chem_merged){
  SLV_cols <- chem_merged %>% 
    dplyr:: select(c("sample_id"), c("age_group"), c("sample_year"), c("sex"),c("mean_age"), grep("SWD", names(chem_merged)))
  return(SLV_cols)
}

select_SWN <- function(chem_merged){
  SWN_cols <- chem_merged %>% 
    dplyr:: select(c("sample_id"), c("age_group"), c("sample_year"), c("sex"),c("mean_age"), grep("SWN", names(chem_merged)))
  return(SWN_cols)
}

SLC <- function(chem_sorted){
  mutate(chem_sorted,
         SLC = ifelse(detected == "detected", as.numeric(SLV), as.numeric(as.numeric(SLD)/sqrt(2))),
         value = ifelse(detected == "detected", as.numeric(SLV),as.numeric(SLC)))
}

SWC <- function(chem_sorted){
  mutate(chem_sorted,
         SWC = ifelse(detected == "detected", as.numeric(SWV), as.numeric(as.numeric(SWD)/sqrt(2))),
         value = ifelse(detected == "detected", as.numeric(SWV),as.numeric(SWC)))
}

log_value <- function(chem_sorted){
  chem_sorted <- mutate(chem_sorted,
                            log_value = log10(as.numeric(chem_sorted$value)),
                            value_detected = ifelse(detected == "detected", as.numeric(value),NA),
                            log_value_detected = ifelse(detected == "detected", as.numeric(log_value),NA))
  return(chem_sorted)
}

plot_year_log_regD <- function(list_all_chem){
  lapply(list_all_chem,function(x){
    ggplot(x, aes(x= as.factor(sample_year), group = age_group)) +
      xlab("sample year") +
      ylab("log(value)") +
      geom_point(aes(y = log_value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
      #geom_point(aes(y = SLD), size = 1.5,alpha = 3/4) +
      stat_smooth(data = subset(x,detected == "detected"),
                  aes(y= log_value, group = sex,color = sex),linetype = "twodash",size = 0.65,
                  method = "lm", formula = y ~ x + I(x^2), se = F, show.legend = FALSE) +
      facet_grid(order+chem_SLV ~ age_group, scales = "free_y")+
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
  }

plot_year_log_regD_W <- function(list_all_chem){
  lapply(list_all_chem,function(x){
    ggplot(x, aes(x= as.factor(sample_year), group = age_group)) +
      xlab("sample year") +
      ylab("log(value)") +
      geom_point(aes(y = log_value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
      #geom_point(aes(y = SWD), size = 1.5,alpha = 3/4) +
      stat_smooth(data = subset(x,detected == "detected"),
                  aes(y= log_value, group = sex,color = sex),size = 0.7, linetype = "twodash", 
                  method = "lm", formula = y ~ x + I(x^2), se = F,show.legend = FALSE) +
      facet_grid(order+chem_SWV ~ age_group, scales = "free_y")+
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
}

plot_year_regD <- function(list_all_chem){
  lapply(list_all_chem, function(x){
    ggplot(x, aes(x= as.factor(sample_year), group = age_group)) +
      xlab("sample year") +
      geom_point(aes(y = value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
      #geom_point(aes(y = SLD), size = 1.5,alpha = 3/4) +
      geom_smooth(data = subset(x,detected == "detected"), 
                  aes(y = value, group = sex, color = sex), size = 0.7, linetype = "twodash",
                  method = "lm", se = FALSE,show.legend = FALSE) +
      facet_grid(order+chem_SLV ~ age_group, scales = "free_y")+
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
}

plot_year_regD_W <- function(list_all_chem){
  lapply(list_all_chem, function(x){
    ggplot(x, aes(x= as.factor(sample_year), group = age_group)) +
      xlab("sample year") +
      geom_point(aes(y = value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
      #geom_point(aes(y = SWD), size = 1.5,alpha = 3/4) +
      geom_smooth(data = subset(x,detected == "detected"), 
                  aes(y = value, group = sex, color = sex), size = 0.7, linetype = "twodash",
                  method = "lm", formula = y ~ ploy(x,2), se = FALSE,show.legend = FALSE) +
      facet_grid(order+chem_SWV ~ age_group, scales = "free_y")+
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
}

plot_age_regD <- function(list_all_chem){
  lapply(list_all_chem, function(x){
    ggplot(x, aes(x= as.numeric(mean_age), group = sample_year)) +
      xlab("mean_age") +
      ylab("value") +
      geom_point(aes(y = value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
      #geom_point(aes(y = SLD), size = 1.5,alpha = 3/4) +
      geom_smooth(data = subset(x, detected == "detected"), 
                  aes(y= value, group = sex,color = sex),linetype = "twodash",size = 0.65,
                  method = "lm",formula = y ~ ploy(x,2),se = F,show.legend = FALSE) +
      facet_grid(order+chem_SLV ~ sample_year, scales = "free_y")+
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
}

regD_W <- function(list_all_chem){
  lapply(list_all_chem, function(x){
    ggplot(x, aes(x= as.numeric(mean_age), group = sample_year)) +
      xlab("mean_age") +
      ylab("value") +
      geom_point(aes(y = value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
      #geom_point(aes(y = SWD), size = 1.5,alpha = 3/4) +
      geom_smooth(data = subset(x, detected == "detected"), 
                  aes(y= value, group = sex,color = sex),linetype = "twodash",size = 0.65,
                  method = "lm",formula = y ~ ploy(x,2),se = F, show.legend = FALSE) +
      facet_grid(order+chem_SWV ~ sample_year, scales = "free_y")+
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
}

plot_age_log_regD <- function(list_all_chem){
  lapply(list_all_chem,function(x){
      ggplot(x, aes(x= as.numeric(mean_age), group = sample_year)) +
        xlab("mean_age") +
        ylab("log(value)") +
        geom_point(aes(y = log_value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
        scale_color_manual(values=c("turquoise","tomato")) +
        scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
        #geom_point(aes(y = SLD), size = 1.5,alpha = 3/4) +
        stat_smooth(data = subset(x,detected == "detected"),
                    aes(y= log_value, group = sex,color = sex),linetype = "twodash",size = 0.65,
                    method = "lm", formula=y~poly(x,2),se = F, show.legend = FALSE) +
        facet_grid(order+chem_SLV ~ sample_year, scales = "free_y")+
        theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
}

plot_age_log_regD_W <- function(list_all_chem){
  lapply(list_all_chem,function(x){
    ggplot(x, aes(x= as.numeric(mean_age), group = sample_year)) +
      xlab("mean_age") +
      ylab("log(value)") +
      geom_point(aes(y = log_value, shape = detected, color = sex), size = 1.8, show.legend = TRUE) +
      scale_color_manual(values=c("turquoise","tomato")) +
      scale_shape_manual(values=c('detected' = 16, 'non-detected' = 17)) +
      #geom_point(aes(y = SWD), size = 1.5,alpha = 3/4) +
      geom_smooth(data = subset(x,detected == "detected"),
                  aes(y= log_value, group = sex,color = sex),linetype = "twodash",size = 0.65,
                  method = "lm",formula = y ~ I(x^2)+ x,se = FALSE,show.legend = FALSE) +
      facet_grid(order+chem_SWV ~ sample_year, scales = "free_y")+
      theme(axis.text.x = element_text(angle = 320, size = 6,face = "bold"), legend.direction = "vertical", legend.box = "vertical")})
}

# detect_rate <- function(chem_sorted){
#   chem_sorted$detected <- forcats::fct_explicit_na(chem_sorted$detected)
#   count_all <- chem_sorted %>% dplyr::count(`chem_name`,`sample_year`)
#   count <- chem_sorted %>% dplyr::count(`chem_name`,`sample_year`,`detected`)
#   count_detected <- count %>% subset(detected == "detected") 
#   count_detected <- merge(count_detected,count_all,by = c("chem_name","sample_year")) 
#   colnames(count_detected)[4:5] <- c("detected_num","total_num")
#   # count_detected$detected_rate <- formattable::percent(count_detected %>% with(`detected_num`/`total_num`)) 
#   count_detected$detection_rate <- as.numeric(count_detected$detected_num/count_detected$total_num)
#   write.xlsx(count_detected 
#              , file = "Rate of Detection.xlsx"
#              , sheetName = chem_sorted
#              , append = TRUE)
# }

detect_rate <- function(chem_sorted){
  chem_sorted$detected <- forcats::fct_explicit_na(chem_sorted$detected)
  count_all <- chem_sorted %>% dplyr::count(`chem_name`)
  count <- chem_sorted %>% dplyr::count(`chem_name`,`detected`)
  count_detected <- count %>% subset(detected == "detected") 
  count_detected <- merge(count_detected,count_all,by = c("chem_name")) 
  colnames(count_detected)[3:4] <- c("detected_num","total_num")
  # count_detected$detected_rate <- formattable::percent(count_detected %>% with(`detected_num`/`total_num`)) 
  count_detected$detection_rate <- as.numeric(count_detected$detected_num/count_detected$total_num)
  write.xlsx(count_detected 
             , file = "Rate of Detection.xlsx"
             , sheetName = chem_sorted
             , append = TRUE)
}

reg_estimate <- function(chem_sorted_reg_list){
  estimate <- 
    lapply(chem_sorted_reg_list, function(x){
      reg_i <-  lm(log_value ~ sex_rel + mean_age_rel + mean_age_2 + sample_year_rel + sample_year_2,
                 data = x)
      return(summary(reg_i)$coefficient[,1])})
    
  estimate_df <- as.data.frame(do.call("rbind",estimate))
  estimate_df$chem <- row.names(estimate_df)
  
  rownames(estimate_df) <- NULL
  return(estimate_df)
  }

reg_Pr <- function(chem_sorted_reg_list){
  Pr <- 
    lapply(chem_sorted_reg_list, function(x){
      reg_i <-  lm(log_value ~ sex_rel + mean_age_rel + mean_age_2 + sample_year_rel + sample_year_2,
                   data = x)
      return(summary(reg_i)$coefficient[,4])})
  
  Pr_df <- as.data.frame(do.call("rbind",Pr))
  Pr_df$chem <- row.names(Pr_df)
  rownames(Pr_df) <- NULL

  return(Pr_df)
}
