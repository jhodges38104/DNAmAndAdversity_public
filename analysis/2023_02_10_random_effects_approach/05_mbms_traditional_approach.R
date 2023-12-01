# perform traditional approach --------------------------------------------

mbms_model_results <- list()

mbms_models <- list()

for (clock in epigenetic_clock_variables_chr) {
  for (exposure in c(early_life_exposures_mbms, adult_life_exposures_mbms)) {
    
    # load svs for analysis II
    svs_for_analysis_ii <- readr::read_csv(
      system.file(
        paste0('surrogate_variables/mbms/', lookup_for_mbms_sv_filenames[exposure], '_mbms_svs_analysis2.csv'),
        package = 'DNAm'
      ), 
      show_col_types = FALSE
    )
    svs_for_analysis_ii %<>% rename(id = 1)
    
    # lookup and assign ids based on sentrix_ids 
    svs_for_analysis_ii$id <- 
      sentrix_to_mbms_id_converter[svs_for_analysis_ii$id]
    svs_for_analysis_ii$id %<>% as.character()
    
    # merge surrogate variables into MBMS
    mbms_w_surrogate_variables_analysis_ii <- 
      mbms %>% left_join(svs_for_analysis_ii,
                         by = c('idno' = 'id'))
    
    # create dummy variables for racialized groups
    mbms_w_surrogate_variables_analysis_ii_racialized <- categorical_to_dummies(mbms_w_surrogate_variables_analysis_ii,
                                                                                variable = 'race', drop_categorical = FALSE)
    
    # if the exposure is logical or quantitative, we can create the interaction term fairly programmatically:
    if (is.numeric(mbms_w_surrogate_variables_analysis_ii_racialized[[exposure]]) | 
        is.logical(mbms_w_surrogate_variables_analysis_ii_racialized[[exposure]])) { 
      
      mbms_w_surrogate_variables_analysis_ii_racialized %<>% mutate(
        "raceblack_non_hispanic_interacted_with_{exposure}" := get(exposure) * `race_black non-hispanic`,
        "racewhite_non_hispanic_interacted_with_{exposure}" := get(exposure) * `race_white non-hispanic`)
      
      interacted_exposure_names <- c(
        glue::glue("raceblack_non_hispanic_interacted_with_{exposure}"),
        glue::glue("racewhite_non_hispanic_interacted_with_{exposure}"))
      
      # if the exposure is categorical, we'll need to build the interaction term more manually:
    } else {
      
      exposure_categories <- na.omit(unique(mbms_w_surrogate_variables_analysis_ii_racialized[[exposure]]))
      
      # since parents education and education have been collapsed to binary (categorical) variables, 
      # only use the lower of the two levels — 
      # and for the EOD measure, the chosen reference category is 1-2
      if (exposure %in% c('parents_educ', 'educ', 'eod_racial_3_levels')) {
        exposure_categories <- setdiff(exposure_categories, c("parents_4yr+_college_degree", "4yr+_college_degree", "1_2"))
      }
      
      racialized_exposures <- 
        expand.grid(
          race = c("race_black non-hispanic", "race_white non-hispanic"),
          exposure = exposure_categories)
      
      racialized_exposures$interaction_names <-
        replace_plus_with_p(paste0(
          racialized_exposures$race,
          "_interacted_with_",
          exposure, "_", 
          racialized_exposures$exposure
        ))
      
      mbms_w_surrogate_variables_analysis_ii_racialized %<>% categorical_to_dummies(variable = {{ exposure }}, drop_categorical = FALSE)
      
      for (row_i in 1:nrow(racialized_exposures)) {
        mbms_w_surrogate_variables_analysis_ii_racialized[
          racialized_exposures$interaction_names[row_i]] <- 
          mbms_w_surrogate_variables_analysis_ii_racialized[,racialized_exposures$race[row_i]] * 
          mbms_w_surrogate_variables_analysis_ii_racialized[,paste0(exposure, "_", racialized_exposures$exposure[row_i])]
      }
      
      interacted_exposure_names <- racialized_exposures$interaction_names
    }
    
    
    
    # create an imputed dataset
    imputed_dataset <- mbms_w_surrogate_variables_analysis_ii_racialized %>% 
      select(
        idno,
        !! clock,
        age,
        race,
        !! interacted_exposure_names,
        bmi, 
        smoke_now,
        sex_gender,
        !! cell_type_variables_chr,
        !! paste0("sv", 1:5))  %>% 
      replace_dash_and_space_colnames() %$% 
      mice(data = ., m = 40, seed = 1234, printFlag = FALSE)
    
    model_formula <- paste0(
      clock,
      " ~ age + sex_gender + race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + bmi + smoke_now + sv1 + sv2 + sv3 + sv4 + sv5")
    
    # fit analysis II model with SVs
    mbms_models[[length(mbms_models) + 1]] <- 
      with(data = imputed_dataset,
           lm(formula = as.formula(model_formula))) %>%
      pool() %>%
      broom::tidy(conf.int = TRUE) %>%
      bind_cols(
        model = 'analysis_ii_with_svs',
        exposure = exposure,
        clock = clock,
        dataset = 'mbms'
      )
    mbms_model_results[[length(mbms_model_results) + 1]] <-
      mbms_models[[length(mbms_models)]] %>%
      filter(stringr::str_detect(term, exposure))
  }
}
