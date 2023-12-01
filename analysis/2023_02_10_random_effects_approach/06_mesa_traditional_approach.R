mesa_models <- list()
mesa_model_results <- list()

for (clock in epigenetic_clock_variables_chr) {
  for (exposure in c(early_life_exposures_mesa, adult_life_exposures_mesa)) {
    
    # load svs for analysis II
    svs_for_analysis_ii <- readr::read_csv(
      system.file(
        paste0('surrogate_variables/mesa_analysis_2/', lookup_for_mesa_sv_filenames[exposure], '_mesa_svs_analysis2.csv'),
        package = 'DNAm'
      ),
      show_col_types = FALSE
    )
    svs_for_analysis_ii %<>% rename(id = 1)
    svs_for_analysis_ii$id %<>% as.character()
    
    # merge surrogate variables into MBMS
    mesa_w_surrogate_variables_analysis_ii <- 
      mesa_usborn %>% left_join(svs_for_analysis_ii,
                                by = c('idno' = 'id'))
    
    # create dummy variables for racialized groups
    mesa_w_surrogate_variables_analysis_ii_racialized <- categorical_to_dummies(mesa_w_surrogate_variables_analysis_ii,
                                                                                variable = 'race', drop_categorical = FALSE)
    
    # if the exposure is logical or quantitative, we can create the interaction term fairly programmatically:
    if (is.numeric(mesa_w_surrogate_variables_analysis_ii_racialized[[exposure]]) | 
        is.logical(mesa_w_surrogate_variables_analysis_ii_racialized[[exposure]])) { 
      
      mesa_w_surrogate_variables_analysis_ii_racialized %<>% mutate(
        "raceBlack_non_Hispanic_interacted_with_{exposure}" := get(exposure) * `race_Black non-Hispanic`,
        "raceWhite_non_Hispanic_interacted_with_{exposure}" := get(exposure) * `race_White non-Hispanic`,
        "raceHispanic_interacted_with_{exposure}" := get(exposure) * `race_Hispanic`,
      )
      
      interacted_exposure_names <- c(
        glue::glue("raceBlack_non_Hispanic_interacted_with_{exposure}"),
        glue::glue("raceWhite_non_Hispanic_interacted_with_{exposure}"),
        glue::glue("raceHispanic_interacted_with_{exposure}")
      )
      
      # if the exposure is categorical, we'll need to build the interaction term more manually:
    } else {
      
      exposure_categories <- na.omit(unique(mesa_w_surrogate_variables_analysis_ii_racialized[[exposure]]))
      
      if (exposure %in% c('parents_educ', 'educ', 'mds_racial_3_levels')) {
        exposure_categories <- setdiff(exposure_categories, c("parents_4yr+_college_degree", "4yr+_college_degree", "1_2"))
      }
      
      racialized_exposures <- 
        expand.grid(
          race = c("race_Black non-Hispanic", "race_White non-Hispanic", "race_Hispanic"),
          exposure = exposure_categories)
      
      racialized_exposures$interaction_names <-
        replace_dashes_and_spaces(replace_plus_with_p(paste0(
          racialized_exposures$race,
          "_interacted_with_",
          exposure, "_", 
          racialized_exposures$exposure
        )))
      
      mesa_w_surrogate_variables_analysis_ii_racialized %<>% categorical_to_dummies(variable = {{ exposure }}, drop_categorical = FALSE)
      
      for (row_i in 1:nrow(racialized_exposures)) {
        mesa_w_surrogate_variables_analysis_ii_racialized[
          racialized_exposures$interaction_names[row_i]] <- 
          mesa_w_surrogate_variables_analysis_ii_racialized[,racialized_exposures$race[row_i]] * 
          mesa_w_surrogate_variables_analysis_ii_racialized[,paste0(exposure, "_", racialized_exposures$exposure[row_i])]
      }
      
      interacted_exposure_names <- racialized_exposures$interaction_names
    }
    
    
    # create an imputed dataset 
    imputed_dataset <- mesa_w_surrogate_variables_analysis_ii_racialized %>% 
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
        !! paste0("sv", 1:10)) %$% 
      mice(data = ., m = 40, seed = 1234, printFlag = FALSE)
    
    model_formula <- paste0(
      clock,
      " ~ age + sex_gender + race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + bmi + smoke_now + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10"
    )
    
    # fit analysis II model with SVs
    mesa_models[[length(mesa_models) + 1]] <- 
      mice:::with.mids(data = imputed_dataset,
                       lm(formula = as.formula(model_formula))) %>%
      pool() %>%
      broom::tidy(conf.int = TRUE) %>%
      bind_cols(
        model = 'analysis_ii_with_svs',
        exposure = exposure,
        clock = clock,
        dataset = 'mesa'
      )
    mesa_model_results[[length(mesa_model_results) + 1]] <-
      mesa_models[[length(mesa_models)]] %>%
      filter(stringr::str_detect(term, exposure))
  }
}
