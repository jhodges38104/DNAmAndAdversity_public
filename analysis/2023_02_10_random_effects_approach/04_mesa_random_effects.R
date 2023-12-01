
# mesa random effects -----------------------------------------------------

mesa_re_model_results <- list()

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
  
  # merge surrogate variables into MESA
  mesa_w_surrogate_variables_analysis_ii <- 
    mesa_long %>% left_join(svs_for_analysis_ii,
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
      clock_estimate,
      idno,
      clock,
      age,
      race,
      !! interacted_exposure_names,
      bmi, 
      smoke_now,
      sex_gender,
      !! cell_type_variables_chr,
      !! paste0("sv", 1:10)) %>% 
    replace_dash_and_space_colnames() %$% 
    mice(data = ., m = 40, seed = 1234, printFlag = FALSE)
  
  # create regression model
  model <- with(data = imputed_dataset, lmer(
    formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10")))
  
  pooled_model <- pool(model)
  
  # fit a model for only first generation clocks
  model_1st_gen <- with(
    data = imputed_dataset %>% filter(clock %in% gen_1_clocks),
    lmer(
    formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10
      ")))
  
  pooled_model_1st_gen <- pool(model_1st_gen)
  
  
  # fit a model for only age-estimator 1st gen clocks  
  model_1st_gen_age_estimators <- with(
    data = imputed_dataset %>% filter(clock %in% gen_1_clocks_shorter),
    lmer(formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10
      ")))

  pooled_model_1st_gen_age_estimators <- pool(model_1st_gen_age_estimators)
  
  # fit a model for only second generation clocks
  model_2nd_gen <- with(
    data = imputed_dataset %>% filter(clock %in% gen_2_clocks),
    lmer(
    formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10
      ")))
  
  pooled_model_2nd_gen <- pool(model_2nd_gen)
  
  effect_estimates <- 
    bind_rows(
      # overall (1st and 2nd combined) 
      broom.mixed::tidy(pooled_model, conf.int = TRUE) %>% 
        mutate(clock_generation = '1st and 2nd Pooled', clock = clock_generation),
      
      # 1st generation summary 
      broom.mixed::tidy(pooled_model_1st_gen, conf.int = TRUE) %>% 
        mutate(clock_generation = '1st Pooled (all clocks)', clock = clock_generation),
      
      # 1st gen age estimators 
      broom.mixed::tidy(pooled_model_1st_gen_age_estimators, conf.int = TRUE) %>% 
        mutate(clock_generation = '1st Pooled (age-estimators)', clock = clock_generation),
      
      # 2nd generation summary
      broom.mixed::tidy(pooled_model_2nd_gen, conf.int = TRUE) %>% 
        mutate(clock_generation = '2nd Pooled', clock = clock_generation),
    )
  
  # store the model results
  mesa_re_model_results[[length(mesa_re_model_results)+1]] <- effect_estimates %>% 
    filter(stringr::str_detect(term, exposure)) 
}

# rowbind the results together 
mesa_re_model_results <- bind_rows(mesa_re_model_results)

# separate the exposure term into the race part and the exposure term
mesa_re_model_results$term %<>% stringr::str_remove_all("`") 
mesa_re_model_results %<>% separate(term, sep = "_interacted_with_", into = c('race', 'term'))

# use the exposure_variables_namer to add the human readable "term_str" 
mesa_re_model_results %<>% mutate(term_str = setNames(nm = exposure_variables_named_chr, names(exposure_variables_named_chr))[term])

# call the 2.5% and 97.5% CI "low" and "high" 
# colnames(mesa_re_model_results)[3:4] <- c('low', 'high')

# remove the "race" prefix from the entries in the race column
mesa_re_model_results$race %<>% stringr::str_remove_all("race_|race")
mesa_re_model_results$race %<>% tools::toTitleCase() # convert to Title Case


# rename the race terms in a standard fashion
mesa_re_model_results$race <- case_when(
  mesa_re_model_results$race == "White_non_Hispanic" ~ "White non-Hispanic",
  mesa_re_model_results$race == "Black_non_Hispanic" ~ "Black non-Hispanic",
  mesa_re_model_results$race == 'Hispanic' ~ 'Hispanic',
  mesa_re_model_results$race == "_black_non_hispanic" ~ "Black non-Hispanic",
  mesa_re_model_results$race == "_white_non_hispanic" ~ "White non-Hispanic",
  mesa_re_model_results$race == "_hispanic" ~ "Hispanic"
  )

