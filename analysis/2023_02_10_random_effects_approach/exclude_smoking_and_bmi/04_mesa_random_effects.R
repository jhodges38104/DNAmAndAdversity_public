
# mesa random effects -----------------------------------------------------

mesa_re_model_results <- list()

for (exposure in c(early_life_exposures_mesa, adult_life_exposures_mesa)) {
  
  # load svs for analysis II
  svs_for_analysis_ii <- readr::read_csv(
    system.file(
      paste0('surrogate_variables/mesa_analysis_1/', lookup_for_mesa_sv_filenames[exposure], '_mesa_svs_analysis1.csv'),
      package = 'DNAm'
    ),
    show_col_types = FALSE
  )
  svs_for_analysis_ii %<>% rename(id = 1)
  svs_for_analysis_ii$id %<>% as.character()
  
  # merge surrogate variables into MBMS
  mesa_w_surrogate_variables_analysis_ii <- 
    mesa_long %>% left_join(svs_for_analysis_ii,
                            by = c('idno' = 'id'))
  
  # create an imputed dataset
  imputed_dataset <- mesa_w_surrogate_variables_analysis_ii %>% 
    select(
      clock_estimate,
      idno,
      clock,
      age,
      race,
      !! exposure,
      bmi, 
      smoke_now,
      sex_gender,
      !! cell_type_variables_chr,
      !! paste0("sv", 1:10)) %$% 
    mice(data = ., m = 40, seed = 1234, printFlag = FALSE)
  
  # create regression model
  model <- with(data = imputed_dataset, lmer(
    formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race / ", exposure, " + 
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
      race / ", exposure, " + 
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
      race / ", exposure, " + 
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
      race / ", exposure, " + 
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
mesa_re_model_results %<>% separate(term, sep = ":", into = c('race', 'term'))

# use the exposure_variables_namer to add the human readable "term_str" 
mesa_re_model_results %<>% mutate(term_str = setNames(nm = exposure_variables_named_chr, names(exposure_variables_named_chr))[term])

# call the 2.5% and 97.5% CI "low" and "high" 
# colnames(mesa_re_model_results)[3:4] <- c('low', 'high')

# remove the "race" prefix from the entries in the race column
mesa_re_model_results$race %<>% stringr::str_remove_all("race")
