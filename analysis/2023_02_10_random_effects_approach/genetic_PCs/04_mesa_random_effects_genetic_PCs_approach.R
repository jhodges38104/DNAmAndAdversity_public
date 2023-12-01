
# mesa random effects -----------------------------------------------------

mesa_re_model_results <- list()

for (exposure in c(early_life_exposures_mesa, adult_life_exposures_mesa)) {
  
  # load svs for analysis II
  svs_for_analysis_i <- readr::read_csv(
    system.file(
      paste0('surrogate_variables/mesa_analysis_1/', lookup_for_mesa_sv_filenames[exposure], '_mesa_svs_analysis1.csv'),
      package = 'DNAm'
    ),
    show_col_types = FALSE
  )
  svs_for_analysis_i %<>% rename(id = 1)
  svs_for_analysis_i$id %<>% as.character()
  
  # merge surrogate variables into MBMS
  mesa_w_surrogate_variables_analysis_i <- 
    mesa_long %>% left_join(svs_for_analysis_i,
                            by = c('idno' = 'id'))
  
  # create regression model
  model <- lmer(
    formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      age + 
      race / ", exposure, " + 
      bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + 
      sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10
      "),
    data = mesa_w_surrogate_variables_analysis_i)
  
  
  effect_estimates <- 
      # overall (1st and 2nd combined) 
      confint(model, oldNames = FALSE, level = .95) |> 
        as.data.frame() |> 
        tibble::rownames_to_column('term') |> 
        mutate(clock_generation = '1st and 2nd Pooled', clock = clock_generation)
  
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
colnames(mesa_re_model_results)[3:4] <- c('low', 'high')

# remove the "race" prefix from the entries in the race column
mesa_re_model_results$race %<>% stringr::str_remove_all("race")
