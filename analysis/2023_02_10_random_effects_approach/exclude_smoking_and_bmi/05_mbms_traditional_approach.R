# perform traditional approach --------------------------------------------

mbms_model_results <- list()

mbms_models <- list()

for (clock in epigenetic_clock_variables_chr) {
  for (exposure in c(early_life_exposures_mbms, adult_life_exposures_mbms)) {
    
    # load svs for analysis II
    svs_for_analysis_ii <- readr::read_csv(
      system.file(
        paste0('surrogate_variables/mbms/', lookup_for_mbms_sv_filenames[exposure], '_mbms_svs_analysis1.csv'),
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
    
    # create an imputed dataset
    imputed_dataset <- mbms_w_surrogate_variables_analysis_ii %>% 
      select(
        idno,
        !! clock,
        age,
        race,
        !! exposure,
        sex_gender,
        !! cell_type_variables_chr,
        !! paste0("sv", 1:5)) %$% 
      mice(data = ., m = 40, seed = 1234, printFlag = FALSE)
    
    model_formula <- paste0(
      clock,
      " ~ age + sex_gender + race / ",
      exposure,
      " + Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5")
    
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
