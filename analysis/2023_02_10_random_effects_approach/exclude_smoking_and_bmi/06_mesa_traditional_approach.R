mesa_models <- list()
mesa_model_results <- list()

for (clock in epigenetic_clock_variables_chr) {
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
      mesa_usborn %>% left_join(svs_for_analysis_ii,
                                by = c('idno' = 'id'))
    
    # create an imputed dataset 
    imputed_dataset <- mesa_w_surrogate_variables_analysis_ii %>% 
      select(
        idno,
        !! clock,
        age,
        race,
        !! exposure,
        sex_gender,
        !! cell_type_variables_chr,
        !! paste0("sv", 1:10)) %$% 
      mice(data = ., m = 40, seed = 1234, printFlag = FALSE)
    
    model_formula <- paste0(
      clock,
      " ~ age + sex_gender + race / ",
      exposure,
      " + Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10"
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
