mesa_model_results <- list()

for (clock in epigenetic_clock_variables_chr) {
  for (exposure in c(early_life_exposures_mesa, adult_life_exposures_mesa)) {
    
    # analysis I 
    mesa_model_results[[length(mesa_model_results)+1]] <- 
      list(lm(
        formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK"),
        data = mesa_usborn) %>% broom::tidy(conf.int = TRUE) %>% 
          bind_cols(model = 'analysis_i_no_svs', exposure = exposure, clock = clock, dataset='mesa') %>% 
          filter(stringr::str_detect(term, exposure)))
    
    # load svs for analysis I 
    svs_for_analysis_i <- readr::read_csv(
      system.file(
        paste0('surrogate_variables/mesa_analysis_1/', lookup_for_mesa_sv_filenames[exposure], '_mesa_svs_analysis1.csv'),
        package = 'DNAm'
      ),
      show_col_types = FALSE
    )
    svs_for_analysis_i %<>% rename(id = 1)
    svs_for_analysis_i$id %<>% as.character()
    
    # merge surrogate variables into MESA
    mesa_w_surrogate_variables_analysis_i <- 
      mesa_usborn %>% left_join(svs_for_analysis_i,
                                by = c('idno' = 'id'))
    
    # fit analysis I model with SVs
    mesa_model_results[[length(mesa_model_results) + 1]] <- list(lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10"),
      data = mesa_w_surrogate_variables_analysis_i) %>% broom::tidy(conf.int = TRUE) %>% 
        bind_cols(model = 'analysis_i_with_svs', exposure = exposure, clock = clock, dataset='mesa') %>% 
        filter(stringr::str_detect(term, exposure)))
    
    # analysis II with no SVs
    mesa_model_results[[length(mesa_model_results) + 1]] <- list(lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + bmi + smoke_now"),
      data = mesa_usborn) %>% broom::tidy(conf.int = TRUE) %>% 
        bind_cols(model = 'analysis_ii_no_svs', exposure = exposure, clock = clock, dataset='mesa') %>% 
        filter(stringr::str_detect(term, exposure)))
    
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
    
    # fit analysis I model with SVs
    mesa_model_results[[length(mesa_model_results) + 1]] <- list(lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + bmi + smoke_now + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10"),
      data = mesa_w_surrogate_variables_analysis_ii) %>% broom::tidy(conf.int = TRUE) %>% 
        bind_cols(model = 'analysis_ii_with_svs', exposure = exposure, clock = clock, dataset='mesa') %>% 
        filter(stringr::str_detect(term, exposure)))
  }
}
