# perform traditional approach --------------------------------------------

mbms_model_results <- list()

for (clock in epigenetic_clock_variables_chr) {
  for (exposure in c(early_life_exposures_mbms, adult_life_exposures_mbms)) {
    
    # analysis I 
    mbms_model_results[[length(mbms_model_results)+1]] <- 
      lm(
        formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK"),
        data = mbms) %>% broom::tidy(conf.int = TRUE) %>% 
      bind_cols(model = 'analysis_i_no_svs', exposure = exposure, clock = clock, dataset='mbms') %>% 
      filter(stringr::str_detect(term, exposure))
    
    # load svs for analysis I 
    svs_for_analysis_i <- readr::read_csv(
      system.file(
        paste0('surrogate_variables/mbms/', lookup_for_mbms_sv_filenames[exposure], '_mbms_svs_analysis1.csv'),
        package = 'DNAm'
      ),
      show_col_types = FALSE
    )
    svs_for_analysis_i %<>% rename(id = 1)
    
    # lookup and assign ids based on sentrix_ids 
    svs_for_analysis_i$id <- 
      sentrix_to_mbms_id_converter[svs_for_analysis_i$id]
    svs_for_analysis_i$id %<>% as.character()
    
    # merge surrogate variables into MBMS
    mbms_w_surrogate_variables_analysis_i <- 
      mbms %>% left_join(svs_for_analysis_i,
                         by = c('idno' = 'id'))
    
    # fit analysis I model with SVs
    mbms_model_results[[length(mbms_model_results) + 1]] <- lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5"),
      data = mbms_w_surrogate_variables_analysis_i) %>% broom::tidy(conf.int = TRUE) %>% 
      bind_cols(model = 'analysis_i_with_svs', exposure = exposure, clock = clock, dataset = 'mbms') %>% 
      filter(stringr::str_detect(term, exposure))
    
    # analysis II with no SVs
    mbms_model_results[[length(mbms_model_results) + 1]] <- lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + bmi + smoke_now"),
      data = mbms) %>% broom::tidy(conf.int = TRUE) %>% 
      bind_cols(model = 'analysis_ii_no_svs', exposure = exposure, clock = clock, dataset='mbms') %>% 
      filter(stringr::str_detect(term, exposure))
    
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
    
    # fit analysis I model with SVs
    mbms_model_results[[length(mbms_model_results) + 1]] <- lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + bmi + smoke_now + sv1 + sv2 + sv3 + sv4 + sv5"),
      data = mbms_w_surrogate_variables_analysis_ii) %>% broom::tidy(conf.int = TRUE) %>% 
      bind_cols(model = 'analysis_ii_with_svs', exposure = exposure, clock = clock, dataset='mbms') %>% 
      filter(stringr::str_detect(term, exposure))
  }
}
