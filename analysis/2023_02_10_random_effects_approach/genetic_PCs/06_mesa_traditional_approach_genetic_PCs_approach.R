
mesa_model_results <- list()

genetic_PCs <- readr::read_csv(
  system.file("genetic_principle_components/mesa_genetic_pcs_Comb.csv",
              package = "DNAm"))

genetic_PCs$idno %<>% as.character()

model_comparisons <- list()

for (clock in epigenetic_clock_variables_chr) {
  for (exposure in c(early_life_exposures_mesa, adult_life_exposures_mesa)) {
    
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
    analysis_i_with_svs_model <- lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10"),
      data = mesa_w_surrogate_variables_analysis_i)
    
    
    # analysis I with genetic PCs and SVs
    svs_for_analysis_i_w_genetic_pcs <- readr::read_csv(
      system.file(
        paste0('surrogate_variables/mesa_analysis_1_svs_genetic_pcs/', lookup_for_mesa_sv_filenames[exposure], '_mesa_svs_analysis1_plus_genetic_pcs.csv'),
        package = 'DNAm'
      ),
      show_col_types = FALSE
    )
    svs_for_analysis_i_w_genetic_pcs %<>% rename(id = 1)
    svs_for_analysis_i_w_genetic_pcs$id %<>% as.character()
    
    mesa_w_genetic_pcs_and_svs <- mesa_usborn %>% left_join(genetic_PCs, by = c('idno' = 'idno')) %>% 
      left_join(svs_for_analysis_i_w_genetic_pcs, by = c('idno' = 'id'))
    
    analysis_i_with_genetic_pcs_and_svs <- lm(
      formula = paste0(clock, " ~ age + sex_gender + race / ", exposure, " + 
          Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + pc1 + pc2 + pc3 + pc4 + pc5 + sv1 + sv2 + sv3 + sv4 + sv5 + sv6 + sv7 + sv8 + sv9 + sv10"),
      data = mesa_w_genetic_pcs_and_svs)
    
    
    # model comparison 
    model_comparisons[[length(model_comparisons) + 1]] <-
      list(
        exposure = exposure,
        approach = 'without_genetic_pcs',
        clock = clock,
        r.squared = summary(analysis_i_with_svs_model)$r.squared
      )
    model_comparisons[[length(model_comparisons) + 1]] <-
      list(
        exposure = exposure,
        approach = 'with_genetic_pcs',
        clock = clock,
        r.squared = summary(analysis_i_with_genetic_pcs_and_svs)$r.squared
      )
  }
}

model_comparisons %<>% bind_rows()

model_comparisons %<>% tidyr::pivot_wider(id_cols = c(exposure, clock), names_from = approach, values_from = r.squared)
  
model_comparisons %>% 
  ggplot(aes(y = clock)) + 
  geom_point(aes(x = with_genetic_pcs), shape = 5) + 
  geom_point(aes(x = without_genetic_pcs), shape = 6) + 
  # geom_segment(aes(xmin = without_genetic_pcs, xmax = with_genetic_pcs, ymin = clock, ymax = clock)) + 
  facet_wrap(~exposure)

clock_namer <- c(
  'age.horvath' = 'Horvath (1st)',
  'age.hannum' = 'Hannum (1st)',
  'epiToc.clock' = 'epiToc (1st)',
  'zhang_clock' = 'Zhang  (1st)',
  'miage.clock' = 'MiAge (1st)',
  'dnamtl' = 'DNAmTL (1st)',
  'phenoage' = 'PhenoAge (2nd)',
  'zhang.mortality' = 'Zhang Mortality (2nd)',
  'dunedin_age' = 'DunedinPoAm (2nd)',
  'DNAmGrimAge' = 'GrimAge (2nd)')
  
  
model_comparisons %>% 
  mutate(clock = clock_namer[clock], clock = factor(clock, levels = rev(clock_namer))) %>% 
  ggplot(aes(y = clock, x = (with_genetic_pcs / without_genetic_pcs) - 1)) + 
  geom_point() + 
  scale_x_continuous(labels = scales::percent_format()) + 
  xlab("Additional % of Variance Explained Incorporating Genetic PCs") + 
  ggtitle("Additional Variance Explained by Incorporating Genetic PCs",
          "Each point represents a different exposure") + 
  theme_bw()

ggsave("genetic_PCs/additional_pct_explained.png", width = 7, height = 5)
