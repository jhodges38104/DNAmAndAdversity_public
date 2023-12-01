# create coef plot fn -----------------------------------------------------


create_coef_plot <- function(
    variables,
    analysis_type,
    mbms_or_mesa) {
  
  analysis_ii_with_svs |> 
    filter(stringr::str_detect(term, stringr::str_c(variables, collapse='|')),
           dataset == mbms_or_mesa) |> 
    ggplot(
      mapping = aes(
        y = forcats::fct_rev(clock),
        x = estimate,
        xmin = conf.low,
        xmax = conf.high,
        color = ifelse(stringr::str_detect(clock, "Pooled"), "Pooled", ifelse(estimate >= 0, 'Age Acceleration', 'Age Deceleration')),
        shape = ifelse(stringr::str_detect(clock, "Pooled"), "Pooled", ifelse(estimate >= 0, 'Age Acceleration', 'Age Deceleration')),
      )
    ) +
    geom_vline(linetype = 'dashed', xintercept = 0) + 
    geom_pointrange(position = position_dodge(width=0.75), size = .25) + 
    guides(shape = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) + 
    labs(shape = 'Effect', color = 'Effect', y = 'Epigenetic Clock', x = 'Standardized Effect Estimate') + 
    scale_color_manual(values = c('Age Acceleration' = '#ff7675', "Age Deceleration" = '#74b9ff', 'Pooled' = 'black')) +
    facet_grid(tools::toTitleCase(race)~exposure_name, labeller = label_wrap_gen(width=25)) + 
    theme_bw() + 
    theme(legend.position = 'bottom')
}


# mbms early life exposures -----------------------------------------------

create_coef_plot(
  variables = early_life_exposures_mbms,
  mbms_or_mesa = 'mbms'
) 

ggsave(
  filename = here(analysis_dir, 'figures/', 'mbms_early_life_analysis_ii_with_svs.png'),
  width = 18, height = 8,
  scale = .7
)

analysis_ii_with_svs %<>% mutate(std.error = abs(estimate - conf.low)/qnorm(.975))

multiple_comparisons_n <-
  analysis_ii_with_svs %>% filter(dataset == 'mbms',
                                  stringr::str_detect(
                                    term,
                                    stringr::str_c(early_life_exposures_mbms, collapse = '|')
                                  )) %>%
  nrow()


1 - .05 / multiple_comparisons_n

analysis_ii_with_svs %>% filter(dataset == 'mbms',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(early_life_exposures_mbms, collapse = '|')
                                )) %>% 
  mutate(
    conf.low.adj = estimate - qnorm(1 - .05 / multiple_comparisons_n)*std.error,
    conf.high.adj = estimate + qnorm(1 - .05 / multiple_comparisons_n)*std.error) %>% 
  mutate(significant = sign(conf.low.adj) == sign(conf.high.adj)) %>% 
  filter(significant)

analysis_ii_with_svs_mbms_early <- analysis_ii_with_svs %>% filter(dataset == 'mbms',
                           stringr::str_detect(
                             term,
                             stringr::str_c(early_life_exposures_mbms, collapse = '|')
                           ))

any(p.adjust(analysis_ii_with_svs_mbms_early$p.value, method = 'BH') < 0.05)
any(p.adjust(analysis_ii_with_svs_mbms_early$p.value, method = 'bonferroni') < 0.05)

# mesa early life exposures -----------------------------------------------

create_coef_plot(
  variables = early_life_exposures_mesa,
  mbms_or_mesa = 'mesa'
) 

ggsave(
  filename = here(analysis_dir, 'figures/', 'mesa_early_life_analysis_ii_with_svs.png'),
  width = 18, height = 10,
  scale = .7
)


multiple_comparisons_n <-
  analysis_ii_with_svs %>% filter(dataset == 'mesa',
                                  stringr::str_detect(
                                    term,
                                    stringr::str_c(early_life_exposures_mesa, collapse = '|')
                                  )) %>%
  nrow()


1 - .05 / multiple_comparisons_n

analysis_ii_with_svs %>% filter(dataset == 'mesa',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(early_life_exposures_mesa, collapse = '|')
                                )) %>% 
  mutate(
    conf.low.adj = estimate - qnorm(1 - .05 / multiple_comparisons_n)*std.error,
    conf.high.adj = estimate + qnorm(1 - .05 / multiple_comparisons_n)*std.error) %>% 
  mutate(significant = sign(conf.low.adj) == sign(conf.high.adj)) %>% 
  filter(significant)


analysis_ii_with_svs_mesa_early <- analysis_ii_with_svs %>% filter(dataset == 'mesa',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(early_life_exposures_mesa, collapse = '|')
                                ))

any(p.adjust(analysis_ii_with_svs_mesa_early$p.value, method = 'BH') < 0.05)
any(p.adjust(analysis_ii_with_svs_mesa_early$p.value, method = 'bonferroni') < 0.05)


# mbms adult life exposures -----------------------------------------------

create_coef_plot(
  variables = adult_life_exposures_mbms[1:5],
  mbms_or_mesa = 'mbms'
)

ggsave(
  filename = here(analysis_dir, 'figures/', 'mbms_adult_life_analysis_ii_with_svs_set1.png'),
  width = 18, height = 8,
  scale = .85
)

create_coef_plot(
  variables = adult_life_exposures_mbms[6:10],
  mbms_or_mesa = 'mbms'
)

ggsave(
  filename = here(analysis_dir, 'figures/', 'mbms_adult_life_analysis_ii_with_svs_set2.png'),
  width = 18, height = 8,
  scale = .85
)



multiple_comparisons_n <-
  analysis_ii_with_svs %>% filter(dataset == 'mbms',
                                  stringr::str_detect(
                                    term,
                                    stringr::str_c(adult_life_exposures_mbms, collapse = '|')
                                  )) %>%
  nrow()


1 - .05 / multiple_comparisons_n

analysis_ii_with_svs %>% filter(dataset == 'mbms',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(adult_life_exposures_mbms, collapse = '|')
                                )) %>% 
  mutate(
    conf.low.adj = estimate - qnorm(1 - .05 / multiple_comparisons_n)*std.error,
    conf.high.adj = estimate + qnorm(1 - .05 / multiple_comparisons_n)*std.error) %>% 
  mutate(significant = sign(conf.low.adj) == sign(conf.high.adj)) %>% 
  filter(significant)

analysis_ii_with_svs_mbms_adult <- analysis_ii_with_svs %>% filter(dataset == 'mbms',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(adult_life_exposures_mbms, collapse = '|')
                                )) 

any(p.adjust(analysis_ii_with_svs_mbms_adult$p.value, method = 'BH') < 0.05)
any(p.adjust(analysis_ii_with_svs_mbms_adult$p.value, method = 'bonferroni') < 0.05)


# mesa adult life exposures -----------------------------------------------

create_coef_plot(
  variables = adult_life_exposures_mesa[1:5],
  mbms_or_mesa = 'mesa'
) + 
  guides(shape = guide_none()) 

ggsave(
  filename = here(analysis_dir, 'figures/', 'mesa_adult_life_analysis_ii_with_svs_set1.png'),
  width = 18, height = 8,
  scale = .85
)

create_coef_plot(
  variables = adult_life_exposures_mesa[6:10],
  mbms_or_mesa = 'mesa'
) 

ggsave(
  filename = here(analysis_dir, 'figures/', 'mesa_adult_life_analysis_ii_with_svs_set2.png'),
  width = 18, height = 8,
  scale = .85
)

# check for any significant results within MESA:
# Bonferroni correction approach: 
multiple_comparisons_n <-
  analysis_ii_with_svs %>% filter(dataset == 'mesa',
                                  stringr::str_detect(
                                    term,
                                    stringr::str_c(adult_life_exposures_mesa, collapse = '|')
                                  )) %>%
  nrow()


1 - .05 / multiple_comparisons_n

analysis_ii_with_svs %>% filter(dataset == 'mesa',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(adult_life_exposures_mesa, collapse = '|')
                                )) %>% 
  mutate(
    conf.low.adj = estimate - qnorm(1 - .05 / multiple_comparisons_n)*std.error,
    conf.high.adj = estimate + qnorm(1 - .05 / multiple_comparisons_n)*std.error) %>% 
  mutate(significant = sign(conf.low.adj) == sign(conf.high.adj)) %>% 
  filter(significant)

analysis_ii_with_svs_mesa_adult <- analysis_ii_with_svs %>% filter(dataset == 'mesa',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(adult_life_exposures_mesa, collapse = '|')
                                )) 

any(p.adjust(analysis_ii_with_svs_mesa_adult$p.value, method='bonferroni') < 0.05, na.rm=TRUE)
any(p.adjust(analysis_ii_with_svs_mesa_adult$p.value, method='BH') < 0.05, na.rm=TRUE)



# check for any significant results within MBMS: 
# Bonferroni correction approach: 
multiple_comparisons_n <-
  analysis_ii_with_svs %>% filter(dataset == 'mbms',
                                  stringr::str_detect(
                                    term,
                                    stringr::str_c(adult_life_exposures_mesa, collapse = '|')
                                  )) %>%
  nrow()


1 - .05 / multiple_comparisons_n

analysis_ii_with_svs %>% filter(dataset == 'mbms',
                                stringr::str_detect(
                                  term,
                                  stringr::str_c(adult_life_exposures_mesa, collapse = '|')
                                )) %>% 
  mutate(
    conf.low.adj = estimate - qnorm(1 - .05 / multiple_comparisons_n)*std.error,
    conf.high.adj = estimate + qnorm(1 - .05 / multiple_comparisons_n)*std.error) %>% 
  mutate(significant = sign(conf.low.adj) == sign(conf.high.adj)) %>% 
  filter(significant)

# false discovery rate approach: 
analysis_ii_with_svs_mbms <- analysis_ii_with_svs %>% filter(dataset == 'mbms')
analysis_ii_with_svs_mesa <- analysis_ii_with_svs %>% filter(dataset == 'mesa')

fdr_significant_mbms_rows <- p.adjust(analysis_ii_with_svs_mbms$p.value, method = 'BH')
any(fdr_significant_mbms_rows < .05, na.rm = TRUE)

fdr_significant_mesa_rows <- p.adjust(analysis_ii_with_svs_mesa$p.value, method = 'BH')
any(fdr_significant_mesa_rows < .05, na.rm = TRUE)



# classify by exposure type 
analysis_ii_with_svs %<>% mutate(
  exposure_type = 
    case_when(
      stringr::str_detect(term, paste0(c(early_life_exposures_mbms, early_life_exposures_mesa), collapse="|")) ~ "early_life",
      stringr::str_detect(term, paste0(c(adult_life_exposures_mbms, adult_life_exposures_mesa), collapse="|")) ~ "adult_life"
    ))


# how many exposures had age-accelerating effects and were significant (before multiple comparisons) 
# by their classification as early life or adult life.
analysis_ii_with_svs %>% 
  filter(p.value < 0.05) %>%
  filter(clock_generation %in% c("1st Pooled (all clocks)", "1st Pooled (age-estimators)", "2nd Pooled")) %>% 
  filter(estimate > 0) %>% 
  group_by(exposure_type) %>% 
  count()



analysis_ii_with_svs %>% 
  filter(clock_generation == '1st and 2nd Pooled') %>% 
  filter(
    term %in% c(
      'jim_crowTRUE',
      'parents_educparents_lt_4yr_college_degree',
      'rev_ICErace',
      'occupationunemployed',
      'neg_log_poverty_ratio',
      'rev_ICEinc')) %>% 
  select(
    race, term, dataset, estimate, conf.low, conf.high, clock_generation, exposure_name) %>% 
  arrange(term, dataset, race) %>% 
  write.csv(here(analysis_dir, "figures/1st_and_2nd_pooled_estimates.csv"), row.names=FALSE)

