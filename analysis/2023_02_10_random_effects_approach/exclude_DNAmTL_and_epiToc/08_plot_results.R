# create coef plot fn -----------------------------------------------------


create_coef_plot <- function(
    variables,
    analysis_type,
    mbms_or_mesa) {
  
  analysis_ii_with_svs |> 
    filter(stringr::str_detect(term, stringr::str_c(variables, collapse='|')),
           dataset == mbms_or_mesa) |> 
    filter(clock != '1st and 2nd Pooled') |> 
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
) + 
  ggtitle("Early Life Exposures, MBMS")

ggsave(
  filename = here('exclude_DNAmTL_and_epiToc/figures/', 'mbms_early_life_analysis_ii_with_svs.png'),
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

# mesa early life exposures -----------------------------------------------

create_coef_plot(
  variables = early_life_exposures_mesa,
  mbms_or_mesa = 'mesa'
) + 
  ggtitle("Early Life Exposures, MESA")

ggsave(
  filename = here('exclude_DNAmTL_and_epiToc/figures/', 'mesa_early_life_analysis_ii_with_svs.png'),
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

# mbms adult life exposures -----------------------------------------------

create_coef_plot(
  variables = adult_life_exposures_mbms[1:5],
  mbms_or_mesa = 'mbms'
) + 
  ggtitle("Adult Life Exposures (Set 1), MBMS")

ggsave(
  filename = here('exclude_DNAmTL_and_epiToc/figures/', 'mbms_adult_life_analysis_ii_with_svs_set1.png'),
  width = 18, height = 8,
  scale = .85
)

create_coef_plot(
  variables = adult_life_exposures_mbms[6:10],
  mbms_or_mesa = 'mbms'
) + 
  ggtitle("Adult Life Exposures (Set 2), MBMS")

ggsave(
  filename = here('exclude_DNAmTL_and_epiToc/figures/', 'mbms_adult_life_analysis_ii_with_svs_set2.png'),
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


# mesa adult life exposures -----------------------------------------------

create_coef_plot(
  variables = adult_life_exposures_mesa[1:5],
  mbms_or_mesa = 'mesa'
) + 
  ggtitle("Adult Life Exposures (Set 1), MESA") + 
  guides(shape = guide_none()) 

ggsave(
  filename = here('exclude_DNAmTL_and_epiToc/figures/', 'mesa_adult_life_analysis_ii_with_svs_set1.png'),
  width = 18, height = 8,
  scale = .85
)

create_coef_plot(
  variables = adult_life_exposures_mesa[6:10],
  mbms_or_mesa = 'mesa'
) + 
  ggtitle("Adult Life Exposures (Set 2), MESA")

ggsave(
  filename = here('exclude_DNAmTL_and_epiToc/figures/', 'mesa_adult_life_analysis_ii_with_svs_set2.png'),
  width = 18, height = 8,
  scale = .85
)


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


