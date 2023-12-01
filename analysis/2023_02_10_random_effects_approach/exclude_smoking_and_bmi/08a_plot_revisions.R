# Per MESA comments received on 8/23 we have decided to submit to JAMA and 
# can only have 5 images (tables/figures) in the body of the paper, so we need to 
# condense figures created in the original submission. 



# load  underlying_data for figures
analysis_ii_with_svs <- readRDS(here(analysis_dir,"/exclude_smoking_and_bmi/figures/JAMA revisions/underlying_data_2023-08-30.rds"))


# function to plot results 
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


# eFigure 5: Standardized effect estimates by racialized group and 95% confidence intervals for adult 
# life exposures (set 1: Experiences of Discrimination, negative log household income ratio to the poverty line, 
# occupational class, employment status, housing tenure, and black carbon air pollution): MBMS – not including 
# covariate data on BMI and smoking.

# change label of 5th exposure to “Employment status – unemployed [ref: Employed]”
create_coef_plot(
  variables = adult_life_exposures_mbms[1:5],
  mbms_or_mesa = 'mbms'
)

ggsave(
  filename = here(analysis_dir, '/exclude_smoking_and_bmi/figures/JAMA revisions/mbms_adult_life_analysis_ii_with_svs_set1.png'),
  width = 18, height = 8,
  scale = .85
)

# eFigure 8: change label of 4th exposure to “Employment status – unemployed [ref: Employed]”
create_coef_plot(
  variables = adult_life_exposures_mesa[1:5],
  mbms_or_mesa = 'mesa'
) + 
  guides(shape = guide_none()) 

ggsave(
  filename = here(analysis_dir, '/exclude_smoking_and_bmi/figures/JAMA revisions/mesa_adult_life_analysis_ii_with_svs_set1.png'),
  width = 18, height = 8,
  scale = .85
)


#	eFigure 9: change label of 1st exposure to “Oxides of Nitrogen (Higher = More Pollution)”
create_coef_plot(
  variables = adult_life_exposures_mesa[6:10],
  mbms_or_mesa = 'mesa'
) 

ggsave(
  filename = here(analysis_dir, '/exclude_smoking_and_bmi/figures/JAMA revisions/mesa_adult_life_analysis_ii_with_svs_set2.png'),
  width = 18, height = 8,
  scale = .85
)
