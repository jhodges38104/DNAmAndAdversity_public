# Per MESA comments received on 8/23 we have decided to submit to JAMA and 
# can only have 5 images (tables/figures) in the body of the paper, so we need to 
# condense figures created in the original submission. 


# load  underlying_data for figures
analysis_ii_with_svs <- readRDS(here(analysis_dir,"figures/v1/underlying_data.rds"))

# convert exposure_name to character in order to directly change labels
analysis_ii_with_svs$exposure_name <- as.character(analysis_ii_with_svs$exposure_name) 

# change occupational class to employment status
analysis_ii_with_svs$exposure_name[analysis_ii_with_svs$exposure_name == "Occupational Class — Unemployed [ref: Employed]"] <- "Employment Status — Unemployed [ref: Employed]"

# change nitrous oxides to oxides of nitrogen
analysis_ii_with_svs$exposure_name[analysis_ii_with_svs$exposure_name == "Nitrous Oxides (Higher = More Pollution)"] <- "Oxides of Nitrogen (Higher = More Pollution)"

# convert back to factor class
analysis_ii_with_svs$exposure_name <- factor(analysis_ii_with_svs$exposure_name,
                                             levels = c('Born in a Jim Crow State', 
                                                           'Reversed ICE for Racialized Segregation at City of Birth (Black vs. White)',
                                                           'State Policy Liberalism in State of Birth' ,
                                                           'State Policy Conservatism in State of Birth',
                                                           "Parent's Highest Education: < 4-year College Degree [ref: Parents with 4-year College Degree]",
                                                           "Participant's Highest Education: < 4-year College Degree [ref: 4-year College Degree]",
                                                           "Experiences of Discrimination",
                                                           "Experiences of Discrimination (0) [ref: 1-2]",
                                                           "Experiences of Discrimination (3+) [ref: 1-2]",
                                                           "Major Discrimination Scale (0) [ref: 1-2]",
                                                           "Major Discrimination Scale (3+) [ref: 1-2]",
                                                           "-log10(household income / poverty line) in 2010 Dollars",
                                                           "Occupational Class — Non-Supervisory Employee [ref: Supervisory Employee, Owner]", 
                                                           "Employment Status — Unemployed [ref: Employed]", 
                                                           "Housing Tenure — Paying Mortgage [ref: Own Home Free and Clear]", 
                                                           "Housing Tenure — Paying Rent [ref: Own Home Free and Clear]", 
                                                           "Black Carbon (Higher = More Pollution)",
                                                           "Light Absorption Coefficient (Higher = More Pollution)",
                                                           "Pollution Proximity Index (Higher = More Pollution)",
                                                           "Oxides of Nitrogen (Higher = More Pollution)",
                                                           "Reversed Residential Census Tract ICE for Income (Low Income vs. High Income)",
                                                           "Reversed Residential Census Tract ICE for Racial Segregation (Black vs. White non-Hispanic)",
                                                           "Reversed Residential Census Tract ICE for Racialized Economic Segregation (Low Income Black vs. White non-Hispanic High Income)",
                                                           "Reversed Residential Census Tract ICE for Housing Tenure (Renters vs. Paying Mortgage or Own Free and Clear)")) 


# function to plot results 
create_coef_plot <- function(
    variables,
    analysis_type,
    mbms_or_mesa,
    strip_text_size) {
  
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
    theme(legend.position = 'bottom',
          strip.text.x = element_text(size = strip_text_size))
}


#-- Figure 1: “Standardized effect estimates and 95% confidence intervals by 
#---racialized group for early life adverse exposures (born in a Jim Crow state;
#---reversed Index of Concentration at the Extremes for racialized segregation at 
#---city of birth; state policy conservatism in state of birth; parents’ education; 
#---participants’ education) and adult adverse exposures (Experiences of Discrimination; 
#---negative log household income ratio to poverty line; occupational class): MBMS participants”

create_coef_plot(
  variables = fig_1_variables,
  mbms_or_mesa = 'mbms',
  strip_text_size = 8
) 

ggsave(
  filename = here(paste0(analysis_dir,'/figures/v1/JAMA revisions/mbms_fig1_analysis_ii_with_svs_set1.png')),
  width = 18, height = 8,
  scale = .85
)


#-- Figure 2: “Standardized effect estimates and 95% confidence intervals by racialized 
#---group for adult individual-, household-, and area-based adverse exposures (employment status; 
#---housing tenure; black carbon; pollution proximity; reversed Index of Concentration at the 
#---Extremes for income, racial segregation, racialized economic segregation, and housing tenure): 
#---MBMS participants”

create_coef_plot(
  variables = fig_2_variables,
  mbms_or_mesa = 'mbms',
  strip_text_size = 8
)

ggsave(
  filename = here(paste0(analysis_dir,'/figures/v1/JAMA revisions/mbms_fig2_analysis_ii_with_svs_set1.png')),
  width = 18, height = 8,
  scale = .85
)

#-- Figure 3: “Standardized effect estimates and 95% confidence intervals by racialized group 
#---for early life adverse exposures (born in a Jim Crow state; state policy conservatism in state of birth; 
#---parents’ education; participants’ education) and adult adverse exposures (Major Discrimination scale (racialized); 
#---negative log household income ratio to poverty line; employment status): MESA participants”

create_coef_plot(
  variables = fig_3_variables,
  mbms_or_mesa = 'mesa',
  strip_text_size = 7.5
) 

ggsave(
  filename = here(paste0(analysis_dir,'/figures/v1/JAMA revisions/mesa_fig1_analysis_ii_with_svs.png')),
  width = 18, height = 10,
  scale = .7
)


#-- Figure 4: “Standardized effect estimates and 95% confidence intervals by racialized group 
#---for adult household- and area-based adverse exposures (housing tenure; light absorption 
#---coefficient [air pollution]; oxides of nitrogen; reversed Index of Concentration at the Extremes 
#---for income, racial segregation, racialized economic segregation, and housing tenure): MESA participants”

create_coef_plot(
  variables = fig_4_variables,
  mbms_or_mesa = 'mesa',
  strip_text_size = 7
)

ggsave(
  filename =here(paste0(analysis_dir,'/figures/v1/JAMA revisions/mesa_fig2_analysis_ii_with_svs.png')),
  width = 18, height = 10,
  scale = .7
)

