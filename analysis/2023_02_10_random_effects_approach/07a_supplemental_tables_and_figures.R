
# analyze age, smoking, bmi variables -------------------------------------

mbms_models_df <- mbms_models %>% bind_rows()
mesa_models_df <- mesa_models %>% bind_rows()

saveRDS(mbms_models_df, here("model_data/mbms_models_traditional.rds"))
saveRDS(mesa_models_df, here("model_data/mesa_models_traditional.rds"))

formatter <- scales::number_format(accuracy = 0.1)

# smoking in the last 8 hours 
mbms_models %>% 
  bind_rows() %>% 
  filter(term == 'smoke_nowsmoke_last_8hrs') %>% 
  select(clock, exposure, estimate, conf.low, conf.high) %>% 
  mutate(effect_estimate = str_c(formatter(estimate), " (",
                                 formatter(conf.low), ", ",
                                 formatter(conf.high), ")")) %>% 
  select(-c(estimate, conf.low, conf.high)) %>% 
  tidyr::pivot_wider(
    id_cols = clock, 
    names_from = exposure,
    values_from = c(everything(), -clock, -exposure)) -> 
  mbms_smoking_table

# smoking — current, MESA
mesa_models %>% 
  bind_rows() %>% 
  filter(term == 'smoke_nowcurrent_smoker') %>% 
  select(clock, exposure, estimate, conf.low, conf.high) %>% 
  mutate(effect_estimate = str_c(formatter(estimate), " (",
                                 formatter(conf.low), ", ",
                                 formatter(conf.high), ")")) %>% 
  select(-c(estimate, conf.low, conf.high)) %>% 
  tidyr::pivot_wider(
    id_cols = clock, 
    names_from = exposure,
    values_from = c(everything(), -clock, -exposure)) -> 
  mesa_smoking_table

# age, mbms
mbms_models %>% 
  bind_rows() %>% 
  filter(term == 'age') %>% 
  select(clock, exposure, estimate, conf.low, conf.high) %>% 
  mutate(effect_estimate = str_c(formatter(estimate), " (",
                                 formatter(conf.low), ", ",
                                 formatter(conf.high), ")")) %>% 
  select(-c(estimate, conf.low, conf.high)) %>% 
  tidyr::pivot_wider(
    id_cols = clock, 
    names_from = exposure,
    values_from = c(everything(), -clock, -exposure)) -> 
  mbms_age_table

# age, mesa
mesa_models %>% 
  bind_rows() %>% 
  filter(term == 'age') %>% 
  select(clock, exposure, estimate, conf.low, conf.high) %>% 
  mutate(effect_estimate = str_c(formatter(estimate), " (",
                                 formatter(conf.low), ", ",
                                 formatter(conf.high), ")")) %>% 
  select(-c(estimate, conf.low, conf.high)) %>% 
  tidyr::pivot_wider(
    id_cols = clock, 
    names_from = exposure,
    values_from = c(everything(), -clock, -exposure)) -> 
  mesa_age_table


# bmi, mbms
mbms_models %>% 
  bind_rows() %>% 
  filter(term == 'bmi') %>% 
  select(clock, exposure, estimate, conf.low, conf.high) %>% 
  mutate(effect_estimate = str_c(formatter(estimate), " (",
                                 formatter(conf.low), ", ",
                                 formatter(conf.high), ")")) %>% 
  select(-c(estimate, conf.low, conf.high)) %>% 
  tidyr::pivot_wider(
    id_cols = clock, 
    names_from = exposure,
    values_from = c(everything(), -clock, -exposure)) -> 
  mbms_bmi_table

# bmi, mesa
mesa_models %>% 
  bind_rows() %>% 
  filter(term == 'bmi') %>% 
  select(clock, exposure, estimate, conf.low, conf.high) %>% 
  mutate(effect_estimate = str_c(formatter(estimate), " (",
                                 formatter(conf.low), ", ",
                                 formatter(conf.high), ")")) %>% 
  select(-c(estimate, conf.low, conf.high)) %>% 
  tidyr::pivot_wider(
    id_cols = clock, 
    names_from = exposure,
    values_from = c(everything(), -clock, -exposure)) -> 
  mesa_bmi_table


exposure_namer2 <- c(
  jim_crow = "Born in a Jim Crow State", 
  rev_birth_ice_race = "Reversed ICE for Racialized Segregation at City of Birth (Black vs. White)", 
  birth_state_liberalism = "State Policy Liberalism in State of Birth", 
  birth_state_conservatism = "State Policy Conservatism in State of Birth", 
  parents_educ = "Parent's Highest Education", 
  educ = "Participant's Highest Education", 
  eod_racial_small_scale = "Experiences of Discrimination", 
  eod_racial_3_levels = "Experiences of Discrimination", 
  `mds_racial_3_levels` = "Major Discrimination Scale", 
  neg_log_poverty_ratio = "-log10(household income / poverty line) in 2010 Dollars", 
  occupation = "Occupational Class", 
  housing = "Housing Tenure — Paying Mortgage", 
  black_carbon = "Black Carbon", 
  light_absorption = "Light Absorption Coefficient", 
  pollution_proximity = "Pollution Proximity Index", 
  nitrous_oxides = "Nitrous Oxides", 
  rev_ICEinc = "Reversed Residential Census Tract ICE for Income", 
  rev_ICErace = "Reversed Residential Census Tract ICE for Racial Segregation", 
  rev_ICEraceinc = "Reversed Residential Census Tract ICE for Racialized Economic Segregation", 
  rev_ICEown = "Reversed Residential Census Tract ICE for Housing Tenure"
)


colnames(mbms_smoking_table)[2:ncol(mbms_smoking_table)] <- exposure_namer2[colnames(mbms_smoking_table)[2:ncol(mbms_smoking_table)]]
colnames(mesa_smoking_table)[2:ncol(mesa_smoking_table)] <- exposure_namer2[colnames(mesa_smoking_table)[2:ncol(mesa_smoking_table)]]
colnames(mbms_age_table)[2:ncol(mbms_age_table)] <- exposure_namer2[colnames(mbms_age_table)[2:ncol(mbms_age_table)]]
colnames(mesa_age_table)[2:ncol(mesa_age_table)] <- exposure_namer2[colnames(mesa_age_table)[2:ncol(mesa_age_table)]]
colnames(mbms_bmi_table)[2:ncol(mbms_bmi_table)] <- exposure_namer2[colnames(mbms_bmi_table)[2:ncol(mbms_bmi_table)]]
colnames(mesa_bmi_table)[2:ncol(mesa_bmi_table)] <- exposure_namer2[colnames(mesa_bmi_table)[2:ncol(mesa_bmi_table)]]

mbms_smoking_table$clock %<>% factor(
  levels = c('age.horvath', 'age.hannum', 'epiToc.clock', 'zhang_clock',
             'miage.clock', 'dnamtl', 'zhang.mortality',
             'phenoage', 'dunedin_age', 'DNAmGrimAge'),
  labels = clock_order)

mesa_smoking_table$clock %<>% factor(
  levels = c('age.horvath', 'age.hannum', 'epiToc.clock', 'zhang_clock',
             'miage.clock', 'dnamtl', 'zhang.mortality',
             'phenoage', 'dunedin_age', 'DNAmGrimAge'),
  labels = clock_order)

mbms_age_table$clock %<>% factor(
  levels = c('age.horvath', 'age.hannum', 'epiToc.clock', 'zhang_clock',
             'miage.clock', 'dnamtl', 'zhang.mortality',
             'phenoage', 'dunedin_age', 'DNAmGrimAge'),
  labels = clock_order)

mesa_age_table$clock %<>% factor(
  levels = c('age.horvath', 'age.hannum', 'epiToc.clock', 'zhang_clock',
             'miage.clock', 'dnamtl', 'zhang.mortality',
             'phenoage', 'dunedin_age', 'DNAmGrimAge'),
  labels = clock_order)

mbms_bmi_table$clock %<>% factor(
  levels = c('age.horvath', 'age.hannum', 'epiToc.clock', 'zhang_clock',
             'miage.clock', 'dnamtl', 'zhang.mortality',
             'phenoage', 'dunedin_age', 'DNAmGrimAge'),
  labels = clock_order)

mesa_bmi_table$clock %<>% factor(
  levels = c('age.horvath', 'age.hannum', 'epiToc.clock', 'zhang_clock',
             'miage.clock', 'dnamtl', 'zhang.mortality',
             'phenoage', 'dunedin_age', 'DNAmGrimAge'),
  labels = clock_order)

openxlsx::write.xlsx(
  list(
    mbms_smoking = mbms_smoking_table %>% arrange(clock),
    mesa_smoking = mesa_smoking_table %>% arrange(clock),
    mbms_age = mbms_age_table %>% arrange(clock),
    mesa_age = mesa_age_table %>% arrange(clock),
    mbms_bmi = mbms_bmi_table %>% arrange(clock),
    mesa_bmi = mesa_bmi_table %>% arrange(clock)),
  file = here("age_bmi_and_smoking_effect_tables/tables.xlsx"))


# correlation matrices ------------------------------------------------------

mbms_selected <- mbms %>% 
  select(
    age,
    sex_gender,
    smoke_now,
    race,
    !! early_life_exposures_mbms,
    !! adult_life_exposures_mbms, 
    Bcell, CD4T, CD8T, Eos, Mono, Neu, NK) %>% 
  select(where(is.numeric))

mesa_selected <- mesa_usborn %>% 
  select(
    age,
    sex_gender,
    smoke_now,
    race,
    !! early_life_exposures_mesa,
    !! adult_life_exposures_mesa, 
    Bcell, CD4T, CD8T, Eos, Mono, Neu, NK) %>% 
  select(where(is.numeric))

cor_mat_mbms <- cor(as.matrix(mbms_selected), use = 'pairwise.complete.obs')
cor_mat_mesa <- cor(as.matrix(mesa_selected), use = 'pairwise.complete.obs')


ggcorrplot2::ggcorrplot(cor_mat_mbms, method = 'circle')
ggcorrplot2::ggcorrplot(cor_mat_mesa, method = 'circle')