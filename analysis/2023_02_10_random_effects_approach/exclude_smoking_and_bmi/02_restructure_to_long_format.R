
# restructure mbms and mesa to long format --------------------------------

mbms %>% 
  # select just the variables we need for now 
  select(idno, age, race, sex_gender, bmi, smoke_now, !! c(early_life_exposures_mbms, adult_life_exposures_mbms), !! cell_type_variables_chr, !! epigenetic_clock_variables_chr) %>% 
  # for simplicity, let's start with a race and gender specific group and ignore adjustment for bmi, smoking
  # filter(race == 'black non-hispanic', sex_gender == 'man') %>%
  # convert dw_transformer objects to numeric 
  mutate(across(where(~ class(.)[[1]] == 'dw_transformer'), as.numeric)) %>% 
  # pivot the epigenetic clocks into a long format
  tidyr::pivot_longer(
    cols = !! epigenetic_clock_variables_chr,
    names_to = 'clock',
    values_to = 'clock_estimate') %>% 
  # convert the IDs and clock names into integer vectors stan can use to index across random effects
  mutate(idno = as.factor(idno)) %>% 
  # add generation to the data 
  mutate(clock_generation = ifelse(
    clock %in% c(
      'age.horvath', # 'Horvath (1st)',
      'age.hannum', # 'Hannum (1st)',
      'epiToc.age', # 'epiToc (1st)',
      'zhang.age', # 'Zhang Age (1st)',
      'miage.clock', # 'MiAge (1st)',
      'dnamtl' # 'DNAmTL (1st)'
    ),
    '1st',
    '2nd'
  )) -> 
  mbms_long # store data in a long format

mesa_usborn %>% 
  # select just the variables we need for now 
  select(idno, age, race, sex_gender, bmi, smoke_now, !! c(early_life_exposures_mesa, adult_life_exposures_mesa), !! cell_type_variables_chr, !! epigenetic_clock_variables_chr) %>% 
  # for simplicity, let's start with a race and gender specific group and ignore adjustment for bmi, smoking
  # filter(race == 'black non-hispanic', sex_gender == 'man') %>%
  # convert dw_transformer objects to numeric 
  mutate(across(where(~ class(.)[[1]] == 'dw_transformer'), as.numeric)) %>% 
  # pivot the epigenetic clocks into a long format
  tidyr::pivot_longer(
    cols = !! epigenetic_clock_variables_chr,
    names_to = 'clock',
    values_to = 'clock_estimate') %>% 
  # convert the IDs and clock names into integer vectors stan can use to index across random effects
  mutate(idno = as.factor(idno)) %>% 
  # add generation to the data 
  mutate(clock_generation = ifelse(
    clock %in% c(
      'age.horvath', # 'Horvath (1st)',
      'age.hannum', # 'Hannum (1st)',
      'epiToc.age', # 'epiToc (1st)',
      'zhang.age', # 'Zhang Age (1st)',
      'miage.clock', # 'MiAge (1st)',
      'dnamtl' # 'DNAmTL (1st)'
    ),
    '1st',
    '2nd'
  )) -> 
  mesa_long # store data in a long format
