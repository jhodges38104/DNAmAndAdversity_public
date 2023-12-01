
#' Create Imputed Datasets
#' 
#' @import mice
create_imputations <- function() {

  # number of imputations to create
  N_mice <- 40

  # ––––––––––––– mbms –––––––––––––––––
  mbms <- make_imputation_ready_mbms()

  # we don't want to impute certain critical information, like the id numbers
  # amd the epigenetics + cell type data (because it has high proportion
  # missingness)
  whereMat <- is.na(mbms) %>% 
    as.matrix(nrow = nrow(mbms), ncol = ncol(mbms)) %>% 
    as.data.frame() %>% 
    mutate(idno = FALSE, epi_final_sample = FALSE) %>% 
    mutate_at(.vars = epigenetic_clock_variables, ~ FALSE) %>% 
    mutate_at(.vars = cell_type_variables, ~ FALSE)
    
  # run mice on all of mbms
  mbms_mice <-
    mice(
      mbms,
      m = N_mice,
      # ICErace is collinear with the % white and % black
      predictorMatrix = quickpred(mbms, mincor = 0, exclude = c('idno', 'ICErace', 'epi_final_sample', epigenetic_clock_variables_chr, cell_type_variables_chr)),
      seed = 1,
      where = whereMat
    )
  
  # save mice
  saveRDS(mbms_mice, file.path(
      system.file(
        'imputations/',
        package='DNAm'),
      'mbms_mice.rds'))
  
  # save mice
  mbms_complete <- complete(mbms_mice, 'all')
  
  # save completed mice object
  saveRDS(mbms_complete, file.path(
    system.file(
      'imputations/',
      package='DNAm'),
    'mbms_mice_completed.rds'))
  
  
  
  

  # ––––––––––––– mesa –––––––––––––––––
  mesa <- make_imputation_ready_mesa()
  
  # construct where to specify not to use idno 
  whereMat <- mesa %>% is.na() %>% 
    as.matrix(nrow = nrow(mesa), ncol = ncol(mesa)) %>% 
    as.data.frame() %>% 
    mutate(idno = FALSE) %>% 
    mutate_at(.vars = epigenetic_clock_variables, ~ FALSE) %>% 
    mutate_at(.vars = cell_type_variables, ~ FALSE)
  

  # run mice for mesa
  mesa_mice <- mice(
    mesa,
    m = N_mice,
    seed = 1,
    predictorMatrix = quickpred(mesa, mincor = 0, exclude = c('idno')),
    where = whereMat
  )
  
  # save data
  saveRDS(mesa_mice, file.path(
      system.file(
        'imputations/',
        package='DNAm'),
      'mesa_mice.rds'))

  # complete mesa mice object
  mesa_complete <- complete(mesa_mice, 'all')
  
  # save data
  saveRDS(mesa_complete, file.path(
      system.file(
        'imputations/',
        package='DNAm'),
      'mesa_mice_complete.rds'))
}




#' Make Imputation Ready MBMS 
#' 
make_imputation_ready_mbms <- function() {
  
  mbms <- make_regression_ready_mbms()
  
  mbms$sex_gender <- factor(mbms$male, labels = c(`0` = 'woman', `1` = 'man'))
  mbms$race <- factor(mbms$black, labels = c(`0` = 'white non-hispanic', `1` = 'black non-hispanic'))

  # convert categorical variables to leveled factors
  mbms %<>% dummies_to_categorical(
    varname = educ,
    cols = c('college_degree', 'no_hs_degree', 'hs_no_college')
  )
  mbms$educ %<>% factor() %>% relevel('college_degree')
  
  mbms %<>% dummies_to_categorical(
    varname = parents_educ,
    cols = c(
      'parents_college_degree',
      'parents_no_hs',
      'parents_hs_no_college'
    )
  )
  mbms$parents_educ %<>% factor() %>% relevel('parents_college_degree')
  
  mbms %<>% dummies_to_categorical(
    varname = housing,
    cols = c('home_owner_free_and_clear', 'paying_mortgage', 'renter')
  )
  mbms$housing %<>% factor() %>% relevel('home_owner_free_and_clear')
  
  mbms %<>% dummies_to_categorical(
    varname = occupation,
    cols = c('supervisory_employee', 'nonsupervisory_employee', 'unemployed')
  )
  mbms$occupation %<>% factor() %>% relevel('supervisory_employee')
  
  mbms %<>% dummies_to_categorical(
    varname = smoke_now,
    cols = c(
      'smoke_last_8hrs',
      'smoke_not_in_8hrs',
      'ex_smoker',
      'never_smoker'
    )
  )
  mbms$smoke_now %<>% factor() %>% relevel('never_smoker')
  
  mbms %<>% dummies_to_categorical(
    varname = relationship,
    cols = c(
      'married',
      'divorced_separated_widowed',
      'living_w_partner',
      'serious_relationship',
      'single'
    )
  )
  mbms %<>% dummies_to_categorical(
    varname = sleep,
    cols = c(
      'sleep_under_5',
      'sleep_5_to_6',
      'sleep_over_7'
    )
  )
  
  mbms %<>% dummies_to_categorical(
    varname = birth_region,
    cols = c(
      'born_south',
      'born_midwest',
      'born_west',
      'born_northeast'))
  
  mbms %<>% mutate(
    eod_racial_3_levels = case_when(
      eod_racial_small_scale == 0 ~ "0",
      eod_racial_small_scale <= 2 ~ "1-2",
      eod_racial_small_scale >= 3 ~ "3+"
    )
  )
  mbms$eod_racial_3_levels %<>% factor() %>% relevel('1-2')
  
  mbms %<>% mutate(in_poverty = poverty_ratio <= 1)
  
  # since we decided we wanted our exposures to be adverse exposures, we
  # intend to show results for the "reversed ICE" measures where 1 = disadvantaged, 
  # and -1 = advantaged
  mbms$rev_birth_ice_race <- -1 * mbms$birth_ice_race
  mbms$rev_ICEraceinc <- -1 * mbms$ICEraceinc
  mbms$rev_ICErace <- -1 * mbms$ICErace
  mbms$rev_ICEinc <- -1 * mbms$ICEinc
  mbms$rev_ICEown <- -1 * mbms$ICEown
  
  # we also want to reverse liberalism for the same reason, and call it conservatism
  mbms$birth_state_conservatism <- -1 * mbms$birth_state_liberalism
   
  # make sure to use factors with mice -- not characters
  mbms %<>% mutate_if(is.character, as.factor)
  
  # set 0s in epi final sample
  mbms$epi_final_sample[is.na(mbms$epi_final_sample)] <- 0
  
  # make sure the last step is to ensure that mbms is not grouped 
  mbms %<>% ungroup()
  
  return(mbms)
}

#' Make Imputation Ready MESA
#' 
make_imputation_ready_mesa <- function() {
  
  mesa <- make_regression_ready_mesa()
  
  # convert categorical variables to leveled factors
  
  mesa$sex_gender <- factor(mesa$male, labels = c(`0` = 'woman', `1` = 'man'))
  
  mesa %<>% dummies_to_categorical(
    varname = race,
    cols = c('black', 'white', 'hispanic')
  )
  mesa$race %<>% factor(
    levels = c('black', 'white', 'hispanic'),
    labels = c('Black non-Hispanic', 'White non-Hispanic', 'Hispanic'))
  
  
  mesa %<>% dummies_to_categorical(
    varname = educ,
    cols = c('college_degree', 'no_hs_degree', 'hs_no_college')
  )
  mesa$educ %<>% factor() %>% relevel('college_degree')
  
  mesa %<>% dummies_to_categorical(
    varname = parents_educ,
    cols = c(
      'parents_college_degree',
      'parents_no_hs',
      'parents_hs_no_college'
    )
  )
  mesa$parents_educ %<>% factor() %>% relevel('parents_college_degree')
  
  mesa %<>% dummies_to_categorical(
    varname = housing,
    cols = c('home_owner_free_and_clear', 'paying_mortgage', 'renter')
  )
  mesa$housing %<>% factor() %>% relevel('home_owner_free_and_clear')
  
  mesa %<>% dummies_to_categorical(
    varname = occupation,
    cols = c('employed', 'unemployed')
  )
  mesa$occupation %<>% factor() %>% relevel('employed')
  
  mesa %<>% dummies_to_categorical(
    varname = smoke_now,
    cols = c(
      'current_smoker',
      'smoker_former_quit_over_1yr',
      'smoker_former_quit_under_1yr',
      'never_smoker'
    )
  )
  mesa$smoke_now %<>% factor() %>% relevel('never_smoker')
  
  mesa %<>% dummies_to_categorical(
    varname = relationship,
    cols = c(
      'married_or_living_w_partner',
      'divorced',
      'never_married',
      'separated',
      'widowed'
    )
  )

  mesa %<>% dummies_to_categorical(
    varname = birth_region,
    cols = c(
      'born_south',
      'born_midwest',
      'born_west',
      'born_northeast'))
  
  mesa %<>% mutate(
    mds_racial_3_levels = case_when(
      mds_racial_small_scale == 0 ~ "0",
      mds_racial_small_scale <= 2 ~ "1-2",
      mds_racial_small_scale >= 3 ~ "3+"
    )
  )
  mesa$mds_racial_3_levels %<>% factor() %>% relevel('1-2')
  
  mesa %<>% mutate(in_poverty = poverty_ratio <= 1)
  
  # since we decided we wanted our exposures to be adverse exposures, we
  # intend to show results for the "reversed ICE" measures where 1 = disadvantaged, 
  # and -1 = advantaged
  mesa$rev_ICEraceinc <- -1 * mesa$ICEraceinc
  mesa$rev_ICErace <- -1 * mesa$ICErace
  mesa$rev_ICEinc <- -1 * mesa$ICEinc
  mesa$rev_ICEown <- -1 * mesa$ICEown
  
  # we also want to reverse liberalism for the same reason, and call it conservatism
  mesa$birth_state_conservatism <- -1 * mesa$birth_state_liberalism
  
  # need to make sure to use factors for mice
  mesa %<>% mutate_if(is.character, as.factor)
  
  # make sure the last step is that mesa is ungrouped
  mesa %<>% ungroup()
  
  return(mesa)
}
  