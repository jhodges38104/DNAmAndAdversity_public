# fit mbms random effects models ----------------------------------

conf_level <- 0.95 # confidence level to use for visualization 



# Function to load surrogate variables
load_surrogate_vars <- function(exposure) {
  
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
  
  return(svs_for_analysis_ii)
}

# Function to create interaction terms
create_interaction <- function(mbms_w_surrogate_variables_analysis_ii_racialized, exposure) {

  # if the exposure is logical or quantitative, we can create the interaction term fairly programmatically:
  if (is.numeric(mbms_w_surrogate_variables_analysis_ii_racialized[[exposure]]) | 
      is.logical(mbms_w_surrogate_variables_analysis_ii_racialized[[exposure]])) { 
    
    # this creates the interaction-term inside the mbms dataset as if we're manually creating a 
    # model matrix — 
    # so for example if exposure is jim_crow, then we get columns for "both jim_crow and black", "both jim_crow and white"
    mbms_w_surrogate_variables_analysis_ii_racialized %<>% mutate(
      "raceblack_non_hispanic_interacted_with_{exposure}" := get(exposure) * `race_black non-hispanic`,
      "racewhite_non_hispanic_interacted_with_{exposure}" := get(exposure) * `race_white non-hispanic`)
    
    # save the names of the variables we just created
    interacted_exposure_names <- c(
      glue::glue("raceblack_non_hispanic_interacted_with_{exposure}"),
      glue::glue("racewhite_non_hispanic_interacted_with_{exposure}"))
    
    # if the exposure is categorical, we'll need to build the interaction term more manually:
  } else {
    
    # get the levels (categories) of the exposure 
    exposure_categories <- na.omit(unique(mbms_w_surrogate_variables_analysis_ii_racialized[[exposure]]))
    
    # since parents education and education have been collapsed to binary (categorical) variables, 
    # only use the lower of the two levels — 
    # and for the EOD measure, the chosen reference category is 1-2
    if (exposure %in% c('parents_educ', 'educ', 'eod_racial_3_levels')) {
      exposure_categories <- setdiff(exposure_categories, c("parents_4yr+_college_degree", "4yr+_college_degree", "1_2"))
    }
    
    # all combinations of the exposure levels and the racialized groups
    racialized_exposures <- 
      expand.grid(
        race = c("race_black non-hispanic", "race_white non-hispanic"),
        exposure = exposure_categories)
    
    # create the new column names for the interaction variables
    racialized_exposures$interaction_names <-
      replace_plus_with_p(paste0( # replace things like 3+ with 3p since regression doesn't like +s that don't mean + 
        racialized_exposures$race,
        "_interacted_with_",
        exposure, "_", 
        racialized_exposures$exposure
      ))
    
    # turn each of the levels of our categorical exposure into their own columns
    mbms_w_surrogate_variables_analysis_ii_racialized %<>% categorical_to_dummies(variable = {{ exposure }}, drop_categorical = FALSE)
    
    # for each combination of the race * exposure, create that column
    for (row_i in 1:nrow(racialized_exposures)) {
      mbms_w_surrogate_variables_analysis_ii_racialized[
        racialized_exposures$interaction_names[row_i]] <- 
        mbms_w_surrogate_variables_analysis_ii_racialized[,racialized_exposures$race[row_i]] * # race variable from mbms
        mbms_w_surrogate_variables_analysis_ii_racialized[,paste0(exposure, "_", racialized_exposures$exposure[row_i])] # exposure level variable for mbms
    }
    
    # save the column names of the interaction variables we just created
    interacted_exposure_names <- racialized_exposures$interaction_names
  }
  
  return(mbms_w_surrogate_variables_analysis_ii_racialized)
}

# Function to impute dataset
impute_dataset <- function(mbms_w_surrogate_variables_analysis_ii_racialized, 
                           interacted_exposure_names) {
  
  mbms_w_surrogate_variables_analysis_ii_racialized %>% 
    select(
      clock_estimate,
      idno,
      clock,
      age,
      race,
      !! interacted_exposure_names,
      bmi, 
      smoke_now,
      sex_gender,
      !! cell_type_variables_chr,
      !! paste0("sv", 1:5)) %>% 
    replace_dash_and_space_colnames() %$% # need to remove dashes and spaces before doing 
    # regression because lmer did not like variables with dashes or spaces
    mice(data = ., m = 40, seed = 1234, printFlag = FALSE)
}

# Function to create regression model
create_models <- function(imputed_dataset, interacted_exposure_names) {

  # create regression model 
  # 
  # the (1 | idno) term represents a random intercept by participant (idno = "id number", a factor).
  # in other words, this is a term that gets added to the predictions for a given participants' 
  # clock estimates for all the clocks and represents an age acceleration term common to all the clocks
  # not explained by the other covariates, the race-interacted exposure, cell type proportion variables,
  # or surrogate variables.
  # 
  # the (0 + age | clock) represents a random slope on age by clock. this accounts for the fact
  # that the slope on the correlation between each epigenetic clock and age may be different. 
  # we don't need an additional intercept because all of the variables are standardized already
  # so there shouldn't be any need to shift the mean predictions (as they're already 0 due to 
  # standardization). 
  #
  # these models (random effects / lmer models) are how we produce our "pooled" estimates — 
  # what they're doing is treating the multiple observations per participant from each of the 
  # different clocks as observations across which we can estimate relationships between
  # accelerated aging and exposures considering multiple clocks at once. 
  # 
  model <- with(data = imputed_dataset, lmer(
    formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5")))
  
  
  # fit a model for only first generation clocks
  model_1st_gen <- with(
    data = imputed_dataset %>% filter(clock %in% gen_1_clocks),
    lmer(
      formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5")))
  
  # fit a model for only first generation age-estimator clocks
  model_1st_gen_age_estimators <- with(
    data = imputed_dataset %>% filter(clock %in% gen_1_clocks_shorter),
    lmer(
      formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5")))
  
  # fit a model for only 2nd generation clocks
  model_2nd_gen <- with(
    data = imputed_dataset %>% filter(clock %in% gen_2_clocks), 
    lmer(formula = paste0("clock_estimate ~ 
      (1 | idno) + 
      (0 + age | clock) + 
      race + ",
      paste0(paste0("`", replace_dashes_and_spaces(interacted_exposure_names), "`"), collapse = " + "),
      " + bmi + 
      smoke_now + 
      sex_gender + 
      Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + sv1 + sv2 + sv3 + sv4 + sv5")))
  
  return(list(model, model_1st_gen, model_1st_gen_age_estimators, model_2nd_gen))
}

# Function to extract and store effect estimates
extract_effect_estimates <- function(pooled_model,
                                   pooled_model_1st_gen_age_estimators,
                                   pooled_model_1st_gen,
                                   pooled_model_2nd_gen) {
  bind_rows(
    # overall (1st and 2nd combined) 
    broom.mixed::tidy(pooled_model, conf.int = TRUE) %>% 
      mutate(clock_generation = '1st and 2nd Pooled', clock = clock_generation),
    
    # 1st generation summary 
    broom.mixed::tidy(pooled_model_1st_gen, conf.int = TRUE) %>% 
      mutate(clock_generation = '1st Pooled (all clocks)', clock = clock_generation),
    
    # 1st gen age estimators 
    broom.mixed::tidy(pooled_model_1st_gen_age_estimators, conf.int = TRUE) %>% 
      mutate(clock_generation = '1st Pooled (age-estimators)', clock = clock_generation),
    
    # 2nd generation summary
    broom.mixed::tidy(pooled_model_2nd_gen, conf.int = TRUE) %>% 
      mutate(clock_generation = '2nd Pooled', clock = clock_generation),
  )
}



# list to store mbms random effects model results in 
mbms_re_model_results <- list() # list to store output 


# for each of the exposures, create models and store their output
for (exposure in c(early_life_exposures_mbms, adult_life_exposures_mbms)) {
  
  # load the surrogate variables
  svs_for_analysis_ii <- load_surrogate_vars(exposure)
  
  # merge surrogate variables into MBMS
  mbms_w_surrogate_variables_analysis_ii <- 
    mbms_long %>% left_join(svs_for_analysis_ii,
                            by = c('idno' = 'id'))
  
  # create dummy variables for racialized groups
  mbms_w_surrogate_variables_analysis_ii_racialized <- categorical_to_dummies(mbms_w_surrogate_variables_analysis_ii,
                                                                              variable = 'race', drop_categorical = FALSE)
  
  # create interaction variables
  colnames_before_creating_interaction_variables <- colnames(mbms_w_surrogate_variables_analysis_ii_racialized) # get column names before creating new interaction variables
  mbms_w_surrogate_variables_analysis_ii_racialized <- create_interaction(mbms_w_surrogate_variables_analysis_ii_racialized, exposure) # create interaction variables
  colnames_after_creating_interaction_variables <- colnames(mbms_w_surrogate_variables_analysis_ii_racialized) # get column names after creating new interaction variables
  interacted_exposure_names <- setdiff(colnames_after_creating_interaction_variables, colnames_before_creating_interaction_variables) # infer that the new columns are the interaction variables
  
  # create an imputed dataset
  imputed_dataset <- impute_dataset(mbms_w_surrogate_variables_analysis_ii_racialized, interacted_exposure_names)
  
  # fit models 
  c(model, model_1st_gen, model_1st_gen_age_estimators, model_2nd_gen) %<-% create_models(imputed_dataset, interacted_exposure_names)
  # %<-% is the "multiple assignment operator" from the {zeallot} package 
  # 
  # the above could alternatively could be written: 
  # list_of_models <- create_models(imputed_dataset, interacted_exposure_names)
  # model <- list_of_models$model
  # model_1st_gen <- list_of_models$model_1st_gen
  # model_1st_gen_age_estimators <- list_of_models$model_1st_gen_age_estimators
  # model_2nd_gen <- list_of_models$model_2nd_gen
  
  # pool the imputation models' estimates — 
  # this is pooling across the 40 imputations, not across the clocks
  pooled_model <- pool(model) 
  pooled_model_1st_gen <- pool(model_1st_gen) 
  pooled_model_1st_gen_age_estimators <- pool(model_1st_gen_age_estimators) 
  pooled_model_2nd_gen <- pool(model_2nd_gen)
  
  # use broom.mixed to pull out model coefficients and confidence intervals
  effect_estimates <- extract_effect_estimates(pooled_model, pooled_model_1st_gen, pooled_model_1st_gen_age_estimators, pooled_model_2nd_gen)
    
  # store the results 
  mbms_re_model_results[[length(mbms_re_model_results)+1]] <- effect_estimates %>% 
    filter(stringr::str_detect(term, exposure))
}

# rowbind the results together 
mbms_re_model_results <- bind_rows(mbms_re_model_results)

# separate the exposure term into the race part and the exposure term
mbms_re_model_results$term %<>% stringr::str_remove_all("`") 
mbms_re_model_results %<>% separate(term, sep = "_interacted_with_", into = c('race', 'term'))

# use the exposure_variables_namer to add the human readable "term_str" 
mbms_re_model_results %<>% mutate(term_str = setNames(nm = exposure_variables_named_chr, names(exposure_variables_named_chr))[term])

# remove the "race" prefix from the entries in the race column
mbms_re_model_results$race %<>% stringr::str_remove_all("race_|race")
mbms_re_model_results$race %<>% tools::toTitleCase() # convert to Title Case

# rename the race terms in a standard fashion
mbms_re_model_results$race <- case_when(
  mbms_re_model_results$race == "White_non_hispanic" ~ "White non-Hispanic",
  mbms_re_model_results$race == "Black_non_hispanic" ~ "Black non-Hispanic",
  mbms_re_model_results$race == "_black_non_hispanic" ~ "Black non-Hispanic",
  mbms_re_model_results$race == "_white_non_hispanic" ~ "White non-Hispanic")




