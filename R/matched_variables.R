
#' Load the Matched Variables
load_matched_variables <- function() { 
  yaml::read_yaml(
    system.file(
      "helper_data/matched_variables/matched_variables.yml", 
      package='DNAm'))
}

#' Load Select Matched Variables
load_select_matched_variables <- function() { 
  yaml::read_yaml(
    system.file(
      'helper_data/matched_variables/select_matched_variables.yml',
      package='DNAm'))
}

#' Variables Descriptions
#' 
#' 
variables_df <- tibble::tribble(
  ~measure,                    ~mbms,                    ~mesa,
  'age',                       'AgeYRS',                 'age5c',
  'gender',                    'gender',                 'gender1',
  'race',                      'race',                   'race1c',
  'birth_place',               'You_Born',               'bth1',
  'birth_state',               'StateOB',                'stbth1',
  'birth_region',              'birth_region',           'region',
  'education',                 'educ_r',                 'educ_reformatted',
  'mother_nativity',           'Mother',                 'mbth1',
  'father_nativity',           'Father',                 'fbth1',
  'parents_highest_edu',       'parents_highest_edu',    'parents_highest_edu', 
  'marital_status',            'M_Status',               'marital5', # M_Status needs cleaning
  'time_in_US',                'duration_us',            'yrsus1',
  'height',                    'height_cm',              'htcm5',
  'weight',                    'weight_kg',              'wtkg5_reformatted',
  'hip',                       'intHip',                 'hipcm5',
  'waist',                     'intWaist',               'waistcm5',
  'bmi',                       'bmi',                    'bmi5c',
  'tibia_height_ratio',        'tibia_ratio',             NA,
  'glucose',                   'intGlucose',             'glucose5',
  'ldl_cholesterol',           'intLDLChol',             'ldl5',
  'hdl_cholesterol',           'intHDLChol',             'hdl5',
  'diastolic_bp',              'dbp',                    'dbp5c',
  'systolic_bp',               'sbp',                    'sbp5c',
  'diabetes_med',              'med_diab_new',           'diabins5',
  'hypertension_med',          'med_bp_new',             'htnmed5c',
  'diabetes',                  'Diabetes',               'diabet1',
  'hypertension',              'intHypertension',        'htn5c',
  'triglycerides',             'intTrig',                'trig5',
  'framingham_cvd',            'frisk',                  'frci085c',
  'metabolic_syndrome',        'metab_d',                'metsyn5c',
  'asthma',                    'Asthma',                 'asthma5',
  'family_income',             'inc1',                   'income5',
  'employment',                'occ_class_reformatted',  'curjob_reformatted', # extract retired / part time 
  'poverty_ratio',             'pov_us',                 'poverty_ratio',
  'adult_deprivation',         'adult_dep_sum',           NA,
  'child_deprivation',         'childhood_dep_sum',       NA,
  'poverty_below_2x',          'pov_below_2x',           'pov_below_2x',
  'home_ownership',            'House_owner',            'hometyp1',
  'exercise',                  'METmw',                  'exercm5c',
  'smoking',                   'Smoke_now_8hrs',         'smkstat5', # need to make smoking variables comparable
  'sleep_hours',               'Amount_of_sleep',        'slpwkhr4', # categorical vs. quantitative
  'food_last_8hrs',            'intLastFood',             NA,
  'cig_last_8hrs',             'intLastCig',              NA,
  'alc_last_8hrs',             'intLastAlc',              NA,
  'do_something',              'Unfr',                   'uf1resp1',
  'talk_to_others',            'Unfr2',                  'uf2resp1',
  'social_desirability',       'soc_des',                 NA,
  'hostility',                 'hostility',               NA,
  'eds_racial',                'eds_ut_rd',              'eds_s_rd',
  'eod_racial',                'eod_s',                   NA,
  'mds_racial',                 NA,                      'mds_rd',
  'lifetime_discrimination',   'Entire_Lifetime_Stress', 'discrl1c',
  'ct_pct_white',              'percWNH',                'race_whiteNH',
  'ct_pct_black',              'percBNH',                'race_blackNH',
  'ct_pct_hispanic',            NA,                      'race_hisp',
  'ct_pct_asian',               NA,                      'race_asianNH',
  'ct_pov_allage',              NA,                      'pov_allage',
  'ct_pov',                    'percpov',                'pov',    
  'ct_pov_cat',                'ct_pov_cat',             'ct_pov_cat',
  'ICEinc',                    'ICEinc',                 'ICEinc',
  'ICErace',                   'ICErace',                'ICErace',
  'ICEraceinc',                'ICEraceinc',             'ICEraceinc',
  'ICEown',                     NA,                      'ICEown',
  'jim_crow',                  'JC',                     'JC',
  'black_carbon',              'year_ave',                NA,
  'pollution_proximity',       'ppi5',                    NA,
  'nitrous_oxides',             NA,                      'NOx_lik_0_1_yr_exam5_wght',
  'light_absorption',           NA,                      'LAC_lik_0_1_yr_exam5_wsp',
  'iat_mt',                    'score_MT',                NA,
  'iat_bw',                    'score_BW',                NA
  )

#' Rename MBMS Variables
rename_mbms_variables <- function(mbms) {

  variable_names <- 
    with(variables_df %>% filter(!is.na(mbms)),
      setNames(mbms, measure))

  mbms %<>% rename(!!! variable_names)

  return(mbms)
}

#' Rename MESA Variables
rename_mesa_variables <- function(mesa) {

  variable_names <- 
    with(variables_df %>% filter(!is.na(mesa)),
      setNames(mesa, measure))

  mesa %<>% rename(!!! variable_names)

  return(mesa)
}
