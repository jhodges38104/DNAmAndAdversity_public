
#' Make a Regression Ready MBMS Dataset
#' 
#' This function creates a "regression ready" 
#' version of the MBMS dataset.  This means that 
#' categorical/factor variables are expanded 
#' into columns for each level with 1 indicating true
#' and 0 indicating false. 
#' 
#' @export
make_regression_ready_mbms <- function(save = F, just_epigenetics_cohort = F) {

  mbms <- make_clean_mbms()

  dummy <- function(v) { ifelse(v, 1, 0) }

  mbms_ready <- mbms %>% mutate(

    male = dummy(gender == "Male"), 
    black = dummy(race == "Black N.H."),

    born_northeast = dummy(birth_region == "northeast"),
    born_midwest = dummy(birth_region == "midwest"),
    born_west = dummy(birth_region == "west"),
    born_south = dummy(birth_region == "south"),
    
    jim_crow = dummy(JC == 'TRUE'),

    mother_not_usborn = dummy(Mother == 2),
    father_not_usborn = dummy(Father == 2),

    married = dummy(M_Status == 1),
    divorced_separated_widowed = dummy(M_Status == 2),
    living_w_partner = dummy(M_Status == 3),
    serious_relationship = dummy(M_Status == 4),
    single = dummy(M_Status == 5),
    
    household_size = Adults_supported + Children_supported,
    hh_income_2010_dollars = hh_income_2010_dollars,
    hh_income_per_capita_2010 = hh_income_2010_dollars / (Adults_supported + Children_supported),
    neg_log_poverty_ratio = -log10(ratio_to_2010_poverty_line),

    college_degree = dummy(educ_r == '4+ yrs college'),
    no_hs_degree = dummy(educ_r == '< HS'),
    hs_no_college = dummy(educ_r == '>=HS and <4 yrs college'),

    parents_college_degree = dummy(parents_highest_edu == '4+ yrs college'),
    parents_no_hs = dummy(parents_highest_edu == '< HS'),
    parents_hs_no_college = dummy(parents_highest_edu == '>= HS and <4 yrs college'),

    waist_hip_ratio = intWaist / intHip * 100,
    hypertension = dummy(ifelse(is.na(hbp_f), NA, hbp_f %in% 2:5)),
    diabetes = dummy(Diabetes == "Yes"),
    hypertension_med = dummy(med_bp_new == "Yes"),
    diabetes_med = dummy(med_diab_new == "Yes"),
    metabolic_syndrome = dummy(metab_d == "Yes"),
    asthma = dummy(Asthma == 1),
    bmi_overweight = dummy(bmi_cat == "BMI 25 TO 25.9  =  MODERATELY OVERWEIGHT"),
    bmi_obese = dummy(bmi_cat == "BMI 25 TO 25.9  =  OVERWEIGHT"),

    high_physical_activity = dummy(phys_act == "3 = high"),
    low_physical_activity = dummy(phys_act == "1 = low"),

    ct_highest_poverty = dummy(pov_us_5 == 1),
    ct_pov_20pct_and_up = dummy(ct_pov_cat == '>=20%'),
    ct_pov_5_to_20pct = dummy(ct_pov_cat == '>=5% <20%'),
    high_wealth = wealth_h,
    econ_insecure = dummy(econ_insecure == 1),
    econ_secure = dummy(econ_insecure == 3),
    home_owner_free_and_clear = dummy(as.numeric(House_owner) == 2),
    paying_mortgage = dummy(as.numeric(House_owner) == 1),
    renter = dummy(as.numeric(House_owner) == 3),
    debt_over_5000 = dummy(debt == 2),

    supervisory_employee = dummy(occ_class_r == "1 = owner, self-employed, supervisor"),
    nonsupervisory_employee = dummy(occ_class_r == "2 = non-supervisory employee"),
    unemployed = dummy(occ_class_r == "3 = unemployed, not in paid labor force, other"),

    smoke_last_8hrs = dummy(Smoke_now_8hrs == 'Current, in last 8hrs'),
    smoke_not_in_8hrs = dummy(Smoke_now_8hrs == 'Current, not in 8hrs'),
    ex_smoker = dummy(Smoke_now_8hrs == 'Ex-smoker'),
    current_smoker = dummy(ifelse(is.na(Smoke_now), NA, Smoke_now %in% c('Yes, every day', 'Yes, some days'))),
    never_smoker = dummy(Smoke == 'No'),

    sleep_under_5 = dummy(Amount_of_sleep == "Less than 5 hours"),
    sleep_5_to_6 = dummy(Amount_of_sleep == "5 to 6 hours"),
    sleep_over_7 = dummy(Amount_of_sleep == "7 or more hours"),

    alc_last_24hrs = dummy(intLastAlc == "Yes"),
    food_last_8hrs = dummy(intLastFood == "Yes"),

    do_something = dummy(Unfr == "Do something"),
    talk_to_others = dummy(Unfr2 == "Talk"),
    
    epi_final_sample = dummy(final_status == 'included')
  )

  col_renames <- 
    c(
    #idno = 'ParticipantID',
    age = 'AgeYRS',
    household_children = 'Children_supported',
    height = 'height_cm',
    weight = 'weight_kg',
    hip = 'intHip',
    waist = 'intWaist',
    tibia_height_ratio = 'tibia_ratio',
    glucose = 'intGlucose',
    ldl_cholesterol = 'intLDLChol',
    hdl_cholesterol = 'intHDLChol',
    diastolic_bp = 'dbp',
    systolic_bp = 'sbp',
    framingham_cvd = 'frisk',
    poverty_ratio = 'ratio_to_2010_poverty_line',
    hh_income = 'hh_income_2010_dollars',
    hh_income_per_capita = 'hh_income_per_capita_2010',
    adult_deprivation = 'adult_dep_sum',
    child_deprivation = 'childhood_dep_sum',
    poverty_below_2x = 'pov_below_2x',
    smoking = 'Smoke_now_8hrs',
    social_desirability = 'soc_des',
    eds_racial_small_scale = 'eds_ut_rd',
    eds_racial_fine_scale = 'eds_f_rd',
    eod_racial_small_scale = 'eod_s',
    eod_racial_fine_scale = 'eod_f',
    lifetime_discrimination = 'Entire_Lifetime_Stress',
    ct_pct_white = 'percWNH',
    ct_pct_black = 'percBNH',
    ct_pct_hispanic = 'perc_hispanic',
    ct_pct_asian = 'perc_asianNH',
    ct_pct_aian = 'perc_aianNH',
    ct_pct_nhopi = 'perc_nhopiNH',
    ICEown = 'ct_ICEown',
    ICEraceown = 'ct_ICEwbown',
    ct_pov = 'percpov',
    birth_ice_race = 'birth_ice_race',
    birth_state_liberalism = 'liberalism',
    black_carbon = 'year_ave',
    pollution_proximity = 'ppi5',
    iat_mt = 'score_MT',
    iat_bw = 'score_BW',
    #epi_slide = 'Slide',
    epi_row = 'sentrix_row'
  )

  mbms_ready %<>% rename(!! col_renames)

  mbms_ready %<>% select(
    
    idno, 
    
    !! names(col_renames),

    male, black, 

    born_northeast, born_midwest, born_west, born_south, 
    
    jim_crow, birth_state_liberalism,

    mother_not_usborn, father_not_usborn, 
    
    household_size,

    married, divorced_separated_widowed, living_w_partner, serious_relationship, 
    single,

    college_degree, no_hs_degree, hs_no_college, 

    parents_college_degree, parents_no_hs, parents_hs_no_college, 
    
    waist_hip_ratio, hypertension, diabetes, hypertension_med,
    diabetes_med, metabolic_syndrome, asthma, bmi_overweight, bmi_obese,
    bmi, 

    high_physical_activity, low_physical_activity,

    ct_highest_poverty, ct_pov_20pct_and_up, ct_pov_5_to_20pct, 
    high_wealth, econ_insecure, econ_secure,
    home_owner_free_and_clear,  paying_mortgage, renter, debt_over_5000, 
    
    neg_log_poverty_ratio,

    supervisory_employee, nonsupervisory_employee, unemployed, 

    current_smoker, never_smoker, smoke_last_8hrs, smoke_not_in_8hrs, ex_smoker, 

    sleep_under_5, sleep_5_to_6, sleep_over_7,

    alc_last_24hrs, food_last_8hrs, 

    do_something, talk_to_others,

    ICEinc, ICErace, ICEraceinc, ICEown, ICEraceown,

    hostility,
    
    epi_final_sample,
    
    !!! epigenetic_clock_variables,
    !!! cell_type_variables,
    #epi_slide,
    epi_row 
    )
  
  if (just_epigenetics_cohort) {
    mbms_ready %<>% filter(epi_final_sample == 1) 
    
    # add epigenetic clocks adjusted for age
    for (clock in epigenetic_clock_variables) {
      clock_chr <- clock %>% as.character() %>% `[[`(2)
      mbms_ready[[str_c(clock_chr, "_adjusted")]] <-
        residuals(lm(mbms_ready[[clock_chr]] ~ mbms_ready[['age']]))
    }
  }

  if (save) {
    write.csv(
      mbms_ready,
      file.path(
        system.file(
          'regression_ready_versions/',
          package='DNAm'),
        'mbms_regression_ready.csv'),
    row.names=F)
  }
  
  attr(mbms_ready, 'name') = 'MBMS'

  return(mbms_ready)
}



#' Load the Regression Ready Version of MBMS
#'
load_regression_ready_mbms <- function() {
  readr::read_csv(
    system.file('regression_ready_versions/mbms_regression_ready.csv',
      package='DNAm'))
}
