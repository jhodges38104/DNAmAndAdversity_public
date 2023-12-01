
#' Make a Regression Ready MESA Dataset
#' 
#' @seealso make_regression_ready_mbms
#' @param exclude_missing_nativity logical for whether the 2 NA 
#' @export
make_regression_ready_mesa <- function(save=F, exclude_missing_nativity = TRUE) {

  mesa <- make_clean_mesa() 
  
  if (exclude_missing_nativity) {
    mesa %<>% filter(! is.na(bth1))
  }

  dummy <- function(v) { ifelse(v, 1, 0) }

  mesa$exercm5c_cat <- cut(
    mesa$exercm5c, 
    c(-Inf, quantile(mesa$exercm5c, c(0.33, 0.66, 1), na.rm=T), 
      max(mesa$exercm5c)),
    labels = c('low', 'middle', 'high')
  )
  
  # convert recruitment site variable to factor
  mesa$site5c %<>% haven::as_factor()
  
  mesa_ready <- mesa %>% mutate(

    male = dummy(gender1 == "Male"),
    black = dummy(race1c == "Black N.H."),
    white = dummy(race1c == 'White N.H.'),
    hispanic = dummy(race1c == "Hispanic"),

    us_born = dummy(bth1 != 'Another Country'),
    puerto_rican_born = dummy(bth1 == 'Puerto Rico'),

    born_northeast = dummy(birth_region == "northeast"),
    born_midwest = dummy(birth_region == "midwest"),
    born_west = dummy(birth_region == "west"),
    born_south = dummy(birth_region == "south"),

    mother_not_usborn = dummy(mbth1 == "Yes"),
    father_not_usborn = dummy(fbth1 == "Yes"),

    married_or_living_w_partner = dummy(marital5 == "Married / Living with Partner"),
    divorced = dummy(marital5 == "Divorced"),
    never_married = dummy(marital5 == "Never Married"),
    separated = dummy(marital5 == "Separated"),
    widowed = dummy(marital5 == "Widowed"),
    relationship_prefer_not_to_answer = dummy(marital5 == "Prefer Not to Answer"),
    
    college_degree = dummy(educ_reformatted == '4+ yrs college'),
    no_hs_degree = dummy(educ_reformatted == '< HS'),
    hs_no_college = dummy(educ_reformatted == '>= HS and <4 yrs college'),

    # the comparison here is meant to be 
    # parents with a high school degree vs. those with no 
    # high school degree or those with a college degree;
    # 
    # as such, parents_no_hs is inclusive of those with some schooling 
    # and no high school degree as well as those with no schooling;
    # similarly, parents_college_degree is inclusive of those with a 
    # 4 year degree as well as those with a graduate degree.
    # 
    parents_no_hs = dummy(parents_highest_edu == 'Some Schooling,\nDid Not Complete H.S.' | parents_highest_edu == 'No Schooling'),
    parents_hs_no_college = dummy(parents_highest_edu == 'High School Degree' | parents_highest_edu == 'Some College'),
    parents_college_degree = dummy(parents_highest_edu == 'College Degree' | parents_highest_edu == 'Graduate Degree'),

    waist_hip_ratio = waistcm5 / hipcm5 * 100,
    hypertension = dummy(htn5c == 'Yes'),
    diabetes = dummy(diabet1 == "Yes"),
    hypertension_med = dummy(htnmed5c == "Yes"),
    diabetes_med = dummy(diabins5 == "Yes"),
    metabolic_syndrome = dummy(metsyn5c == "Yes"),
    asthma = dummy(asthma5 == 1),
    bmi_grade1_overweight = dummy(bmicat5c == "Grade 1 Overweight"),
    bmi_grade2_overweight = dummy(bmicat5c == "Grade 2 Overweight"),
    bmi_grade3_overweight = dummy(bmicat5c == "Grade 3 Overweight"),

    high_physical_activity = dummy(exercm5c_cat == "high"),
    low_physical_activity = dummy(exercm5c_cat == "low"),

    ct_pov_20pct_and_up = dummy(ct_pov_cat == '>=20%'),
    ct_pov_5_to_20pct = dummy(ct_pov_cat == '>=5% <20%'),
    home_owner_free_and_clear = dummy(hometyp1 == 'Own Free and Clear'),
    paying_mortgage = dummy(hometyp1 == 'Pay a Mortgage'),
    renter = dummy(hometyp1 == 'Rent'),
    hh_income_per_capita_2010 = hh_income_2010_dollars / numhhld5,
    neg_log_poverty_ratio = -log10(ratio_to_2010_poverty_line),

    # employed and unemployed are mutually exclusive, but 10 = retired, working 
    # so they are in both retired and employed
    #
    # the ifelse(is.na(.), NA, as.numeric(.) %in% ...) pattern being used here 
    # is because NA %in% vector for any vector not including NA returns FALSE
    # rather than NA 
    employed = dummy(ifelse(is.na(occupation), NA, as.numeric(occupation) %in% c(2:5, 9))), 
    unemployed = dummy(ifelse(is.na(occupation), NA, as.numeric(occupation) %in% c(1, 6:8,10))),
    retired = dummy(ifelse(is.na(occupation), NA, as.numeric(occupation) %in% 8:10)),

    current_smoker = dummy(smkstat5 == "3: CURRENT SMOKER"),
    smoker_former_quit_over_1yr = dummy(smkstat5 == "1: FORMER SMOKER QUIT MORE THAN 1 YEAR AGO"),
    smoker_former_quit_under_1yr = dummy(smkstat5 == "2: FORMER SMOKER QUIT LESS THAN 1 YEAR AGO"),
    never_smoker = dummy(smkstat5 == "0: NEVER SMOKED"),
    

    do_something = dummy(uf1resp1 == "Do something about it"),
    talk_to_others = dummy(uf2resp1 == "Talk to others about it")
    )

  col_renames <- c(
    idno = 'idno',
    age = 'age5c',
    site = 'site5c',
    household_size = 'numhhld5',
    household_children = 'nhhldc5',
    hh_income = 'hh_income_2010_dollars',
    hh_income_per_capita = 'hh_income_per_capita_2010',
    height = 'htcm5',
    weight = 'wtkg5_reformatted',
    hip = 'hipcm5',
    waist = 'waistcm5',
    glucose = 'glucose5',
    bmi = 'bmi5c',
    ldl_cholesterol = 'ldl5',
    hdl_cholesterol = 'hdl5',
    diastolic_bp = 'dbp5c',
    systolic_bp = 'sbp5c',
    framingham_cvd = 'frci085c',
    lifetime_discrimination = 'discrl1c',
    ct_pct_white = 'race_whiteNH',
    ct_pct_black = 'race_blackNH',
    ct_pct_hispanic = 'race_hisp',
    ct_pct_asian = 'race_asianNH',
    ct_pct_aian = 'race_AIANNH10',
    ct_pct_nhopi = 'race_NHOPINH10',
    ct_pct_other = 'race_otheraloneNH10',
    ct_pov = 'pov_allage',
    jim_crow = 'JC',
    birth_state_liberalism = 'liberalism',
    poverty_ratio = 'ratio_to_2010_poverty_line',
    eds_racial_small_scale = 'eds_s_rd',
    eds_racial_small_scale = 'eds_s_rd',
    eds_racial_fine_scale = 'eds_f_rd',
    mds_racial_small_scale = 'mds_rd',
    nitrous_oxides = 'NOx_lik_0_1_yr_exam5_wght',
    light_absorption = 'LAC_lik_0_1_yr_exam5_wsp',
    epi_slide = 'slide',
    epi_row = 'row'
  )

  mesa_ready %<>% rename(!! col_renames)

  mesa_ready %<>% select(
    !! names(col_renames),
    male, black, white, hispanic,
    jim_crow, birth_state_liberalism,
    us_born, born_northeast, born_midwest, born_west, born_south, 
    mother_not_usborn, father_not_usborn, 
    married_or_living_w_partner, divorced, never_married, separated, 
    widowed,
    college_degree, no_hs_degree, hs_no_college,
    parents_no_hs,
    parents_hs_no_college,
    parents_college_degree,
    waist_hip_ratio,
    hypertension,
    diabetes,
    hypertension_med,
    diabetes_med,
    metabolic_syndrome,
    asthma,
    bmi_grade1_overweight,
    bmi_grade2_overweight,
    bmi_grade3_overweight,
    high_physical_activity,
    low_physical_activity,
    ct_pov_20pct_and_up,
    ct_pov_5_to_20pct,
    neg_log_poverty_ratio,
    home_owner_free_and_clear,
    paying_mortgage,
    renter,
    employed,
    unemployed,
    retired,
    current_smoker,
    never_smoker,
    smoker_former_quit_over_1yr,
    smoker_former_quit_under_1yr,
    do_something,
    talk_to_others,

    ICErace,
    ICEinc, 
    ICEraceinc,
    ICEown,
    ICEraceown,
    
    !!! epigenetic_clock_variables,
    !!! cell_type_variables,
    epi_slide,
    epi_row
  )
  
  # add epigenetic clocks adjusted for age
  for (clock in epigenetic_clock_variables) {
      clock_chr <- clock %>% as.character() %>% `[[`(2)
      mesa_ready[[str_c(clock_chr, "_adjusted")]] <-
        residuals(lm(mesa_ready[[clock_chr]] ~ mesa_ready[['age']]))
  }
  

  if (save) {
    write.csv(
      mesa_ready,
      file.path(
        system.file(
          'regression_ready_versions/',
          package='DNAm'),
        'mesa_regression_ready.csv'),
    row.names=F)
  }
  
  attr(mesa_ready, 'name') <- 'MESA'
  
  return(mesa_ready)
}


#' Load the Regression Ready Version of MESA
#'
load_regression_ready_mesa <- function() {
  readr::read_csv(
    system.file('regression_ready_versions/mesa_regression_ready.csv',
      package='DNAm'))
}

