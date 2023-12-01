# helper variables --------------------------------------------------------

# read in data with sentrix IDs and mbms IDs
mbms_qc_quality <- readr::read_csv(
  system.file(
    "cleaned_mbms/epi_qc/mbms_qc_quality.csv",
    package = 'DNAm'
  )
)

# create a converter object from sentrix IDs to mbms IDs
sentrix_to_mbms_id_converter <-
  setNames(nm = mbms_qc_quality$sentrix_id, object = mbms_qc_quality$participant)


early_life_exposures_mbms <- c(
  'jim_crow',
  'rev_birth_ice_race',
  'birth_state_conservatism',
  'parents_educ',
  'educ'
)

adult_life_exposures_mbms <- c(
  'eod_racial_3_levels',
  'neg_log_poverty_ratio',
  'occupation',
  'housing',
  'black_carbon',
  'pollution_proximity',
  'rev_ICEinc',
  'rev_ICEraceinc',
  'rev_ICErace',
  'rev_ICEown'
)


early_life_exposures_mesa <- c(
  'jim_crow',
  'birth_state_conservatism',
  'parents_educ',
  'educ'
)
adult_life_exposures_mesa <- c(
  'mds_racial_3_levels',
  'neg_log_poverty_ratio',
  'occupation',
  'housing',
  'light_absorption',
  'nitrous_oxides',
  'rev_ICEinc',
  'rev_ICEraceinc',
  'rev_ICErace',
  'rev_ICEown'
)


lookup_for_mbms_sv_filenames <- c(
  'jim_crow' = 'jim_crow_birth_state',
  'rev_birth_ice_race' = 'birthplace_ICErace',
  'birth_state_conservatism' = 'birth_state_policy_liberalism',
  'parents_educ' = 'parents_highest_edu',
  'educ' = 'educ',
  'eod_racial_3_levels' = 'eod',
  'neg_log_poverty_ratio' = 'hh_income_pov_ratio',
  'occupation' = 'occupational_class',
  'housing' = 'housing_tenure',
  'black_carbon' = 'black_carbon_yearly_avg',
  'pollution_proximity' = 'nitrous_oxides_pollution_proximity_index',
  'rev_ICEinc' = 'ICEinc',
  'rev_ICEraceinc' = 'ICEraceinc',
  'rev_ICErace' = 'ICErace',
  'rev_ICEown' = 'ICEown'
)

lookup_for_mesa_sv_filenames <- c(
  'jim_crow' = 'jim_crow_birth_state',
  'rev_birth_ice_race' = 'birthplace_ICErace',
  'birth_state_conservatism' = 'birth_state_policy_liberalism',
  'parents_educ' = 'parents_highest_edu',
  'educ' = 'educ',
  'mds_racial_3_levels' = 'mds',
  'neg_log_poverty_ratio' = 'hh_income_pov_ratio',
  'occupation' = 'occupation',
  'housing' = 'housing_tenure',
  'light_absorption' = 'LAC_lik_0_1_yr_exam5_wsp',
  'nitrous_oxides' = 'NOx_lik_0_1_yr_exam5_wght',
  'rev_ICEinc' = 'ICEinc',
  'rev_ICEraceinc' = 'ICEraceinc',
  'rev_ICErace' = 'ICErace',
  'rev_ICEown' = 'ICEown'
)

exposure_variables_named_chr <- c(
  'Born in a Jim Crow State' = "jim_crow", 
  'Reversed ICE for Racialized Segregation at City of Birth (Black vs. White)' = 'rev_birth_ice_race',
  'State Policy Liberalism in State of Birth' = 'birth_state_liberalism',
  'State Policy Conservatism in State of Birth' = 'birth_state_conservatism',
  "Parent's Highest Education: < 4-year College Degree [ref: Parents with 4-year College Degree]" = "parents_educ_parents_lt_4yr_college_degree",
  "Participant's Highest Education: < 4-year College Degree [ref: 4-year College Degree]" = "educ_lt_4yr_college_degree",
  "Experiences of Discrimination" = "eod_racial_small_scale",
  "Experiences of Discrimination (0) [ref: 1-2]" = "eod_racial_3_levels_0",
  "Experiences of Discrimination (3+) [ref: 1-2]" = "eod_racial_3_levels_3p",
  "Major Discrimination Scale (0) [ref: 1-2]" = "mds_racial_3_levels_0",
  "Major Discrimination Scale (3+) [ref: 1-2]" = "mds_racial_3_levels_3p",
  "-log10(household income / poverty line) in 2010 Dollars" = "neg_log_poverty_ratio",
  "Occupational Class — Non-Supervisory Employee [ref: Supervisory Employee, Owner]" = "occupation_nonsupervisory_employee", 
  "Employment Status — Unemployed [ref: Employed]" = "occupationunemployed", 
  "Housing Tenure — Paying Mortgage [ref: Own Home Free and Clear]" = "housing_paying_mortgage", 
  "Housing Tenure — Paying Rent [ref: Own Home Free and Clear]" = "housing_renter", 
  "Black Carbon (Higher = More Pollution)" = "black_carbon",
  "Light Absorption Coefficient (Higher = More Pollution)" = "light_absorption",
  "Pollution Proximity Index (Higher = More Pollution)" = "pollution_proximity",
  "Oxides of Nitrogen (Higher = More Pollution)" = "nitrous_oxides",
  "Reversed Residential Census Tract ICE for Income (Low Income vs. High Income)" = "rev_ICEinc",
  "Reversed Residential Census Tract ICE for Racial Segregation (Black vs. White non-Hispanic)" = "rev_ICErace",
  "Reversed Residential Census Tract ICE for Racialized Economic Segregation (Low Income Black vs. White non-Hispanic High Income)" = "rev_ICEraceinc",
  "Reversed Residential Census Tract ICE for Housing Tenure (Renters vs. Paying Mortgage or Own Free and Clear)" = "rev_ICEown"
)

# helper variables for separating gen 1 and gen 2 clocks
gen_1_clocks <- c(
  'age.horvath',
  'age.hannum',
  'epiToc.clock',
  'zhang_clock',
  'miage.clock',
  'dnamtl'
)

# gen 1 clocks shorter excludes MiAge, epiToc, and DNAmTL, which were the clocks that 
# are not age predictors
gen_1_clocks_shorter <- c('age.hannum', 'age.horvath', 'zhang_clock')

gen_2_clocks <- c(
  'phenoage',
  'zhang.mortality',
  'dunedin_age',
  'DNAmGrimAge'
)


replace_dash_and_space_colnames <- function(df) {
  colnames(df) <- stringr::str_replace_all(colnames(df), "-|[:space:]", "_")
  return(df)
}

replace_dashes_and_spaces <- function(str) {
  stringr::str_replace_all(str, "-|[:space:]", "_") 
}

replace_plus_with_p <- function(str) {
  stringr::str_replace_all(str, "\\+", "p") 
}

###----------------------------------------------------------------------------

# Figure revisions requested prior to journal submittion that require condensing
# the number of figures in the manuscript. Per Nancy on 8.15.23, submission manuscript 
# will contain 4 figures:


fig_1_variables <- c(early_life_exposures_mbms,
                     'eod_racial_3_levels',
                     'neg_log_poverty_ratio',
                     'occupationnonsupervisory_employee')

fig_2_variables <- c('occupationunemployed',
                     'housing',
                     'black_carbon',
                     'pollution_proximity',
                     'rev_ICEinc',
                     'rev_ICEraceinc',
                     'rev_ICErace',
                     'rev_ICEown')

fig_3_variables <- c(early_life_exposures_mesa,
                     'mds_racial_3_levels',
                     'neg_log_poverty_ratio',
                     'occupation')

fig_4_variables <- c('housing',
                     'light_absorption',
                     'nitrous_oxides',
                     'rev_ICEinc',
                     'rev_ICEraceinc',
                     'rev_ICErace',
                     'rev_ICEown')
