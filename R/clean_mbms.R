
#' Make Clean MBMS
make_clean_mbms <- function() {
  mbms <- readRDS(system.file('cleaned_mbms/clean_mbms.rds', package='DNAm'))
  return(mbms)
}

#' Original code for Making a Clean Version of MBMS
#' @export
original_make_clean_mbms <- function() {
  mbms <- make_mbms_sas_csv_merged()
  mbms <- make_factored_mbms(mbms)
  mbms <- relevel_mbms(mbms)
  ct_measures <- load_mbms_census_tract_measures()
  mbms <- merge_mbms_and_census_tract_measures(mbms, ct_measures)
  mbms <- merge(mbms, load_mbms_air_pollution_data(), all.x=T)
  mbms %<>% add_place_of_birth_absms()
  mbms %<>% merge_mapc_into_mbms()
  mbms %<>% merge_mbms_ct_housing_tenure()
  mbms %<>% reformat_mbms_for_comparison()
  mbms %<>% merge_state_policy_liberalism_into_mbms()
  mbms %<>% add_epi_clocks_to_mbms()
  mbms %<>% add_epi_final_sample_info_to_mbms()
  mbms %<>% adjust_mbms_hh_income_to_2010_dollars()
  mbms %<>% add_ratio_to_2010_poverty_line_mbms()
  return(mbms)
}

#' Save a Clean Version of the Cleaned MBMS dataset
save_clean_mbms <- function() {
  
  mbms <- original_make_clean_mbms()

  set.seed(123) # set seed to ensure reproducibility
  
  mbms %<>% 
    mutate(idno = sprintf("%05d", sample(10000:99999, 1))) %>% # generate dummy ids for random effects 
    select(-c(
      GEOID10,
      ParticipantID,
      g250m_id,
      strRA,
      SurveyDate,
      dtmDOBday,
      dtmDOBmonth,
      BirthDate,
      SurveyTime,
      strRANotes,
      starts_with("other"), # write-in drugs 
      Exper,
      Inifile, 
      Time, 
      que1,
      Welcome,
      wherefrom,
      special,
      ID,
      Blood_Spot_Card_Number,
      Sample_Date,
      Notes,
      Date_frozen,
      Loc_name,
      X,
      Y,
      geo_id2,
      dc_mt,
      dc_bw,
      CityOB,
      city_final,
      AREAKEY,
      SOURCE_NAME40,
      TABLE_SOURCE40,
      SOURCE_NAME50,
      TABLE_SOURCE50,
      SOURCE_NAME60,
      TABLE_SOURCE60,
      birth_year_mod_10,
      `...1`,
      id,
      Slide)) %>% 
    relocate(idno, .before = SurveyTimeAMPM)
  

  saveRDS(mbms, system.file("cleaned_mbms/clean_mbms.rds", package="DNAm"))
  write.csv(mbms, system.file("cleaned_mbms/clean_mbms.csv", package="DNAm"), row.names=FALSE)
}

#' Make SAS + CSV Merged Dataset
#' 
#' Make a merged dataset starting with the SAS dataset 
#' and appending on those columns from the CSV which are 
#' newly appended. 
#' 
make_mbms_sas_csv_merged <- function() { 

  # load raw data
  mbms_sas <- load_mbms_sas()
  mbms_csv <- load_mbms_csv()

  # check which columns were appended to the SAS to make the CSV
  appended_colnames <- setdiff(colnames(mbms_csv), colnames(mbms_sas))

  # join the SAS & appended_columns (i.e. those in the CSV but not in SAS)
  mbms <- full_join(mbms_sas, mbms_csv[,c('ParticipantID', appended_colnames)])

  # convert columns into factors
  mbms %<>% make_factored_mbms

  return(mbms)
}

#' Reformat MBMS for Comparison to MESA
reformat_mbms_for_comparison <- function(mbms) {

  # make state name lowercase
  mbms$StateOB %<>% tolower() %>% tools::toTitleCase()

  # add abbreviated statenames 
  states_abb <- c(setNames(state.abb, state.name), "District of Columbia" = "DC")
  mbms$StateOB_abb <- states_abb[mbms$StateOB]

  # add birth region
  mbms$birth_region <- regions_lookup[mbms$StateOB_abb]

  mbms$occ_class_reformatted <- mbms$occ_class_r

  mbms$occ_class_reformatted %<>% recode(
    "1 = owner, self-employed, supervisor" = "Employed", 
    "2 = non-supervisory employee" = "Employed",
    "3 = unemployed, not in paid labor force, other" = "Not Employed"
  )

  mbms$occ_class_reformatted %<>% forcats::fct_relevel("Not Employed")
  
  mbms$Smoke_now_reformatted <- mbms$Smoke_now

  mbms$Smoke_now_reformatted %<>% recode(
    "Yes, every day" = "Yes",
    "Yes, some days" = "Yes",
    "No, not at all" = "No")

  mbms$Smoke_now_reformatted %<>% forcats::fct_explicit_na("No")

  mbms$intLastCig %<>% factor(labels = c(`1` = "Yes", `2` = "No")) %>% relevel("No")
  mbms$intLastAlc %<>% factor(labels = c(`1` = "Yes", `2` = "No")) %>% relevel("No")

  mbms$intLastFood <- ifelse(mbms$intLastFoodHours > 8, 1, 2)
  mbms$intLastFood %<>% factor(labels = c(`1` = "Yes", `2` = "No")) %>% relevel("No")

  # mbms$Smoke_now %<>% forcats::fct_expand("Current, in last 8hrs", "Current, not in 8rs")
  mbms %<>% mutate(
    Smoke_now_8hrs = case_when(

      sm_current == 'Yes' & 
      intLastCig == 'Yes' ~ 'Current, in last 8hrs',
    
      sm_current == 'Yes' & 
      intLastCig == 'No' ~ 'Current, not in 8hrs',

      sm_ex == 1 ~ "Ex-smoker",
      sm_never == 1 ~ "Never smoker") %>% factor() %>% relevel("Never smoker"))


  mbms$JC[is.na(mbms$JC)] <- 0
  mbms$JC %<>% as.logical()

  mbms$pov_below_2x <- ifelse(mbms$pov_us < 2, 1, 0)

  mbms$ct_pov_cat <- with(mbms, case_when(
      percpov < .05 ~ '<5%',
      percpov >= .05 & percpov < .2 ~ '>=5% <20%',
      percpov >= .2 ~ '>=20%')) %>% factor(
    levels = c('<5%', '>=5% <20%', '>=20%'))

  mbms %<>% mutate(
    # get the max of parents edus as a numeric
    parents_highest_edu = 
      # we use suppressWarnings({}) here because we later explicitly 
      # deal with the behavior of max(NA, NA) returning -Inf, which 
      # is warned about
      suppressWarnings({apply(
        mbms[,c('Mother_Ed', 'Father_Ed')] %>% 
          mutate_if(is.factor, as.numeric), 1, max, na.rm=T)}),

    # convert to a factor with NA and levels 1:6
    parents_highest_edu = factor(
      parents_highest_edu,
      levels = c(1:7, `8` = NA, `-Inf` = NA)),

    # convert to a factor with namd levels
    parents_highest_edu = recode(
      parents_highest_edu,
      !!! setNames(levels(mbms$Mother_Ed)[1:7], 1:7)),
    

    # aggregate levels 
    parents_highest_edu = recode(parents_highest_edu,
      "Less than 12th grade" = "< HS",
      "High school degree" = ">= HS and <4 yrs college",
      "GED" = ">= HS and <4 yrs college",
      "Some college" = ">= HS and <4 yrs college",
      "Vocational school" = ">= HS and <4 yrs college",
      "4 years of college" = "4+ yrs college",
      "Graduate degree" = "4+ yrs college"),
    
    # create mbms eds_f and eds_f_rd
    eds_f = 
      (-1*Court + 6) + 
      (-1*Serv + 6) + 
      (-1*Smart + 6) + 
      (-1*Afraid + 6) + 
      (-1*Haras + 6),
    
    eds_f_rd = eds_f * (Why__multiple__01 | Why__multiple__03 | (Why__main_ %in% c(1,3))),
    
    
    # For comparison with MESA, 
    # set the "Occupied without payment of cash rent" category to NA
    House_owner = ifelse(House_owner == "Occupied without payment of cash rent", NA, House_owner)
   
  )

  return(mbms)
}

#' Add Place of Birth ABSMs 
add_place_of_birth_absms <- function(mbms) {

  mbms %<>% rename(
    BLACK40 = NEGRO40, 
    BLACK50 = NEGRO50,
    BLACK60 = NEGRO60,
    BLACK70 = RACE_NG.y
  )
  
  mbms %<>% rename(
    WHITE70 = RACE_W.y
  )
  
  mbms %<>% rename(
    TOTAL70 = X100POPCO.y
  )
  
  mbms %<>% mutate(
    birth_year_mod_10 = lubridate::year(BirthDate) %% 10
  )
  
  mbms %<>% mutate(
    birth_decade = (lubridate::year(BirthDate) - birth_year_mod_10) - 1900)
  
  mbms %<>% mutate(
    birth_black_count = case_when(
      birth_decade == 40 ~ BLACK40 * (1-(birth_year_mod_10/10)) + BLACK50 * (birth_year_mod_10/10),
      birth_decade == 50 ~ BLACK50 * (1-(birth_year_mod_10/10)) + BLACK60 * (birth_year_mod_10/10),
      birth_decade == 60 ~ BLACK60 * (1-(birth_year_mod_10/10)) + BLACK70 * (birth_year_mod_10/10),
      birth_decade == 70 ~ BLACK70),
    birth_white_count = case_when(
      birth_decade == 40 ~ WHITE40 * (1-(birth_year_mod_10/10)) + WHITE50 * (birth_year_mod_10/10),
      birth_decade == 50 ~ WHITE50 * (1-(birth_year_mod_10/10)) + WHITE60 * (birth_year_mod_10/10),
      birth_decade == 60 ~ WHITE60 * (1-(birth_year_mod_10/10)) + WHITE70 * (birth_year_mod_10/10),
      birth_decade == 70 ~ WHITE70),
    birth_total_count = case_when(
      birth_decade == 40 ~ TOTAL40 * (1-(birth_year_mod_10/10)) + TOTAL50 * (birth_year_mod_10/10),
      birth_decade == 50 ~ TOTAL50 * (1-(birth_year_mod_10/10)) + TOTAL60 * (birth_year_mod_10/10),
      birth_decade == 60 ~ TOTAL60 * (1-(birth_year_mod_10/10)) + TOTAL70 * (birth_year_mod_10/10),
      birth_decade == 70 ~ TOTAL70),
    birth_frc_black = birth_black_count / birth_total_count,
    birth_frc_white = birth_white_count / birth_total_count,
    birth_ice_race = case_when(
      birth_decade == 40 ~ ICE_race40 * (1-(birth_year_mod_10/10)) + ICE_race50 * (birth_year_mod_10/10),
      birth_decade == 50 ~ ICE_race50 * (1-(birth_year_mod_10/10)) + ICE_race60 * (birth_year_mod_10/10),
      birth_decade == 60 ~ ICE_race60 * (1-(birth_year_mod_10/10)) + ICE_race70 * (birth_year_mod_10/10),
      birth_decade == 70 ~ ICE_race70)
  )

  return(mbms)
}


#' Adjust MBMS Household Income to 2010 dollars 
#'
adjust_mbms_hh_income_to_2010_dollars <- function(mbms) {
  
  inflation_df <- load_inflation_df() 
  
  mbms %<>% rowwise() %>% 
    mutate(hh_income_2010_dollars =
                       adjust_to_2010_dollars(
                         inflation_df = inflation_df,
                         from_year = SurveyYear,
                         orig_amount = (inc1 %>% as.character() %>% as.numeric())
                       )
                     )
  return(mbms)
}


#' Calculate Household Poverty Based on 2010 Household Income Dollars
#'
add_ratio_to_2010_poverty_line_mbms <- function(mbms) {
  
  pov_thresholds_2010 <- load_poverty_thresholds(year = 2010)
  
  mbms %<>% mutate(
    ratio_to_2010_poverty_line = 
      calculate_poverty_ratio(
        thresholds = pov_thresholds_2010, 
        household_income = hh_income_2010_dollars,
        total_people = Adults_supported + Children_supported,
        num_seniors = 0, # we don't have any data to inform this -- so an assumption
        # has to be made, and given that the MBMS participants were 
        num_children = Children_supported
      )
  )
}
