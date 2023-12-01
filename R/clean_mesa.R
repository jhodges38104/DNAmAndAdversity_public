#' Make a Clean Version of MESA
#' @export
make_clean_mesa <- function() { 
  # load raw data
  mesa <- load_mesa_sas()

  # make factored mesa data 
  mesa <- make_factored_mesa(mesa)

  # reformat for comparison
  mesa <- reformat_mesa_for_comparison(mesa)

  # add jim crow states
  mesa$JC <- mesa$stbth1 %in% jim_crow_states

  # add us regions
  mesa$birth_region <- regions_lookup[mesa$stbth1]
  
  # add state policy liberalism
  mesa %<>% merge_state_policy_liberalism_into_mesa()
  
  # add epigenetic clocks data
  mesa %<>% add_epi_clocks_to_mesa()
  
  # load consumer price index inflation data
  inflation_df <- load_inflation_df()
  
  # adjust to 2010 dollars 
  mesa %<>% adjust_mesa_hh_income_to_2010_dollars()
  
  
  # add ratio to 2010 poverty line 
  mesa %<>% add_ratio_to_2010_poverty_line_mesa()
  
  # add the additional pct race/ethnicity data from 2022-01-12 
  mesa %<>% merge_in_pct_aian_nhopi_other()

  return(mesa)
}


#' Reformat MESA data for Comparison to MBMS
reformat_mesa_for_comparison <- function(mesa) { 

  mesa$bth1 <- factor(mesa$bth1, 
    labels = attr(mesa$bth1, 'labels') %>% 
      names() %>% 
      tolower() %>% 
      gsub("[0-9]: ", "", .) %>% 
      tools::toTitleCase() %>% 
      gsub("U.s.", "U.S.", .))

  mesa$educ_reformatted <- mesa$educ1

  mesa$educ_reformatted %<>% recode(
    "No Schooling" = "< HS", 
    "Grades 1-8" = "< HS", 
    "Grades 9-11" = "< HS", 
    "Completed High School/GED" = ">= HS and <4 yrs college", 
    "Some College but No Degree" = ">= HS and <4 yrs college", 
    "Technical School Certificate" = ">= HS and <4 yrs college", 
    "Associate Degree" = ">= HS and <4 yrs college", 
    "Bachelor's Degree" = "4+ yrs college", 
    "Graduate or Professional School" = ">= HS and <4 yrs college"
  )

  mesa %<>% mutate(

    # get the max of parents edus as a numeric
    
    parents_highest_edu = 
      # we use suppressWarnings({}) here because we later explicitly 
      # deal with the behavior of max(NA, NA) returning -Inf, which 
      # is warned about
      suppressWarnings({apply(
        mesa[,c('momschl2', 'dadschl2')] %>% 
         mutate_if(is.factor, as.numeric), 1, max, na.rm=T)}),

    # convert to a factor with NA and levels 1:6
    parents_highest_edu = factor(
      parents_highest_edu,
      levels = c(1:6, `-Inf` = NA)),

    # convert to a factor with namd levels
    parents_highest_edu = recode(
      parents_highest_edu,
      !!! setNames(levels(mesa$momschl2), 1:6)))

  # reformat weight from pounds to kilograms
  # 
  # this is probably more precision than is really needed, but best to be safer
  # than sorry!
  # 
  # .45359237
  # 
  # I found this conversion originally from wikipedia, but with the following citation:
  # 
  # https://en.wikipedia.org/wiki/Pound_(mass)
  # 
  # United States National Bureau of Standards (25 June 1959). 
  # "Notices "Refinement of values for the yard and the pound"" (PDF). Retrieved 12 August 2006.
  # https://www.ngs.noaa.gov/PUBS_LIB/FedRegister/FRdoc59-5442.pdf
  # 
  # I had a little trouble accessing that link, so I checked web-archive to confirm:
  # http://web.archive.org/web/20190312200258/https://www.ngs.noaa.gov/PUBS_LIB/FedRegister/FRdoc59-5442.pdf
  # 
  mesa$wtkg5_reformatted <- mesa$wtlb5
  mesa$wtkg5_reformatted %<>% multiply_by(.45359237)

  mesa$bmicat5c %<>% as.factor %>% 
    recode(
      `1` = "Normal",
      `2` = "Grade 1 Overweight",
      `3` = "Grade 2 Overweight",
      `4` = "Grade 3 Overweight")

  mesa$marital5 %<>% recode(
    "1" = "Married / Living with Partner",
    "2" = "Widowed",
    "3" = "Divorced",
    "4" = "Separated",
    "5" = "Never Married",
    "6" = "Prefer Not to Answer")

  mesa$most_recent_job <- as.numeric(mesa$curjob5)
  mesa %<>% mutate(
    most_recent_job = ifelse(is.na(most_recent_job), as.numeric(curjob4), most_recent_job),
    most_recent_job = ifelse(is.na(most_recent_job), as.numeric(curjob3), most_recent_job),
    most_recent_job = ifelse(is.na(most_recent_job), as.numeric(curjob2), most_recent_job),
    most_recent_job = ifelse(is.na(most_recent_job), as.numeric(curjob1), most_recent_job)
    )
  mesa$most_recent_job %<>% factor(labels = levels(mesa$curjob1))
  

  mesa$income5_num <- recode(mesa$income5,
    "< $5000" = 5000,
    "$5000 - $7999" = mean(c(5000,7999)),
    "$8000 - $11999" = mean(c(8000,11999)),
    "$12000 - $15999" = mean(c(12000,15999)),
    "$16000 - $19999" = mean(c(16000,19999)),
    "$20000 - $24999" = mean(c(20000,24999)),
    "$25000 - $29999" = mean(c(25000,29999)),
    "$30000 - $34999" = mean(c(30000,34999)),
    "$35000 - $39999" = mean(c(35000,39999)),
    "$40000 - $49999" = mean(c(40000,49999)),
    "$50000 - $74999" = mean(c(50000,74999)),
    "$75000 - $99999" = mean(c(75000,99999)),
    "$100,000 - $124,99" = mean(c(100000,124999)),
    "$125,000 - $149,999" = mean(c(125000,149999)),
    "$150,000 or more" = 150000
    )

  
  # the minimum number of householders is 1 because the question 
  # was phrased asking participants to include themselves.
  mesa$numhhld5 <- ifelse(mesa$numhhld5 == 0, 1, mesa$numhhld5)

  # mesa$pov : "poverty: % persons age 5 and above below poverty level (B16009)"
  mesa$ct_pov_cat <- with(mesa, case_when(
      pov < .05 ~ '<5%',
      pov >= .05 & pov < .2 ~ '>=5% <20%',
      pov >= .2 ~ '>=20%')) %>% factor(
    levels = c('<5%', '>=5% <20%', '>=20%'))


  mesa$occupation %<>% as.factor %>% 
    recode(
    `1` = "1: HOMEMAKER", 
    `2` = "2: EMPLOYED, FULL TIME", 
    `3` = "3: EMPLOYED, PART TIME",
    `4` = "4: EMPLOYED, ON LEAVE (HEALTH REASONS)", 
    `5` = "5: EMPLOYED, ON LEAVE (NON-HEALTH REASONS)",
    `6` = "6: UNEMPLOYED, < 6 MONTHS", 
    `7` = "7: UNEMPLOYED, > 6 MONTHS", 
    `8` = "8: RETIRED, NOT WORKING",
    `9` = "9: RETIRED, WORKING", 
    `10` = "10: RETIRED, VOLUNTEERING")

  mesa$smkstat5_reformatted <- mesa$smkstat5

  mesa$smkstat5_reformatted %<>% recode(
    "0: NEVER SMOKED" = "No", 
    "1: FORMER SMOKER QUIT MORE THAN 1 YEAR AGO" = "No",
    "2: FORMER SMOKER QUIT LESS THAN 1 YEAR AGO" = "No", 
    "3: CURRENT SMOKER" = "Yes",
    "4: DO NOT KNOW" = "Do not know"
    )

  mesa$smkstat5_reformatted %<>% forcats::fct_relevel("Yes")

  mesa$slpwkhr4_reformatted <- mesa$slpwkhr4

  mesa$slpwkhr4_reformatted %<>% sapply(function(x) {
    if (is.na(x)) {
      return(NA)
    } else if (x < 5) {
      return("Less than 5 hours")
    } else if (x < 6) {
      return("5 to 6 hours")
    } else if (x < 7) {
      return("6 to 7 hours")
    } else if (x >= 7) {
        return("7 or more hours")
    } else return(NA)
  })

  mesa$slpwkhr4_reformatted %<>% as.factor

  mesa$slpwkhr4_reformatted %<>% forcats::fct_relevel(
    "7 or more hours", "6 to 7 hours", '5 to 6 hours', 'Less than 5 hours'
  )

  mds_vars <- c('uf2fire1', 'uf2hire1', 'uf2stop1', 'uf2educ1', 'uf2move1', 'uf2nghb1')
  orig_mds_labels <- 
    lapply( mds_vars, function(x) { attr(mesa[[x]], 'label') })

  mesa %<>% mutate(
    uf2fire1 = ifelse(uf1fire1 == 1, uf2fire1, NA),
    uf2hire1 = ifelse(uf1hire1 == 1, uf2hire1, NA),
    uf2stop1 = ifelse(uf1stop1 == 1, uf2stop1, NA),
    uf2educ1 = ifelse(uf1educ1 == 1, uf2educ1, NA),
    uf2move1 = ifelse(uf1move1 == 1, uf2move1, NA),
    uf2nghb1 = ifelse(uf1nghb1 == 1, uf2nghb1, NA))

  for (iter in 1:length(mds_vars)) {
    attr(mesa[[mds_vars[[iter]]]], 'label') <-
      orig_mds_labels[[iter]]
  }

  # create mds_rd which measures how many components of the 
  # major discrimination scale participants attributed to race or
  # ethnicity based discrimination

  mesa %<>% mutate(

    mds =
      ifelse(! is.na(uf1fire1), uf1fire1, 0) +
      ifelse(! is.na(uf1hire1), uf1hire1, 0) +
      ifelse(! is.na(uf1stop1), uf1stop1, 0) +
      ifelse(! is.na(uf1educ1), uf1educ1, 0) +
      ifelse(! is.na(uf1move1), uf1move1, 0) +
      ifelse(! is.na(uf1nghb1), uf1nghb1, 0),
    mds_rd =
      ifelse(uf2fire1 == 1 & ! is.na(uf2fire1), uf1fire1, 0) +
      ifelse(uf2hire1 == 1 & ! is.na(uf2hire1), uf1hire1, 0) +
      ifelse(uf2stop1 == 1 & ! is.na(uf2stop1), uf1stop1, 0) +
      ifelse(uf2educ1 == 1 & ! is.na(uf2educ1), uf1educ1, 0) +
      ifelse(uf2move1 == 1 & ! is.na(uf2move1), uf1move1, 0) +
      ifelse(uf2nghb1 == 1 & ! is.na(uf2nghb1), uf1nghb1, 0),
    )
  
  # make sure if they provided no data that they are coded as missing
  mesa %<>% rowwise() %>%
    mutate(
      mds_rd = ifelse(all(c(
        is.na(uf1fire1),
        is.na(uf1hire1),
        is.na(uf1stop1),
        is.na(uf1educ1),
        is.na(uf1move1),
        is.na(uf1nghb1)
      )), NA, mds_rd))
  
  mesa %<>% mutate(
    mds_rd_bin = ifelse(mds_rd >= 1, 1, 0))

  # Rescale and construct the MESA eds 6 point and 45 point scales
  mesa_eds_vars <- c('curtesy1', 'respect1', 'service1', 'smart1', 'afraid1', 'dishon1', 'better1', 'insult1', 'threat1')

  mesa_eds_var_labels <- 
    sapply(mesa_eds_vars, function(mesa_var) attr(mesa[[mesa_var]], 'label'))

  mesa %<>% mutate_at(.vars = mesa_eds_vars, function(x) { -1 * (as.integer(x) - 6)})

  for (mesa_var in mesa_eds_vars) {
    attr(mesa[[mesa_var]], 'label') <- mesa_eds_var_labels[[mesa_var]]
  }

  mesa %<>% mutate(
    eds_f = curtesy1 + respect1 + service1 + smart1 + afraid1 + dishon1 + better1 + insult1 + threat1 )

  mesa %<>% mutate(
    eds_s =
      ifelse(curtesy1 >= 2, 1, 0) +
      ifelse(respect1 >= 2, 1, 0) +
      ifelse(service1 >= 2, 1, 0) +
      ifelse(smart1 >= 2, 1, 0) +
      ifelse(afraid1 >= 2, 1, 0) +
      ifelse(dishon1 >= 2, 1, 0) +
      ifelse(better1 >= 2, 1, 0) + 
      ifelse(insult1 >= 2, 1, 0) + 
      ifelse(threat1 >= 2, 1, 0)
      )
  
  mesa %<>% mutate(
    eds_s_matching_mbms =
      ifelse(curtesy1 >= 2, 1, 0) +
      ifelse(respect1 >= 2, 1, 0) +
      ifelse(smart1 >= 2, 1, 0) +
      ifelse(afraid1 >= 2, 1, 0) +
      ifelse(dishon1 >= 2, 1, 0) +
      ifelse(better1 >= 2, 1, 0)
      )


  # create 45 and 6 point scales for everyday discrimination scale
  mesa %<>% mutate(eds_f_rd = eds_f * mds_rd_bin,
    eds_s_rd = eds_s * mds_rd_bin,
    eds_s_rd_matching_mbms = eds_s_matching_mbms * mds_rd_bin,
    )

  # recode situational responses
  mesa$uf1resp1 %<>% as.factor %>% 
    recode(`1` = "Accept it as a fact of life", `2` = "Do something about it")

  mesa$uf2resp1 %<>% as.factor %>% 
    recode(`1` = "Talk to others about it", `2` = "Keep it to yourself")
  
  
  # mesa birth year 
  mesa %<>% mutate(
    birth_year = 2011 - age5c)

  return(mesa)
}


#' Adjust MESA Household Income to 2010 Dollars
#' (Assumes 2011 for survey date)
#' 
adjust_mesa_hh_income_to_2010_dollars <- function(mesa, inflation_df) {
  inflation_df <- load_inflation_df()
  
  mesa %<>% left_join(load_mesa_e5_survey_dates())
  
  mesa %<>% rowwise() %>% mutate(
    hh_income_2010_dollars =
      adjust_to_2010_dollars(
        inflation_df = inflation_df,
        from_year = year5,
        orig_amount = income5_num
      )
  )
  
  return(mesa)
  
}



#' Calculate Household Poverty Based on 2010 Household Income Dollars
#'
add_ratio_to_2010_poverty_line_mesa <- function(mesa) {
  
  pov_thresholds_2010 <- load_poverty_thresholds(year = 2010)
  
  mesa %<>% mutate(
    ratio_to_2010_poverty_line = 
      calculate_poverty_ratio(
        thresholds = pov_thresholds_2010, 
        household_income = hh_income_2010_dollars,
        total_people = numhhld5,
        num_seniors = nhhlde5,
        num_children = nhhldc5 
      )
  )
}


#' Add Percent Measures for American Indian and Alaska Native, Native Hawaiian & Other Pacific Islander, & "Other" 
#' 
merge_in_pct_aian_nhopi_other <- function(mesa) {
  mesa %<>% left_join(load_mesa_ct_race_aian_nhopi_other())
}
