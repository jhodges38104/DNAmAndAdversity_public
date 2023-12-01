
# fill in table details
table1[5, 2:13] <- create_n_counts_mbms_and_mesa()

# average age
table1[7, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(age)[[1]])
table1[8, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(age)[[1]])

# number female
table1[10, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(male == 0)[[1]])
# number male
table1[11, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(male == 1)[[1]])
# number missing 
table1[12, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(male)[[1]])


# household size 
table1[13, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(household_size)[[1]])
table1[14, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(household_size)[[1]])

# number of children 
table1[15, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(household_children)[[1]])
table1[16, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(household_children)[[1]])

# born in jim crow state 
table1[19, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(jim_crow == 1)[[1]])
table1[20, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(jim_crow)[[1]])

# racialized economic segregation at time of birth 
table1[22, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(birth_ice_race)[[1]])
table1[23, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(birth_ice_race)[[1]])

# state policy liberalism 
table1[24, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(birth_state_liberalism)[[1]])
table1[25, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(birth_state_liberalism)[[1]])


# parents highest education 
table1[27, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(parents_no_hs == 1)[[1]])
table1[28, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(parents_hs_no_college == 1)[[1]])
table1[29, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(parents_college_degree == 1)[[1]])
table1[30, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(parents_college_degree)[[1]])

# participants highest education 
table1[32, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(no_hs_degree == 1)[[1]])
table1[33, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(hs_no_college == 1)[[1]])
table1[34, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(college_degree == 1)[[1]])
table1[35, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(college_degree)[[1]])


# self reported measures of discrimination 
# eod 
# table1[28, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(eod_racial_small_scale)[[1]])
# 
# this line should be the == 0 group 
table1[38, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(eod_racial_small_scale == 0)[[1]])
table1[39, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(eod_racial_small_scale %in% c(1,2))[[1]])
table1[40, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(eod_racial_small_scale >= 3)[[1]])
table1[41, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(eod_racial_small_scale)[[1]])



# 
# mds
table1[43, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(mds_racial_small_scale == 0)[[1]])
table1[44, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(mds_racial_small_scale %in% c(1,2))[[1]])
table1[45, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(mds_racial_small_scale >= 3)[[1]])
table1[46, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(mds_racial_small_scale)[[1]])


# household income in 2010 dollars
table1[48, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(hh_income)[[1]])
table1[49, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(hh_income)[[1]])
table1[50, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(hh_income_per_capita)[[1]])
table1[51, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(hh_income_per_capita)[[1]])
table1[52, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(poverty_ratio)[[1]])
table1[53, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(poverty_ratio)[[1]])

# households below poverty line 
table1[54, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(poverty_ratio < 1)[[1]])
table1[55, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(poverty_ratio < 1)[[1]])


# occupational class - employed 
table1[57, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(unemployed == 0)[[1]])
table1[58, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(unemployed == 0 & supervisory_employee == 0)[[1]])
table1[59, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(supervisory_employee == 1)[[1]])
table1[60, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(supervisory_employee)[[1]])

table1[61, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(unemployed == 1)[[1]])
table1[62, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(unemployed)[[1]])

# housing tenure
table1[64, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(paying_mortgage == 1)[[1]])
table1[65, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(home_owner_free_and_clear == 1)[[1]])
table1[66, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(renter == 1)[[1]])
table1[67, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(ifelse(! (renter | home_owner_free_and_clear | paying_mortgage), NA, 1))[[1]])

# air pollution 
table1[69, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(black_carbon)[[1]])
table1[70, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(black_carbon)[[1]])
table1[71, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(light_absorption)[[1]])
table1[72, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(light_absorption)[[1]])
table1[73, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(pollution_proximity)[[1]])
table1[74, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(pollution_proximity)[[1]])
table1[75, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(nitrous_oxides)[[1]])
table1[76, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(nitrous_oxides)[[1]])


# census tract composition 
# pct white 
table1[79, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ct_pct_white)[[1]])
table1[80, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ct_pct_black)[[1]])
table1[81, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ct_pct_hispanic)[[1]])
table1[82, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ct_pct_asian)[[1]])
table1[83, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ct_pct_aian)[[1]])
table1[84, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ct_pct_nhopi)[[1]])
table1[85, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(ct_pct_black)[[1]])
# table1[62, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ct_pct_aian)[[1]])


# ice measures
table1[87, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ICEinc)[[1]], accuracy = 0.01)
table1[88, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(ICEinc)[[1]])
table1[89, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ICErace)[[1]], accuracy = 0.01)
table1[90, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(ICErace)[[1]])
table1[91, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ICEraceinc)[[1]], accuracy = 0.01)
table1[92, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(ICEraceinc)[[1]])
table1[93, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(ICEown)[[1]], accuracy = 0.01)
table1[94, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(ICEown)[[1]])

# smoking measures
table1[95, 1] <- "Current Smokers"
table1[96, 1] <- "[Missing: N (%)]"
table1[95, 2:13] <- create_n_pct_entries_mbms_and_mesa(vars(current_smoker == 1)[[1]])
table1[96, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(current_smoker)[[1]])

# BMI measures
table1[97, 1] <- "BMI"
table1[98, 1] <- "[Missing: N (%)]"
table1[97, 2:13] <- create_mean_sd_entries_mbms_and_mesa(vars(bmi)[[1]], accuracy = 0.01)
table1[98, 2:13] <- create_missing_n_pct_entries_mbms_and_mesa(vars(bmi)[[1]])


