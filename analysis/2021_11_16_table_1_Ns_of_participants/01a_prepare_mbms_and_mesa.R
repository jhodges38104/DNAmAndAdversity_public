
# add NA empty columns to MBMS / MESA as needed to make calculating rows of data for
# MBMS (all), MBMS black, MBMS white, MESA (all), MESA (usb), ... easier --
#
# by having the same variables present in both datasets, even if all NAs in one,
# we don't have to worry later when we reference data[variable] where data 
# is an argument (one of mbms or mesa) and variable is also an argument, namely 
# one of our exposures, clocks, or other measures of interest.
# 

mesa$eod_racial_small_scale <- NA
mbms$mds_racial_small_scale <- NA

mesa$supervisory_employee <- NA
mesa$birth_ice_race <- NA
mesa$black_carbon <- NA
mbms$light_absorption <- NA
mesa$pollution_proximity <- NA
mbms$nitrous_oxides <- NA

# put percent scale variables on the percent scale

mbms %<>% mutate(across(starts_with("ct_pct"), ~ multiply_by(., 100)))
mesa %<>% mutate(across(starts_with("ct_pct"), ~ multiply_by(., 100)))

# fix smoking variable 
mbms$current_smoker <- ifelse(mbms$never_smoker == 1, 0, mbms$current_smoker)
# mbms$current_smoker <- ifelse(mbms$smoke_last_8hrs == 1, 1, mbms$current_smoker)
# mbms$current_smoker <- ifelse(mbms$smoke_not_in_8hrs == 1, 1, mbms$current_smoker)
# mbms$current_smoker <- ifelse(mbms$ex_smoker == 1, 0, mbms$current_smoker)
