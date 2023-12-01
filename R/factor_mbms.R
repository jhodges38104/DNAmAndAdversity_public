#'  Return a Factored Version of the MBMS Dataset
#'
#' @import dplyr magrittr
make_factored_mbms <- function(mbms) {
  
  # code relationship 
  mbms$relationship %<>% factor(., labels = c(
    "Married",
    "Divorced",
    "Separated",
    "Widowed",
    "Living w/ partner, not married",
    "In a relationship, not living w/ partner"
  ))

  # mother born in the US 
  mbms$mother_usborn %<>% factor(., labels = c("No", "Yes"))

  # father born in the US 
  mbms$father_usborn %<>% factor(., labels = c("No", "Yes"))

  # mother education level 
  mbms$Mother_Ed %<>% factor(., labels = c(
    "Less than 12th grade",
    "High school degree",
    "GED",
    "Some college",
    "Vocational school",
    "4 years of college",
    "Graduate degree",
    "I don't know"
  ))

  # father education level 
  mbms$Father_Ed %<>% factor(., labels = c(
    "Less than 12th grade",
    "High school degree",
    "GED",
    "Some college",
    "Vocational school",
    "4 years of college",
    "Graduate degree",
    "I don't know"
  ))
  
  # participant education level 
  mbms$Your_Ed %<>% factor(., labels = c(
    "Less than 12th grade",
    "High school degree",
    "GED",
    "Some college",
    "Vocational school",
    "4 years of college",
    "Graduate degree"
  ))
  
  mbms$educ_p %<>% factor(., labels = c(
    `1` = '< HS',
    `2` = '>= HS and <4 yrs college',
    `3` = '4+ yrs college'
  ))
  
  mbms$House_owner %<>% factor(., labels = c(
    "Owned by you or someone in this household with a mortagege or loan",
    "Owned by you or someone in this household free and clear",
    "Rented for cash rent",
    "Occupied without payment of cash rent"
  ))

  # convert race black/white to factor
  mbms$race %<>% factor(., labels = c(`1` = 'Black N.H.', `2` = 'White N.H.'))
  # mbms$race %<>% forcats::fct_relevel('White N.H.')

  # apply gender
  mbms$gender %<>% factor(., labels = c('Male', 'Female'))

  # BMI categories
  mbms$bmi_cat %<>% factor(., labels = c("BMI < 18.5 = UNDERWEIGHT",
      "BMI  18.5 TO 24.9  = HEALTHY WEIGHT",
      "BMI 25 TO 25.9  =  MODERATELY OVERWEIGHT",
      "BMI 25 TO 25.9  =  OVERWEIGHT"))

  # occupational class
  mbms$occ_class_r %<>% factor(., labels = c("1 = owner, self-employed, supervisor",
      "2 = non-supervisory employee",
      "3 = unemployed, not in pail labor force, other"))
  
  mbms$occ_class_r %<>% recode(
    "3 = unemployed, not in pail labor force, other" = "3 = unemployed, not in paid labor force, other")

  # current smoker
  mbms$sm_current %<>% factor(., labels = c( "No", "Yes"))

  # physical activity level
  mbms$phys_act %<>% factor(., labels = c("1 = low",
      "2 = moderate",
      "3 = high"))


  # education levels
  mbms$educ_r %<>% factor(.,
    labels = c("< HS",
      ">=HS and <4 yrs college",
      "4+ yrs college")) # %>% relevel("4+ yrs college")

  # dibetes
  mbms$Diabetes %<>% factor(.,
    labels = c("Yes", "No")) 

  # diabetes medication
  mbms$med_diab_new %<>% factor(labels = c("No", "Yes"))

  # hypertension 
  mbms$intHypertension %<>% factor(.,
    labels = c("No", "Yes", "Don't Know", "Borderline")) 

  # hypertension medication
  mbms$med_bp_new %<>% factor(labels = c("No", "Yes"))

  # factor income
  mbms$inc1 %<>% as.factor()

  # smoke (ever smoked >=100 cigarettes)
  mbms$Smoke %<>% factor(.,
    labels = c("Yes", "No"))

  # smoking
  mbms$Smoke_now %<>% factor(.,
    labels = c("Yes, every day", "Yes, some days", "No, not at all"))

  # sleep
  mbms$Amount_of_sleep %<>% factor(.,
    labels = c("7 or more hours",	"6 to 7 hours",	"5 to 6 hours",	"Less than 5 hours")
    )

  # metabolic syndrome 
  mbms$metab_d %<>% factor(., labels = c("No", "Yes"))

  # accept/do something
  mbms$Unfr[mbms$Unfr == ""] <- NA
  mbms$Unfr2[mbms$Unfr2 == ""] <- NA
  mbms$Unfr %<>% factor(labels = c(`1` = "Accept it", `2` = "Do something"))
  mbms$Unfr2 %<>% factor(labels = c(`1` = "Talk", `2` = "Keep to myself")) 


  return(mbms)
}


#' Relevel Factors
relevel_mbms <- function(mbms) {
  mbms$gender %<>% forcats::fct_relevel("Female")
  return(mbms)
}
