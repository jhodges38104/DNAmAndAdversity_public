# dependencies
library(lme4)
library(broom)
library(broom.mixed)
library(mice)
library(here)
devtools::load_all(".")

# setup -------------------------------------------------------------------

mbms <- make_imputation_ready_mbms()
mbms %<>% filter(epi_final_sample == 1)
mesa <- make_imputation_ready_mesa()

# cleaning
mesa$us_born %<>% as.logical()
mbms$jim_crow %<>% as.logical()
mbms$idno %<>% as.character()
mesa$idno %<>% as.character()

# store original datasets
mbms_orig <- mbms
mesa_orig <- mesa

# standardize mbms
# mbms %<>% effectsize::standardize()

mesa_usborn <- mesa %>% filter(us_born)

early_life_exposures_mesa <- c(
  'jim_crow',
  'birth_state_conservatism',
  'parents_educ',
  'educ'
)

zscore <- function(x, na.rm=T) {
  ((x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm))
}

# standardize
mbms %<>% mutate(across(where(is.numeric), zscore))
mesa %<>% mutate(across(where(is.numeric), zscore))
mesa_usborn %<>% mutate(across(where(is.numeric), zscore))


# make sure that DNAmTL is on the opposite scale from the other clocks since 
# telomere length decreases with age 
mbms$dnamtl %<>% multiply_by(-1)
mesa_usborn$dnamtl %<>% multiply_by(-1)


# one modification to the variables proposed by Nancy was to collapse the education 
# variables to <4 years and 4+ years of college 
mbms %<>% mutate(
  parents_educ = case_when(
    parents_educ == 'parents_college_degree' ~ 'parents_4yr+_college_degree',
    parents_educ %in% c('parents_hs_no_college', 'parents_no_hs') ~ 'parents_lt_4yr_college_degree'),
  educ = case_when(
    educ == 'college_degree' ~ '4yr+_college_degree',
    educ %in% c('hs_no_college', 'no_hs_degree') ~ 'lt_4yr_college_degree')
)
  
mesa_usborn %<>% mutate(
  parents_educ = case_when(
    parents_educ == 'parents_college_degree' ~ 'parents_4yr+_college_degree',
    parents_educ %in% c('parents_hs_no_college', 'parents_no_hs') ~ 'parents_lt_4yr_college_degree'),
  educ = case_when(
    educ == 'college_degree' ~ '4yr+_college_degree',
    educ %in% c('hs_no_college', 'no_hs_degree') ~ 'lt_4yr_college_degree')
)
  