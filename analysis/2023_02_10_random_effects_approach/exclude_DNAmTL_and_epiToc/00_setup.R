# dependencies
library(lme4)
library(broom)
library(broom.mixed)
library(here)
library(doParallel)
devtools::load_all(".")

# setup -------------------------------------------------------------------

analysis_dir <- here("analysis/2023_02_10_random_effects_approach/")

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

# setup for parallel processing
registerDoParallel()