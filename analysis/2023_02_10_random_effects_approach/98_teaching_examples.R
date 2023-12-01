
library(here)

source(here("analysis/2023_02_10_random_effects_approach/00_setup.R"))
source(here("analysis/2023_02_10_random_effects_approach/01_helper_variables.R"))


# let's do some simple teaching examples 

# for our simplest models (not pooling)
# we might pick one exposure: jim_crow 

summary(lm(age.hannum ~ age + jim_crow, data = mbms))

# now we can add race interaction using our fancy "/" interaction
# term so that we don't have to do any adding up of terms (which
# would be annoying when we wanted confidence intervals)
summary(lm(age.hannum ~ age + race / jim_crow, data = mbms))

# so we can include additional variables now like smoking, bmi, sex/gender, 
# cell type proportions
summary(lm(age.hannum ~ age + race / jim_crow + sex_gender + bmi + smoking + 
             Bcell + CD4T + CD8T + Eos + Mono + Neu + NK, data = mbms))


# since we're including bmi and smoking, we're using "analysis2" here:
svs_for_jim_crow <- readr::read_csv(
  system.file(
    'surrogate_variables/mbms/jim_crow_birth_state_mbms_svs_analysis2.csv',
    package = 'DNAm'
  ),
  show_col_types = FALSE
)


colnames(svs_for_jim_crow)[1] <- 'id'

svs_for_jim_crow$id <- sentrix_to_mbms_id_converter[svs_for_jim_crow$id]
svs_for_jim_crow$id %<>% as.character() # cast IDs to character vector

mbms_w_svs <- left_join(mbms, svs_for_jim_crow[,1:6], by = c('idno' = 'id'))

# now we can add surrogate variables
summary(lm(age.hannum ~ age + race / jim_crow + sex_gender + bmi + smoking + 
             Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + 
             sv1 + sv2 + sv3 + sv4 + sv5, data = mbms_w_svs))

lm(age.hannum ~ age + race / jim_crow + sex_gender + bmi + smoking + 
             Bcell + CD4T + CD8T + Eos + Mono + Neu + NK + 
             sv1 + sv2 + sv3 + sv4 + sv5, data = mbms_w_svs) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  print(n = 30)
