#' Save the Census Tract Housing Tenure Data
save_mass_census_tract_housing_tenure_data <- function() {

  # below where black/white are used they refer to 
  # non-hispanic black/white.
  variables_of_interest <- tibble::tribble(
  ~varname, ~description,
  "B25003_001E", "total tenure pop est",
  "B25003_002E", "owner occupied pop est",
  "B25003_003E", "renter occupied pop est",
  "B25003A_001E", "white tenure pop est",
  "B25003A_002E", "white owner occupied pop est",
  "B25003A_003E", "white renter occupied pop est",
  "B25003B_001E", "black tenure pop est",
  "B25003B_002E", "black owner occupied pop est",
  "B25003B_003E", "black renter occupied pop est"
  )


  MAacs2015_CT <- get_acs(geography = "tract", state = "MA",
                          variables = 
                          variables_of_interest$varname,
                          year = 2015, geometry = FALSE, output = "wide", 
                          keep_geo_vars = FALSE, moe_level = 95, survey = "acs5")

  MAacs2015_CT %<>% rename(!! setNames(variables_of_interest$varname, variables_of_interest$description))

  MAacs2015_CT %<>% mutate(
    ct_frc_own = `owner occupied pop est` / `total tenure pop est`,
    ct_frc_black_own = `black owner occupied pop est` / `black tenure pop est`,
    ct_ICEown = (`owner occupied pop est` - `renter occupied pop est`) / `total tenure pop est`,
    ct_ICEwbown = (`white owner occupied pop est` - `black renter occupied pop est`) / `total tenure pop est`)

  MAacs2015_CT %<>% select(GEOID, ct_frc_own, ct_frc_black_own, ct_ICEown, ct_ICEwbown)

  write.csv(MAacs2015_CT, file.path(system.file('mbms', package="DNAm"), "housing_tenure_ct.csv"), row.names=F)
}
