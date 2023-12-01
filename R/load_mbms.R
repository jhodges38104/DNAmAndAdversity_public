

#' Read in the MBMS CSV Respose Dataset
#' 
#' @importFrom readr read_csv
#' 
load_mbms_csv <- function() {
  readr::read_csv(system.file('mbms/master_analytic_v10_01.24.20.csv', package='DNAm'), show_col_types = FALSE)
}

#' Read in MBMS SAS Response Dataset
#'
#' @importFrom haven read_sas
load_mbms_sas <- function() {
  haven::read_sas(
    system.file('mbms/master_analytic_nov15_2011.sas7bdat',
    package='DNAm'))
}


#' Load MBMS Census Tract Measures
#'
load_mbms_census_tract_measures <- function() {
  readr::read_csv(
    system.file(
      'mbms/21_mepigen_CTmeasures.csv',
      package='DNAm'
    ),
    show_col_types = FALSE
  )
}

#' Merge MBMS and Census Tract Measures
#'
merge_mbms_and_census_tract_measures <- function(mbms, census_tract_measures) {
  merge(
    mbms,
    census_tract_measures,
    by.x = 'GEOID10',
    by.y = 'geoid', 
    all.x=T)
}

#' Load MBMS Air Pollution Data
load_mbms_air_pollution_data <- function() {
  haven::read_sas(
    system.file(
      'mbms/bc_mbms_dna.sas7bdat',
      package='DNAm'
    )
  )
}

#' Merge MBMS Census Tract Housing Tenure Data
merge_mbms_ct_housing_tenure <- function(mbms) {

  housing_tenure <- readr::read_csv(system.file('mbms/housing_tenure_ct.csv', package="DNAm"), show_col_types = FALSE)

  mbms %>% merge(housing_tenure, by.x = "GEOID10", by.y = "GEOID", all.x=T) 
}


#' Load MBMS Epigenetic QC Data
load_mbms_epi_qc_df <- function() {
  readr::read_csv(system.file("/mbms/epi_qc/mbms_qc_quality.csv", package='DNAm'), show_col_types = FALSE)
}
