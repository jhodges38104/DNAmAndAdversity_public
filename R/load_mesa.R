
#' Read in the MESA CSV Response Dataset
#' 
#' @importFrom readr read_csv
#' 
load_mesa_csv <- function() {
  readr::read_csv(system.file('mesa/MESA responses.csv', package='DNAm'), show_col_types = FALSE)
}



#' Read MESA SAS Format 
#'
#' @importFrom haven read_sas
load_mesa_sas <- function() {
  read_sas(
    data_file = system.file("mesa/mesa_krieger_08172020.sas7bdat", package='DNAm'),
    catalog_file = system.file("mesa/formats.sas7bcat", package='DNAm'))
}



#' Load MESA Additional Race Data 
#' 
load_mesa_ct_race_aian_nhopi_other <- function() {
  haven::read_sas(data_file = system.file("mesa/exrace_mesa_krieger_01122022.sas7bdat", package = "DNAm"))
}