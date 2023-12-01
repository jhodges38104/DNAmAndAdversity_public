
#' Load MESA Survey Dates
#' @export 
#' @examples 
#' mesa_e5_survey_dates <- load_mesa_e5_survey_dates()
load_mesa_e5_survey_dates <- function() {
  haven::read_sas(
    data_file = system.file("mesa/MESAe5_MonthYear_20211201/MESAe5_MonthYear_20211201.sas7bdat", package='DNAm'))
}