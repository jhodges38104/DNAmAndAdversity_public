
#' Load Consumer Price Index Inflation
#' 
#' @source https://data.bls.gov/timeseries/CUUR0000SA0
#' 
#' US city average inflation based on the referent point 
#' 1982-84=100. 
#' 
#' Columns are year, jan,...,dec, half1, half2
#' Downloaded November 30 2021
load_inflation_df <- function() {
  
  inflation_file <- system.file(
    'inflation/SeriesReport-20211130134134_3bd6fc.xlsx',
    package = 'DNAm'
  )
  df <- readxl::read_excel(inflation_file, skip = 11)
  
  df %<>% janitor::clean_names()
  df
}


#' Adjust Dollars to 2010 Dollars 
#' 
#' @param inflation_df the data frame rendered by `load_inflation_df`
#' 
#' @examples 
#' inflation_df <- load_inflation_df()
#' 
#' # observe inflation going forward in time
#' adjust_to_2010_dollars(inflation_df, 2009, 100) 
#' 
#' # observe deflation going back in time
#' adjust_to_2010_dollars(inflation_df, 2011, 100) 
#' adjust_to_2010_dollars(inflation_df, 2010, 100) 
#' adjust_to_2010_dollars(inflation_df, 2008, 100000) 
#' adjust_to_2010_dollars(inflation_df, 2008, 100000) 
#' adjust_to_2010_dollars(inflation_df, 2009, 100000) 
#' adjust_to_2010_dollars(inflation_df, 2009, 100000) 
#' 
adjust_to_2010_dollars <- function(inflation_df, from_year, orig_amount) {
  
  from_year_cpi <- inflation_df %>% filter(year == {{ from_year }}) %>% 
    pull(annual)
  
  cpi_2010 <- inflation_df %>% filter(year == 2010) %>% pull(annual)
  
  quotient_factor <- from_year_cpi / cpi_2010
  
  amount_in_2010_dollars <- orig_amount / quotient_factor
  
  return(amount_in_2010_dollars)
}
