
#' Load State Policy Liberalism Results
#' 
#' @source Caughey, Devin; Warshaw, Christopher, 2014, "The Dynamics of State
#' Policy Liberalism, 1936-2014", http://dx.doi.org/10.7910/DVN/ZXZMJB
#' Dataverse [Distributor] V1 [Version].
#' https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/ZXZMJB
#'   

#' @export
load_state_policy_liberalism_index <- function() {
  readr::read_csv(
    system.file(
      'mbms/Warshaw_Caughey_2015_statepolicyliberalism/state_policy_idealpoints-all.csv',
      package = 'DNAm'
    ),
    show_col_types = F
  )
}


#' Merge State Policy Liberalism into MBMS 
#' 
#' @export
#' @examples 
#' 
#' devtools::load_all()
#' 
#' # this is as in from the beginning of make_clean_mbms()
#' mbms <- make_mbms_sas_csv_merged()
#' mbms <- make_factored_mbms(mbms)
#' mbms <- relevel_mbms(mbms)
#' ct_measures <- load_mbms_census_tract_measures()
#' mbms <- merge_mbms_and_census_tract_measures(mbms, ct_measures)
#' mbms <- merge(mbms, load_mbms_air_pollution_data(), all.x=T)
#' mbms %<>% add_place_of_birth_absms()
#' mbms %<>% merge_mapc_into_mbms()
#' mbms %<>% merge_mbms_ct_housing_tenure()
#' mbms %<>% reformat_mbms_for_comparison()
#' mbms %<>% merge_state_policy_liberalism_into_mbms()
#'
merge_state_policy_liberalism_into_mbms <- function(mbms) {
  
  mbms %<>% mutate(
    BirthDate = lubridate::ymd(BirthDate),
    BirthYear = lubridate::year(BirthDate)
  )
  
  mbms %<>% mutate(
    SurveyDate = lubridate::ymd(SurveyDate),
    SurveyYear = lubridate::year(SurveyDate),
    SurveyMonth = lubridate::month(SurveyDate))
  
  mbms %<>% mutate(
    BirthYear = ifelse(is.na(BirthYear),
                       SurveyYear - AgeYRS,
                       BirthYear)
  )
  
  policy_liberalism <- load_state_policy_liberalism_index()
  
  policy_liberalism %<>% adjust_liberalism_to_include_prior_years()
  
  mbms %<>% left_join(
    policy_liberalism %>% select(median, year, abb) %>% 
      rename(liberalism = median, state_abb = abb),
    by = c('StateOB_abb' = 'state_abb', 'BirthYear' = 'year'))
  
  return(mbms)
}



#' Merge State Policy Liberalism into MESA 
#' @export
#' 
#' @examples 
#' 
#' devtools::load_all()
#' # load raw data
#' mesa <- load_mesa_sas()
#' 
#' # make factored mesa data 
#' mesa <- make_factored_mesa(mesa)
#' 
#' # reformat for comparison
#' mesa <- reformat_mesa_for_comparison(mesa)
#' 
#' # add jim crow states
#' mesa$JC <- mesa$stbth1 %in% jim_crow_states
#' 
#' # add us regions
#' mesa$birth_region <- regions_lookup[mesa$stbth1]
#' 
#' mesa %<>% merge_state_policy_liberalism_into_mesa()
merge_state_policy_liberalism_into_mesa <- function(mesa) {
  
  policy_liberalism <- load_state_policy_liberalism_index()
  
  policy_liberalism %<>% adjust_liberalism_to_include_prior_years()
  
  mesa %<>% left_join(
    policy_liberalism %>% select(median, year, abb) %>% 
      rename(liberalism = median, state_abb = abb),
    by = c('stbth1' = 'state_abb', 'birth_year' = 'year'))
  
  return(mesa)
}


#' Adjust Liberalism Data to use 10-Year Avg in Period Prior to Study
#' 
#' @examples 
#' liberalism <- load_state_policy_liberalism_index()
#' 
#' ggplot(liberalism, aes(x = year, y = median, color = abb)) +
#'   geom_point() +
#'   geom_line() + 
#'   facet_wrap(~abb %in% jim_crow_states)
#'   
#'   
#' liberalism %<>% adjust_liberalism_to_include_prior_years()
#' 
#' 
#' ggplot(liberalism, aes(x = year, y = median, color = abb)) +
#'   geom_point() +
#'   geom_line() + 
#'   facet_wrap(~abb %in% jim_crow_states)
#'   
#' library(plotly)
#' ggplotly()
#' 
adjust_liberalism_to_include_prior_years <- function(liberalism) {

  liberalism_first_decade_avg <-
    liberalism %>%
      select(-1) %>%
      filter(! abb %in% c("AK", "HI")) %>% 
      group_by(abb) %>%
      filter(! is.na(median)) %>%
      slice_min(order_by = year, n = 10) %>%
      summarize(mean = mean(.data$median))

  new_rows <- expand.grid(
    abb = setdiff(unique(liberalism$abb), c("AK", "HI")),
    year = 1918:1934)
  
  liberalism %<>% bind_rows(new_rows)

  liberalism %<>%
    left_join(liberalism_first_decade_avg %>%
                rename(first_decade_avg = mean), by = 'abb')

  liberalism %<>% mutate(
    median = ifelse(is.na(median), first_decade_avg, median))

  return(liberalism)
}
