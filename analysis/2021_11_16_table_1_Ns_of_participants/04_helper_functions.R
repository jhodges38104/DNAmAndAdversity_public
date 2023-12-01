
# helper functions for tabulating N (%) and mean (SD) outcomes in 
# MBMS and MESA


# summarize counts --------------------------------------------------------

create_n_counts <- function(df, filter_exp) {
  df %>% filter( {{ filter_exp }} ) %>% 
    nrow()
}

filter_exps <- vars(
  TRUE, 
  black == 1,
  black == 0,
  TRUE,
  us_born == 1,
  us_born == 1 & black == 1,
  us_born == 1 & white == 1,
  us_born == 1 & hispanic == 1,
  us_born == 0,
  us_born == 0 & black == 1,
  us_born == 0 & white == 1,
  us_born == 0 & hispanic == 1)

datasets <- c('mbms', 'mesa') %>% 
  rep(times = c(3, 9))


# create Ns by racial/ethnic and nativity group for MBMS and MESA table 1
create_n_counts_mbms_and_mesa <- function() {
  
  purrr::map_chr(
    1:length(filter_exps),
    function(i) {
      create_n_counts(get(datasets[[i]]), !! filter_exps[[i]])
    }
  )
}

create_n_pct_entries <- function(df, filter_exp, second_filter_exp) {
  
  numerator <- df %>% filter(!! filter_exp) %>% 
    filter(!! second_filter_exp) %>% 
    nrow()
  
  denominator <- df %>% filter(!! filter_exp) %>% filter(not(!! second_filter_exp) | (!! second_filter_exp) ) %>% nrow()
  
  str_c(
      scales::number_format(accuracy = 1)(numerator),
      " (",
      scales::percent_format(accuracy = 0.1)(numerator/denominator),
      ")"
    )
}

# example 
# create_n_pct_entries(mbms, filter_exps[[1]], vars(hs_no_college == 1)[[1]])

create_n_pct_entries_mbms_and_mesa <- function(second_filter_exp) {
  purrr::map_chr(
    1:length(filter_exps),
    ~ create_n_pct_entries(
      df = get(datasets[[.]]),
      filter_exp = filter_exps[[.]],
      second_filter_exp = second_filter_exp)
  )
}


create_missing_n_pct_entries <- function(df, filter_exp, var) {
  
  numerator <- df %>% filter(!! filter_exp) %>% 
    filter(is.na(!! var)) %>% 
    nrow()
  
  denominator <- df %>% filter(!! filter_exp) %>% nrow()
  
  str_c(
    "[",
    scales::number_format(accuracy = 1)(numerator),
    " (",
    scales::percent_format(accuracy = 0.1)(numerator/denominator),
    ")]"
  )
}

create_missing_n_pct_entries_mbms_and_mesa <- function(var) {
  purrr::map_chr(
    1:length(filter_exps),
    ~ create_missing_n_pct_entries(
      df = get(datasets[[.]]),
      filter_exp = filter_exps[[.]],
      var = var)
  )
}




# example 
# create_n_pct_entries_mbms_and_mesa(vars(college_degree == 1)[[1]])

create_mean_sd_entries <- function(df, filter_exp, var, accuracy = 0.1) {
  
  if (! var %>% quo_name() %in% colnames(df)) { 
    str <- 'NA'
  } else { 
    values <- df %>% 
      filter(!! filter_exp) %>% 
      filter(is.finite(!! var)) %>% 
      pull(!! var) 
      
  
      str <- str_c(
        scales::number_format(accuracy = accuracy)(mean(values, na.rm=T)),
        " (",
        scales::number_format(accuracy = accuracy)(sd(values, na.rm=T)),
        ")"
      )
  }
  return(str)
}

# example 
# create_mean_sd_entries(mbms, filter_exp = filter_exps[[1]], var = epigenetic_clock_variables[[1]])

create_mean_sd_entries_mbms_and_mesa <- function(var, accuracy = 0.1) {
  purrr::map_chr(
    1:length(filter_exps),
    ~ create_mean_sd_entries(get(datasets[[.]]), filter_exps[[.]], var, accuracy = accuracy))
}

# example
create_mean_sd_entries_mbms_and_mesa(epigenetic_clock_variables[[1]])

