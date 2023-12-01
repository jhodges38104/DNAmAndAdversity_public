
# create and apply helper functions that create "mean (sd)" strings for each
# clock by race/ethnicity & nativity stratified subpopulations of MBMS and MESA

# define subpopulations:
# 3 for MBMS and 9 for MESA: 
# MBMS: 
#   1 Total
#   2 Black NH 
#   2 White NH 
# MESA 
#   1 Total
#   2 US Born Total 
#   3 US Born Black NH 
#   4 US Born White NH 
#   5 US Born Hispanic
#   6 Non-US Born Total
#   7 Non-US Born Black NH 
#   8 Non-US Born White NH 
#   9 Non-US Born Hispanic 

datasets <- c(rep('mbms', 3), rep('mesa', 9))

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
  us_born == 0 & hispanic == 1
)

# calculate the mean and standard deviation of an epigenetic clock from the
# dataset given filter criteria and whether or not to adjust
calculate_mean_sd <- function(df, clock_var, filter_exp, adjust = F) {
  
  # if adjust, get clock_var_adjusted values
  if (adjust) {
    clock_var2 <- clock_var %>% quo_name() %>% str_c('_adjusted') %>% 
      vars() %>% `[[`(1)
    
    clock_values <- df %>% 
      filter(!! filter_exp) %>% 
      pull(!!clock_var2)
    
  # get clock_var values
  } else {
    clock_values <- df %>% 
      filter( !! filter_exp ) %>% 
      pull(!! clock_var)
  }
  
  # summarize into mean (sd) string
  str_c(
    scales::number_format(accuracy = 0.01)(mean(clock_values, na.rm=T)),
    " (",
    scales::number_format(accuracy = 0.01)(sd(clock_values, na.rm=T)),
    ")"
  )
}
  
# example 
# calculate_mean_sd(mbms, clock_var = epigenetic_clock_variables[[1]], 
#                   filter_exp = filter_exps[[1]])


# wrapper function for calculate_mean_sd which constructs the 
# mean (sd) string for each of the MBMS and MESA subpopulations of interest 
# (12) total, given by the datasets and filter_exps vectors
calculate_row_of_means_and_sds <- function(clock_var, adjust = F) {
  
  purrr::map_chr(1:length(filter_exps), 
  function(i) {
    calculate_mean_sd(df = get(datasets[[i]]), 
                      clock_var = clock_var,
                      filter_exp = filter_exps[[i]],
                      adjust = adjust)
  })
}
  
# insert names to the epigenetic clock variables vector so that 
# purrr knows how to name columns
names(epigenetic_clock_variables) <- as.character(epigenetic_clock_variables) %>% str_remove_all("~")

# create a data frame of epigenetic clocks summarized by mean (sd) for 
# each MBMS and MESA subpopulation
epi_clocks <- purrr::map_df(
  epigenetic_clock_variables,
  calculate_row_of_means_and_sds) %>% 
  t.data.frame() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column('clock')

# specify these are un-adjusted clocks
epi_clocks$adjusted <- factor('raw', levels = c('raw', 'adjusted'))

# calculate age adjusted epigenetic clocks summaries, mean (sd), for 
# the 12 MBMS and MESA race/ethnicity & nativity stratified subpopulations 
epi_clocks_adjusted <- purrr::map_df(
  epigenetic_clock_variables,
  calculate_row_of_means_and_sds,
  adjust = T) %>% 
  t.data.frame() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column('clock')

# specify that these are adjusted results
epi_clocks_adjusted$adjusted <- factor('adjusted', levels = c('raw', 'adjusted'))

# combine the un-adjusted and adjusted clocks
epi_clocks %<>% bind_rows(epi_clocks_adjusted)

# re-order columns to have clock, then adjusted, then the rest as before
epi_clocks %<>% select(clock, adjusted, everything())

# use more readable names for columns -- this matches with datasets and filter_exps
colnames(epi_clocks)[3:14] <- 
  c('MBMS Total', 'MBMS Black NH', 'MBMS White NH',
    'MESA Total', 'MESA US Total', 'MESA USB Black NH', 'MESA USB White NH', 'MESA USB Hispanic',
    'MESA NUSB Total', 'MESA NUSB Black NH', 'MESA NUSB White NH', 'MESA NUSB Hispanic')

# factor epigenetic clocks (with levels ordered) 
epi_clocks$clock %<>% factor(levels = c(
  'age.hannum',
  'age.horvath',
  'zhang_clock',
  'phenoage',
  'dunedin_age',
  'zhang.mortality',
  'DNAmGrimAge',
  'miage.clock',
  'epiToc.clock',
  'dnamtl'
))

# re-order by clocks and then adjusted status
epi_clocks %<>% arrange(adjusted, clock)


names(cell_type_variables) <- as.character(cell_type_variables) %>% str_remove_all("~")

# cell type proportions 
cell_type_proportions <- purrr::map_df(
  cell_type_variables,
  calculate_row_of_means_and_sds) %>% 
  t.data.frame() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column('cell_type')

