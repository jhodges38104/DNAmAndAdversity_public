# munge into a dataframe --------------------------------------------------

model_results <- c(mbms_model_results, mesa_model_results)
model_results %<>% bind_rows()

# clean term slightly by removing the prefixing race on all effects
model_results$term %<>% stringr::str_remove_all("^race")

# set factor levels in model results
model_results$model %<>% factor(
  levels = c(
    'analysis_i_no_svs',
    'analysis_i_with_svs',
    'analysis_ii_no_svs',
    'analysis_ii_with_svs'
  )
)

model_results %<>% separate(
  term,
  into = c('race', 'term'),
  sep = ':'
)

# reverse results for DNAmTL
model_results %<>% mutate(across(where(is.numeric), ~ ifelse(clock == 'dnamtl',-1 * ., .)))

# remove white non-Hispanic jim crow effects in MBMS
model_results %<>% filter(
  ! (race == 'white non-hispanic' & dataset == 'mbms' & term == 'jim_crowTRUE')
)


# clean clock names -------------------------------------------------------------

clock_order <- c('Horvath (1st)', 'Hannum (1st)', 'epiToc (1st)', 'Zhang Age (1st)',
                 'MiAge (1st)', 'DNAmTL (1st)', 'Zhang Mortality (2nd)',
                 'PhenoAge (2nd)', 'DunedinPoAm (2nd)', 'GrimAge (2nd)')

model_results$clock %<>% factor(
  levels = c('age.horvath', 'age.hannum', 'epiToc.clock', 'zhang_clock',
             'miage.clock', 'dnamtl', 'zhang.mortality',
             'phenoage', 'dunedin_age', 'DNAmGrimAge'),
  labels = clock_order
)

# add clock generation indicator column
model_results %<>% mutate(clock_generation = ifelse(
  clock %in% c(
    'Horvath (1st)',
    'Hannum (1st)',
    'epiToc (1st)',
    'Zhang Age (1st)',
    'MiAge (1st)',
    'DNAmTL (1st)'
  ),
  '1st',
  '2nd'
))


# combine approaches ------------------------------------------------------

mbms_re_model_results %<>% mutate(dataset = 'mbms')
mesa_re_model_results %<>% mutate(dataset = 'mesa')

re_model_results <- bind_rows(mbms_re_model_results, mesa_re_model_results)

re_model_results %<>% mutate(
  estimate = (high + low)/2, 
  model = 'analysis_ii_with_svs') %>% 
  rename(
    conf.low = low,
    conf.high = high)

re_model_results %<>% filter( ! (term == 'jim_crowTRUE' & race == 'White Non-Hispanic' & dataset == 'mbms'))

analysis_ii_with_svs <- 
  model_results %>% 
  filter(model == 'analysis_ii_with_svs') %>% 
  bind_rows(re_model_results)

# clean exposure names ----------------------------------------------------

exposure_namer <- setNames(
  nm = exposure_variables_named_chr,
  object = names(exposure_variables_named_chr))

# analysis_ii_with_svs$term %<>% stringr::str_remove("TRUE")
analysis_ii_with_svs %<>% mutate(exposure_name = exposure_namer[term])
analysis_ii_with_svs %<>% mutate(exposure_name = factor(exposure_name, levels = exposure_namer))


# clean clock names -------------------------------------------------------------

analysis_ii_with_svs %<>% mutate(clock = factor(
  clock,
  levels = c(
    "Horvath (1st)",
    "Hannum (1st)",
    "epiToc (1st)",
    "Zhang Age (1st)",
    "MiAge (1st)",
    "DNAmTL (1st)",
    "1st Pooled",
    "Zhang Mortality (2nd)",
    "PhenoAge (2nd)",
    "DunedinPoAm (2nd)",
    "GrimAge (2nd)",
    "2nd Pooled",
    "1st and 2nd Pooled"
  ),
  labels = c(
    "Horvath (1st)",
    "Hannum (1st)",
    "epiToc (1st)",
    "Zhang Age (1st)",
    "MiAge (1st)",
    "DNAmTL (1st)",
    "1st Pooled (excl. DNAmTL and MiAge)",
    "Zhang Mortality (2nd)",
    "PhenoAge (2nd)",
    "DunedinPoAm (2nd)",
    "GrimAge (2nd)",
    "2nd Pooled",
    "1st and 2nd Pooled"
  )
))
