

test_that("breakdown of participants is correct in MBMS", {
  
  mbms <- make_clean_mbms()
  
  # 1264 total
  testthat::expect_equal(
    nrow(mbms),
    1005)
  
  # female
  testthat::expect_equal(
    mbms %>% filter(gender == 'Female') %>% nrow(),
    665)
  
  # male count
  testthat::expect_equal(
    mbms %>% filter(gender == 'Male') %>% nrow(),
    340)
  
  # black non-hispanic count 
  testthat::expect_equal(
    mbms %>% filter(race == 'Black N.H.') %>% nrow(),
    504)
  
  # white non-hispanic count 
  testthat::expect_equal(
    mbms %>% filter(race == 'White N.H.') %>% nrow(),
    501)
  
  
  # reconstruct eds variables
  mbms %<>% mutate(
    eds_ut_recalculated =
      (-1*Court + 6 > 0) + 
      (-1*Serv + 6 > 0) + 
      (-1*Smart + 6 > 0) + 
      (-1*Afraid + 6 > 0) + 
      (-1*Haras + 6 > 0),
    eds_ut_rd_recalculated = 
      eds_ut_recalculated * (Why__multiple__01 | Why__multiple__03 | (Why__main_ %in% c(1,3))),
    eds_ut_recalculated = ifelse(is.na(eds_ut_recalculated), 0, eds_ut_recalculated),
    eds_ut_rd_recalculated = ifelse(is.na(eds_ut_rd_recalculated), 0, eds_ut_rd_recalculated))
  
  testthat::expect_true(all(
    mbms$eds_ut - mbms$eds_ut_recalculated == 0, na.rm=T))
  
  testthat::expect_true(all(
    mbms$eds_ut_rd - mbms$eds_ut_rd_recalculated == 0, na.rm=T))
  
  na_to_zero <- function(x) {
    ifelse(is.na(x), 0, x)
  }
  
  # test that the definition of EOD matches expectations
  mbms %<>% ungroup()
  mbms %<>% mutate(
    eod_ever_n = 
      na_to_zero(mbms$school == 1) + 
      na_to_zero(mbms$Job == 1) + 
      na_to_zero(mbms$Work == 1) + 
      na_to_zero(mbms$House == 1) + 
      na_to_zero(mbms$Med == 1) + 
      na_to_zero(mbms$Store == 1) + 
      na_to_zero(mbms$Bank == 1) + 
      na_to_zero(mbms$Street == 1) + 
      na_to_zero(mbms$Law == 1)
  )
  
  testthat::expect_true(
    all(unique(mbms$eod_s - mbms$eod_ever_n) %in% c(0, NA)))
  
  
  # test state policy liberalism -- 
  # make sure that the missing state policy liberalism values reflect 
  # only where the original authors did not estimate values -- 
  # this explains why there is more missingness for liberalism than, say,
  # the jim crow birth place variable
  missing_liberalism_states_of_birth <- mbms %>% filter(is.na(liberalism)) %>% 
    select(race, gender, StateOB, BirthYear, liberalism) %>% 
    pull(StateOB) 
  
  testthat::expect_true(all(missing_liberalism_states_of_birth %in% c(
    "Puerto Rico", 'Virgin Islands', 'District of Columbia', ''
    )))
  
})
