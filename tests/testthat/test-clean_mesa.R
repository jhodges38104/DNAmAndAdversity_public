
test_that("breakdown of participants is correct in MESA", {
  
  mesa <- make_clean_mesa()
  
  # 1264 total
  testthat::expect_equal(
    nrow(mesa),
    1264)
  
  # female
  testthat::expect_equal(
    mesa %>% filter(gender1 == 'Female') %>% nrow(),
    650)
  
  # male count
  testthat::expect_equal(
    mesa %>% filter(gender1 == 'Male') %>% nrow(),
    614)
  
  # hispanic count 
  testthat::expect_equal(
    mesa %>% filter(race1c == 'Hispanic') %>% nrow(),
    402)
  
  # white non-hispanic count
  testthat::expect_equal(
    mesa %>% filter(race1c == 'White N.H.') %>% nrow(),
    590)
  
  # black non-hispanic count
  testthat::expect_equal(
    mesa %>% filter(race1c == 'Black N.H.') %>% nrow(),
    272)
  
  
  # test state policy liberalism -- 
  # make sure that the missing state policy liberalism values reflect 
  # only where the original authors did not estimate values -- 
  # this explains why there is more missingness for liberalism than, say,
  # the jim crow birth place variable
  # missing_liberalism_states_of_birth <- mesa %>% filter(is.na(liberalism)) %>% 
  #   select(bth1, stbth1, birth_year, liberalism) %>% 
  #   pull(stbh1) 
  # 
  # testthat::expect_true(all(missing_liberalism_states_of_birth %in% c(
  #   "Puerto Rico", 'Virgin Islands', 'District of Columbia', ''
  #   )))
  
  
})
