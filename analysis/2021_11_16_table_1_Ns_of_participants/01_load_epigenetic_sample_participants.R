
# load mbms and mesa epi samples

mbms <- make_regression_ready_mbms()
mbms %<>% filter(epi_final_sample == 1)

mesa <- make_regression_ready_mesa(exclude_missing_nativity = FALSE)
