library(testthat)
library(DNAm)

mbms <- make_clean_mbms()
mesa <- make_clean_mesa()

test_check("DNAm")
