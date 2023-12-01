
#' Load Poverty Threshold Data
#' @export
load_poverty_thresholds <- function(year = 2011, skip_header = T) {
  readxl::read_xls(
    system.file(
      stringr::str_c(
      "helper_data/poverty_threshold_", year, ".xls"),
      package='DNAm'),
    skip = if (skip_header) 8 else 0
  )
}

  #' Calculate Ratio to Poverty Threshold
  #' @export
  calculate_poverty_ratio <- function(
    thresholds = load_poverty_thresholds(),
    household_income,
    total_people,
    num_seniors,
    num_children) {
    
    if (! is.na(total_people) & ! is.na(num_children)) {
      if (total_people <= num_children) {
        total_people <- num_children + 1
      }
    }

    poverty_threshold <- 
      # handle NAs
      if (is.na(total_people)) {
        NA
      # single person households
      } else if (total_people <= 1) {
        # with a senior
        if (! is.na(num_seniors) & num_seniors >= 1) {
          thresholds[[2,3]]
        } else {
        # without a senior
          thresholds[[1,3]]
        }
      # two person households
      } else if (total_people == 2) {
        # with a senior
        if (! is.na(num_seniors) & num_seniors >= 1) {
          # with a child
          if (! is.na(num_children) & num_children >= 1) {
            thresholds[[6,4]]
          # without children
          } else {
            thresholds[[6,3]]
          }
        # without a senior
        } else {
          # with a child 
          if (! is.na(num_children) & num_children >= 1) {
            thresholds[[5,4]]
          # without children
          } else {
            thresholds[[5,3]]
          }
        }
      # >=3 person households
      } else {
        col_i <- if (is.na(num_children)) 2 else min(num_children,8) + 3
        # <= 8 person households
        if (total_people <= 8) {
          row_i <- total_people + 5
          thresholds[[row_i, col_i]]
        # >= 9 person households
        } else {
          thresholds[[14,col_i]]
        }
      }

    return(household_income / poverty_threshold)
  }

