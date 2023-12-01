# Converting Categorical Variables to Dummy Variables and Vice-Versa
#
# This document introduces a pair of functions which should be intuitive to
# tidyverse users and standardizes the process for converting categorical
# columns of data back and forth from a set of dummy variables (columns of
# TRUE/FALSE indicators for each of the categorical levels).


#' Convert Categorical Variables to Dummy Variables 
#' 
#' This function returns a dataframe modified to contain dummy variables
#' (TRUE/FALSE indicator columns) for each level of the variable specified. 
#' 
#' The new column names are automatically constructed to be {variable}_{level}
#' where variable is given as a function argument and level is specified as
#' each unique level of the variable given.
#'
#' @param df the data.frame to modify
#' 
#' @param variable a tidy-evaluation style expression that names a column to
#' convert into dummy variables
#' 
#' @param drop_categorical (default: true) an indicator to remove the original
#' variable
#' 
#' @return a modified data.frame with new columns that are dummy indicators for
#' each level of the variable specified
#' 
#' @seealso dummies_to_categorical
#' @export
#' 
categorical_to_dummies <- function(df, variable, drop_categorical = TRUE) {
  
  # capture the tidy evaluation style variable name
  variable_orig <- rlang::enquo(variable)
  
  # convert to a character
  variable <- rlang::quo_name(variable_orig)
  
  # get the unique levels
  unique_levels <- na.omit(unique(df[[variable]]))
  
  # for each unique level, construct a new dummy variable
  for (level in unique_levels) {
    df[[paste0(variable, '_', level)]] <- (df[[variable]] == level)
  }
  
  # if drop_categorical is specified, drop the original variable
  if (drop_categorical) { df <- df %>% select(- c(!! variable_orig)) } 
  
  return(df)
}


#' Convert Dummy Variables to Categorical Variables
#' 
#' 
#' @param df a data.frame or tibble to modify
#' 
#' @param varname a tidy-evaluation style expression which is both the
#' new variable name to create and the prefix for the existing dummy variables
#' to convert into the new categorical variable. 
#' 
#' @param drop_dummies (default: true) an indicator to drop the dummy variables
#' after creating the new categorical variable
#' 
#' @return a modified data.frame with a new categorical variable based on the
#' dummy variables starting with variable_prefix
#' 
#' @seealso categories_to_dummies
#' @export
#' 
dummies_to_categorical <- function(df, varname, cols, drop_dummies = TRUE) {
  
  # capture tidy-evaluation style expression
  var_prefix_orig <- enquo(varname)
  
  # get the variable_prefix in character format
  variable_prefix <- rlang::quo_name(var_prefix_orig)
  
  if (missing(cols)) {
    
    # construct our regular expression to match variable_prefix columns
    variable_prefix_regex <- stringr::str_glue("^{variable_prefix}_")
  
    # match on the column names
    matching_varnames <- stringr::str_detect(colnames(df), variable_prefix_regex)
    
    # extract matching column names
    matching_varnames <- colnames(df)[matching_varnames]
    
    # get the unique levels based on the column names (after the variable_prefix part)
    unique_levels <- stringr::str_remove_all(matching_varnames, variable_prefix_regex)
  
  } else {
    # we assume the user specified the columns they want to convert to a categorical
    matching_varnames <- cols
    
    # get the unique levels based on the column names (after the variable_prefix part)
    unique_levels <- matching_varnames
  }
  
  # create a new vector to store categorical levels in
  new_vector <- rep(NA, nrow(df))
  
  # create an indicator for if there were logical inconsistencies -- namely if
  # multiple dummy variables are true at the same time, this means the dummy
  # variables cannot be represented by a categorical vector that takes on
  # discrete values one-at-a-time
  level_conflicts <- FALSE
  
  for (i in 1:length(unique_levels)) {
    
    # update the new vector to reflect if the i-th dummy variable is TRUE;
    # 
    # if so, set the ith value in the new_vector to the level corresponding to
    # unique_levels[[i]].
    
    for (j in 1:length(new_vector)) {
      # if the dummy variable is true
      new_vector[[j]] <- if(! is.na(df[[matching_varnames[[i]]]][[j]]) && df[[matching_varnames[[i]]]][[j]]) { 
        # if the new_vector hasn't already been written into in the jth term
        if (is.na(new_vector[[j]])) {
          # write in the appropriate categorical level
          unique_levels[[i]]
        } else {
          # otherwise not a logical conflict with the dummy variables.
          level_conflicts <- TRUE
        }
        # if the dummy variable is false
      } else {
        # leave the new_vector alone
        new_vector[[j]]
      } 
    }
  }
  
  # raise errors to reflect issues de-dummying the variables
  if (level_conflicts) {
    stop("conflicts in de-dummying variables")
  }
  
  # create the new variable
  df[[variable_prefix]] <- new_vector
  
  # if the user specifies, drop the dummy variables
  if (drop_dummies) { 
    df <- df %>% select(-matching_varnames)
  }
  
  return(df)
}
