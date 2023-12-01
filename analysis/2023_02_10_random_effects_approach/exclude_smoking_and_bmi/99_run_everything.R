library(here)


analysis_dir <- here("exclude_smoking_and_bmi/")

source(here(analysis_dir, "00_setup.R"))
source(here(analysis_dir, "01_helper_variables.R"))
source(here(analysis_dir, "02_restructure_to_long_format.R"))
source(here(analysis_dir, "03_mbms_random_effects.R"))
source(here(analysis_dir, "04_mesa_random_effects.R"))
source(here(analysis_dir, "05_mbms_traditional_approach.R"))
source(here(analysis_dir, "06_mesa_traditional_approach.R"))
source(here(analysis_dir, "07_clean_results.R"))
source(here(analysis_dir, "08_plot_results.R"))

