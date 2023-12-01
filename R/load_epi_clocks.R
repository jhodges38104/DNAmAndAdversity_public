
# Load Epigenetic Clock Information

#' Load MBMS Epigenetic Clocks Data
#' @export
#' @examples 
#' mbms_epi_clocks <- load_mbms_epi_clocks()
load_mbms_epi_clocks <- function() {
  readr::read_csv(
    system.file(
      "epigenetics/Epigeneticclock 20211001/all_clocks_mbms.csv",
      package = 'DNAm'
    ),
    show_col_types = FALSE
  )
}

#' Load MESA Epigenetic Clocks Data
#' @export
load_mesa_epi_clocks <- function() {
  readr::read_csv(
    system.file(
      "epigenetics/Epigeneticclock 20211001/all_clocks_mesa.csv",
      package = 'DNAm'
    ),
    show_col_types = FALSE
  )
}

#' Load Epi Clocks Descriptions
#' @export
load_epi_clocks_descriptions <- function() {
  readxl::read_excel(
    system.file(
      "epigenetics/Epigeneticclock 20211001/epi_clocks_variables_descriptions.xlsx",
      package = 'DNAm'
    )
  )
}


#' Load Epi Clocks Supplemental Overview 
#' @export
load_epi_clocks_supplemental_overview <- function() {
  readxl::read_excel(
    system.file(
      "epigenetics/Epigeneticclock 20211001/supplemental_clock_overview.xlsx",
      package = 'DNAm'
    )
  )
}



#' Load MBMS Final Sample Info
#' @export
load_mbms_final_epi_sample_info <- function() {
  readr::read_csv(
    system.file(
      "epigenetics/Epigeneticclock 20211001/final_sample_info.csv",
      package = 'DNAm'
    ),
    show_col_types = FALSE
  )
}


#' Load MBMS GrimAge Estimates
#' @export
load_mbms_grimage <- function() {
  readr::read_csv(
    system.file(
      "epigenetics/grimage_mbms.csv",
      package = "DNAm"
    ),
    show_col_types = FALSE
  )
}

#' Add Epigenetic Clocks Data to MBMS
#' @export
add_epi_clocks_to_mbms <- function(mbms, with_final_sample_info = F) {
  
  mbms_clocks <- load_mbms_epi_clocks()
  
  # we don't need age from this dataset, as this is exactly duplicate 
  # to AgeYRS for those who made it into the epigenetics sample and is 
  # NA for those who didn't
  mbms_clocks %<>% select(-age)
  
  mbms %<>% left_join(mbms_clocks, by = c("ParticipantID" = "participant"))
  
  mbms %<>% left_join(load_mbms_grimage(), by = c("ParticipantID" = "mbms_id"))
  
  if (with_final_sample_info) {
    mbms %<>% add_epi_final_sample_info_to_mbms()
  }
  
  return(mbms)
}

#' Add Epigenetics Final Sample Info to MBMS
#' @export
add_epi_final_sample_info_to_mbms <- function(mbms) {
    final_sample_info <- load_mbms_final_epi_sample_info()
    mbms %<>% left_join(final_sample_info, by = c("ParticipantID" = "participant"))
}

#' Add Epigenetic Clocks Data to MESA
#' @export
add_epi_clocks_to_mesa <- function(mesa, with_final_sample_info = F) {
  
  mesa_clocks <- load_mesa_epi_clocks()
  
  # we don't need age from this dataset, as this is exactly duplicate 
  # to age5c for those who made it into the epigenetics sample and is 
  # NA for those who didn't
  mesa_clocks %<>% select(-age)

  
  mesa %<>% left_join(mesa_clocks, by = c("idno" = "mesa_id"))
  
  mesa %<>% merge_in_grimage_to_mesa()
  
  return(mesa)
}



#' Add Grimage data to MESA 
#' 
merge_in_grimage_to_mesa <- function(mesa) {
  grimage <- readr::read_csv(system.file("epigenetics/grimage_mesa.csv", package='DNAm'), show_col_types = FALSE)  %>% 
    select(-1) %>% 
    rename(idno = 'SampleID')
    
  mesa %<>% left_join(grimage %>% select(idno, DNAmGrimAge), by = c('idno' = 'idno'))
  
  return(mesa)
}

#' Clock Variables
#' @export
epigenetic_clock_variables <- vars(
  phenoage,
  zhang.mortality,
  dnamtl,
  age.hannum,
  age.horvath,
  epiToc.clock,
  miage.clock,
  dunedin_age,
  zhang_clock,
  DNAmGrimAge
)

#' Clock Variables Character vector
#' @export 
epigenetic_clock_variables_chr <-
  purrr::map(epigenetic_clock_variables,
             ~ str_replace_all(
                               rlang::quo_name(.), "~", "")) %>% 
  unlist() %>% 
  as.character()


#' Cell Types for Epigenetic Data
#' @export
#' 
cell_type_variables <- vars(
  Bcell, 
  CD4T,
  CD8T,
  Eos,
  Mono,
  Neu, 
  NK
)

#' Cell Type Variables Character vector
#' @export 
cell_type_variables_chr <-
  purrr::map(cell_type_variables,
             ~ str_replace_all(
                               rlang::quo_name(.), "~", "")) %>% 
  unlist() %>% 
  as.character()
