
#' Load MAPC Data
#' 
load_mapc <- function() {
  library(sf)
  sf::read_sf(system.file("mapc/export-gisdata.mapc.health_mapc_ppi_g250.shp", package='DNAm'))
}

#' Convert MAPC Projection to Lat/Lon System
convert_mapc_to_latlon <- function(mapc) {
  st_transform(mapc, "+proj=longlat")
}

#' Intersect MBMS and MAPC
#' 
intersect_mbms_and_mapc <- function() {

  mbms <- make_clean_mbms()

  # load mapc data, convert to lat/lons
  mapc <- load_mapc()
  mapc %<>% convert_mapc_to_latlon()

  # extract just the participant lat/lons, keep participant ID
  mbms_locations <- 
    mbms %>% select(ParticipantID, X, Y)

  # convert mbms_locations to an sf object
  mbms_locations %<>% 
    sf::st_as_sf(coords = c('X', 'Y'),
                 crs = "+proj=longlat")

  # reproject the mbms locations into the exact same coordinate system 
  # as the mapc data
  mapc_crs <- st_crs(mapc, asText=T)
  mbms_locations <- st_transform(mbms_locations, crs = mapc_crs)

  # use st_intersections to match 
  mbms_matched <- mbms_locations %>% mutate(
    intersection = as.integer(st_intersects(geometry, mapc)),
    area = ifelse(is.na(intersection), NA, mapc$g250m_id[intersection]))

  # save the data
  saveRDS(mbms_matched, 
    file.path(system.file('mapc/', package='DNAm'),
    'mbms_matched.rds'))

}

#' Make MAPC Simple (i.e. just g250m_id, ppi5)
#'
#' The reason we're making this is because working with 
#' rgdal, rgeos, etc. is often causing my R session to seg-fault,
#' so by writing a CSV version of just the data we need to work 
#' with, we can avoid working with the geometry data which seems 
#' to be causing the seg-faults I'm encountering.
#' 
make_mapc_simple <- function() {

  # load mapc data
  mapc <- load_mapc()

  # only keep mapc info we need
  mapc %<>% select(g250m_id, ppi5)

  # convert to data frame
  mapc <- as.data.frame(mapc) %>% select(-geometry)
  
  # write csv
  write.csv(mapc, 
    file.path(system.file('mapc/', package='DNAm'),
    'mapc_simple.csv'), row.names=F)
}

#' Load MAPC Simple (g250m_id, ppi5)
#'
load_mapc_simple <- function() {
  readr::read_csv(system.file('mapc/mapc_simple.csv', package="DNAm"), show_col_types = FALSE)
}

#' Merge MAPC Data into MBMS
merge_mapc_into_mbms <- function(mbms) {
  
  # retrieve the matching from mbms participants -> mapc g250m_ids
  mbms_mapc_matching <- readRDS(
    system.file('mapc/mbms_matched.rds', package='DNAm'))

  # merge in the mapc area codes
  mbms %<>% merge(mbms_mapc_matching[c('ParticipantID', 'area')], by = 'ParticipantID', all.x=T)
  
  # rename to g250m_id to match
  mbms %<>% rename(g250m_id = area)

  # load mapc data
  mapc <- load_mapc_simple()

  # merge ppi5 into mbms
  mbms %<>% merge(mapc, by = 'g250m_id', all.x=T)

  return(mbms)

}
