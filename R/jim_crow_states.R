#' Jim Crow States
#' @export 
jim_crow_states <- 
  c('TX', 'OK', 'MO', 'AR', 'LA', 'MS', 'TN', 'KY', 'AL', 'GA', 'FL',
    'SC', 'NC', 'VA', 'WV', 'DC', 'MD', 'DE'
  )


# US Census Regions

west <- c('ID', "MT", "WY", "NV", 'UT', 'CO', 'AZ', 'NM',
  'AK', 'HI', 'WA', 'OR', 'CA')

midwest <- c('ND', 'MN', 'WI', 'MI', 'SD', 'IA', 'IL', 'IN', 'OH',
'NE', 'KS', 'MO')

south <- c('OK', 'TX', 'AR', 'LA', 'MS', 'TN', 'KY', 'WV', 'MD',
'DE', 'DC', 'VA', 'NC', 'AL', 'GA', 'SC', 'FL')

northeast <- c('ME', 'NH', 'VT', 'MA', 'CT', 'RI', 'NY', 'PA', 'NJ')


regions <- data.frame(
  region = 
    c(rep('west', length(west)),
      rep('midwest', length(midwest)),
      rep('south', length(south)),
      rep('northeast', length(northeast))),
  state_abb = 
    c(west,
    midwest,
    south,
    northeast))

regions_lookup <- setNames(regions$region, regions$state_abb)
