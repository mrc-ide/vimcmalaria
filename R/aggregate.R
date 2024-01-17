# country functions ------------------------------------------------------------
#' Aggregate VIMC outputs up to the country level
#' @param   dt     data frame with site-level outputs to aggregate
#' @param   pop    VIMC population input for country level (age- and year-specific)
#' @returns data frame with aggregated outputs up to the country level
#' @export
aggregate_outputs<- function(dt, pop){
  dt<- data.table::data.table(dt)
  
  dt[, `:=` (
    cases = sum(cases),
    deaths = sum(deaths),
    dalys = sum(dalys)),
    by = c('age', 'year', 'scenario')]
  
  
  # remove cohort size, because for sites with some unmodelled locations, sum of cohort size != national population
  dt<- dt |> 
    dplyr::select(-cohort_size)
  dt <- unique(dt, by = c('age', 'year', 'scenario'))
  pop<- pop |>
    dplyr::rename(age = age_from,
           cohort_size = value) |>
    select(year, age, cohort_size)
  dt<- merge(dt, pop, by =c('age', 'year'))
  
  
  # calculate rates --------------------------------------------------------------
  dt[, `:=` (
    clinical = NULL,
    mortality = NULL,
    dalys_pp = NULL,
    site_name = iso3c,
    urban_rural = 'total'
  )]
  
  return(dt)
}


