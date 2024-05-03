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
    dplyr::select(-.data$cohort_size)
  dt <- unique(dt, by = c('age', 'year', 'scenario', 'parameter_draw'))
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
    site_name = NULL,
    urban_rural = 'total'
  )]

  return(dt)
}


#' Aggregate dose outputs
#' @param   doses_full     dose input
#' @returns aggregated dose output
#' @export
aggregate_doses<- function(doses_full){
  doses_per_year <- doses_full |>
    dplyr::group_by(year) |>
    summarise(doses=sum(doses)) |>
    mutate(scenario=scenario)

  return(doses_per_year)
}
