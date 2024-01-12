# country functions ------------------------------------------------------------
aggregate_outputs<- function(dt, pop){
  dt<- data.table(dt)
  
  dt[, `:=` (
    cases = sum(cases),
    deaths = sum(deaths),
    dalys = sum(dalys)),
    by = c('age', 'year', 'scenario')]
  
  
  # remove cohort size, because for sites with some unmodelled locations, sum of cohort size != national population
  dt<- dt |> 
    select(-cohort_size)
  dt <- unique(dt, by = c('age', 'year', 'scenario'))
  pop<- pop |>
    rename(age = age_from,
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


