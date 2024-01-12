process_output<- function(model, site_name, ur, scenario){
  
  message('postprocessing')
  # calculate rates
  raw_output<- drop_burnin(model, burnin= unique(model$burnin)* 365)
  
  output <- postie::get_rates(
    raw_output,
    time_divisor = 365,
    baseline_t = 1999,
    age_divisor = 365,
    scaler = 0.215,
    treatment_scaler = 0.517,
  )
  
  dt<- vimc_postprocess(output, le, site_name= site_name, ur= ur, site_data, vimc_pop, pop_single_yr)
  
  
  # final formatting  ------------------------------------------------------------
  output<- format_outputs(dt, site_name = site_name, ur= ur)
  
  if(scenario!="no-vaccination") {
    
    doses_per_year <- pull_doses_output(raw_output, output)
    
  } else{
    doses_per_year<- data.table()
  }
  ### pull out prevalence
  prev <- postie::get_prevalence(raw_output, time_divisor = 365, baseline_t = 1999,
                                 age_divisor = 365) 
  prev$n_2_10 <- raw_output |>
    mutate(year = floor(timestep/365)) |>
    group_by(year) |>
    summarise(n_2_10 = mean(n_730_3649)) |>
    filter(year != max(year)) |>
    pull(n_2_10)
  
  
  return(list('processed_output' = output, 'doses' = doses_per_year, 'prevalence' = prev))
}


expand_life_expectancy<- function(le){
  
  le<- le |>
    filter(country_code == iso3c,
           year >= 2000) |>
    dplyr::group_by(year) |>
    tidyr::complete(age_from = c(1:100)) |>
    dplyr::ungroup() |>
    tidyr::fill(dplyr::all_of(names(le)), .direction = "down")
  
  # fill years out (five year age groups)
  le<- le |>
    dplyr::group_by(age_from) |>
    tidyr::complete(year = c(2000:2100))|>
    dplyr::ungroup() |>
    tidyr::fill(dplyr::all_of(names(le)), .direction = "down") |>
    rename(age_lower = age_from,
           remaining_yrs = value) |>
    select(year, age_lower, remaining_yrs) 
  
  return(le)
}

vimc_postprocess<- function(output, le, site_data, site_name, ur, vimc_pop, pop_single_yr){
  
  # fill rates out to single year age groups
  output<- output |>
    dplyr::group_by(t) |>
    tidyr::complete(age_lower = c(1:100)) |>
    select(-age_upper) |>
    dplyr::ungroup() |>
    tidyr::fill(clinical, severe, mortality, yld_pp, yll_pp, dalys_pp, .direction = 'down') |>
    select(-prop_n, -n, -yll_pp, -dalys_pp) |>
    rename(year = t)
  
  if (quick_run == TRUE){
    
    # fill rates out flatly 
    output<- output |>
      dplyr::group_by(age_lower) |>
      tidyr::complete(year = c(2000:2100)) |>
      dplyr::ungroup() |>
      tidyr::fill(clinical, severe, mortality, yld_pp, .direction = 'down')
  }
  
  
  # merge in inputs for expected remaining years of life (to calculate YLLs)  ------
  le<- expand_life_expectancy(le)
  
  # calculate ylls_pp + dalys per person
  dt<- merge(output, le, by = c('year', 'age_lower'), all.x = TRUE)
  
  # recalculate YLLs and DALYs based on country-specific life expectancy  ------
  dt<- dt |>
    mutate(ylls_pp = mortality * remaining_yrs) |>
    mutate(dalys_pp = ylls_pp + yld_pp) |>
    select(-remaining_yrs)
  
  # scale site population to be consistent with VIMC population ----------------
  populations<- scale_population(site_data,site_name, ur, vimc_pop, pop_single_yr)
  
  # merge in site population + prop_n
  dt<- merge(dt, populations$site_population, by= 'year')
  dt<- merge(dt, populations$age_proportions, by = c('year', 'age_lower'))
  
  # calculate counts for entire time period --------------------------------------
  dt<- dt |>
    mutate(
      cases = round(clinical * vimc_site_population * prop_n),
      deaths = round(mortality * vimc_site_population * prop_n),
      dalys = round(dalys_pp * vimc_site_population * prop_n),
      population = round(vimc_site_population * prop_n)) |>
    select(-prop_n)
  
  return(dt)
  
}


format_outputs<- function(dt, site_name, ur){
  dt <- dt |>
    mutate(
      disease = 'Malaria',
      country = iso3c,
      country_name = countrycode::countrycode(
        sourcevar = iso3c,
        origin = 'iso3c',
        destination = 'country.name'),
      site_name = site_name,
      urban_rural = ur,
      scenario = scenario,
      description = description
    ) |>
    rename(age = age_lower,
           cohort_size = population) |>
    select(
      disease,
      year,
      age,
      country,
      country_name,
      site_name,
      urban_rural,
      scenario,
      description,
      cohort_size,
      cases,
      dalys,
      deaths,
      clinical,
      mortality,
      dalys_pp
    ) |>
    mutate(
      cases = if_else(is.na(cases), 0, cases),
      deaths = if_else(is.na(deaths), 0, deaths),
      dalys = if_else(is.na(dalys), 0, dalys),
      mortality = if_else(is.na(mortality), 0, mortality),
      clinical = if_else(is.na(clinical), 0, clinical),
      dalys = if_else(is.na(dalys), 0, dalys)
    )
  
  return(dt)
}


scale_population<- function(site_data, site_name, ur, vimc_pop, pop_single_yr){
  # merge in population from site files (as we only have VIMC inputs for the national level)
  # first, separately sum cases by year
  total_pop<- site_data$population |>
    group_by(year) |>
    summarise(summed_pop = sum(pop))
  
  # pull the population for the site of interest
  pop <- site_data$population |>
    filter(name_1 == site_name & urban_rural == ur) |>
    select(year, pop) |>
    rename(site_file_population = pop)
  
  # merge these two tables together
  pops<- merge(pop, total_pop, by= 'year')
  
  # merge in national population from VIMC (available for entire time period)
  vimc_pop<- vimc_pop |>
    filter(country_code == iso3c,
           year >= 2000)|>
    rename(national_pop = value)|>
    select(year, national_pop)
  
  # merge in vimc population
  pops<- merge(vimc_pop, pops, all.x = T)
  
  # first rescale site file population based on the ratio of (sum of site file pops in country)/ (VIMC country level population)
  # should be more or less the same, but should be done for consistency sake
  pops<- pops |>
    mutate(vimc_site_population = (site_file_population * national_pop)/summed_pop)
  
  # calculate population ratio as vimc(site)/ vimc(country)
  pops<- pops |>
    mutate(pop_ratio = vimc_site_population/ national_pop) |>
    tidyr::fill(pop_ratio, .direction = 'down')
  
  # then calculate vimc_site_population by multiplying this ratio to the national population for the final 50 years
  pops<- pops |>
    mutate(vimc_site_population = ifelse(year<= 2050, vimc_site_population, pop_ratio* national_pop))
  
  # subset out site file population for 2000-2100
  site_pop<- pops |>
    select(year, vimc_site_population)
  
  # pull in single year population to calculate proportion_n by age group
  national_pop<- pops |>
    select(year, national_pop)
  
  age_proportions<- merge(pop_single_yr, national_pop, by = c('year'))
  age_proportions <- age_proportions |>
    mutate(prop_n = value/ national_pop) |>
    select(year, age_from, age_to, prop_n) |>
    rename(age_lower = age_from)
  
  return(list('site_population' = site_pop, 'age_proportions' = age_proportions))
  
}

pull_doses_output <- function(raw_output, processed_output) {
  scenario <- raw_output$scenario[1]
  raw_output$year <- floor(raw_output$timestep / 365) + 2000
  
  ## Pull out doses before 2040 and over all time
  # N fully vaccinated children are the number receiving the last dose.
  
  if(grepl("rts4", scenario) | grepl("r4", scenario)) {  ## for the booster scenarios
    
    doses_per_year <-raw_output |>
      dplyr::group_by(year) |>
      dplyr::summarise(n_model=mean(n_365_729),    ## average number of people in the eligible age grp (?best way to do this)
                       doses_model=sum(n_pev_epi_booster_1)) |>
      mutate(rate_dosing = doses_model/n_model)
    
    ### Merge in VIMC pop in eligible age gp.
    vimc_cohort <- processed_output |>
      dplyr::filter(age==1) |>
      dplyr::select(year, cohort_size)
    
    doses_per_year <- left_join(doses_per_year, vimc_cohort, by="year") |>
      mutate(doses = rate_dosing * cohort_size)
    
  } else {   ## for the dose 3 without booster scenarios
    
    doses_per_year <-raw_output |>
      dplyr::group_by(year) |>
      dplyr::summarise(n_model=mean(n_0_364),    ## average number of people in the eligible age grp
                       doses_model=sum(n_pev_epi_dose_3)) |>
      mutate(rate_dosing = doses_model/n_model)
    
    ### Merge in VIMC pop in eligible age gp.
    vimc_cohort <- processed_output |>
      dplyr::filter(age==0) |>
      dplyr::select(year, cohort_size)
    
    doses_per_year <- left_join(doses_per_year, vimc_cohort, by="year") |>
      mutate(doses = rate_dosing * cohort_size)  %>%
      select(-c(n_model, doses_model)) %>%
      filter(year<=2100)
    
  }
  return(doses_per_year)
}