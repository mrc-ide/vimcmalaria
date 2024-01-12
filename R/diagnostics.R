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
      dplyr::summarise(n_model=mean(n_0_364),    ## average number of people in the eligible age grp (?best way to do this)
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



# save plotting data  ----------------------------------------------------------
pull_plotting_data<- function(scenario){
  
  if(scenario!="no-vaccination") {
    doses_per_year <- pull_doses_output(raw_output, dt)
    
    saveRDS(doses_per_year, 'doses_per_year.rds')
  } else{
    doses_per_year<- 0
  }
  
  
  ### pull out prevalence
  prev <- postie::get_prevalence(raw_output, 
                                 time_divisor = 365, 
                                 baseline_t = 1999,
                                 age_divisor = 365) 
  
  test<- raw_output |>
    mutate(year = floor(timestep/365)) |>
    group_by(year) |>
    summarise(n_2_10 = mean(n_730_3649)) |>
    filter(year<=100) 
  
  test<- test |> filter(year< max(test$year))
  
  prev$prevalence_2_10 <- test$n_2_10
  
  saveRDS(prev, 'prevalence_per_year.rds')
  # save outputs for plotting
  plotting_inputs<- list('vaccine_plot_input' = vaccine_plot_input,
                         'raw_model_output' = raw_output,
                         'prevalence_per_year' = prev,
                         'doses_per_year' = doses_per_year)
  
  return(plotting_inputs)
}




pull_age_groups_time_horizon<- function(quick_run, scenario, coverage_dt){
  
  year<- 365
  burnin<- 15
  
  if(quick_run == TRUE){
    
    term_yr<- 2025
    pop_val<- 5000
    
    min_ages = c(0:5, 6,15,20) * year
    max_ages = c(1:6, 15,20,200) * year -1
    
  } 
  
  else{
    
    pop_val<- 50000
    term_yr<- 2100
    
    term_yr<- 2050
    
    min_ages = c(seq(0, 19, by= 1), seq(20, 90, by= 10)) * year
    max_ages = c(seq(1, 20, by= 1), seq(30, 100, by= 10)) * year -1
    
  }
  
  return(list('term_yr' = term_yr, 
              'pop_val' = pop_val, 
              'min_ages'= min_ages, 
              'max_ages' = max_ages,
              'burnin' = burnin))
} 


