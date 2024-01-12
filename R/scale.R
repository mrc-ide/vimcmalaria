
scale_cases<- function(dt, site_data){
  
  # scale outputs based on cases from WMR from 2000-2020
  # first sum cases by year (across all ages) in model output and compare
  pre_scale<- dt |>
    filter(scenario== 'no-vaccination')|>
    group_by(year) |>
    summarise(cases = sum(cases))
  
  #average site file cases across last three years
  site_file_cases<- data.table::data.table(site_data$cases_deaths[, c('year', 'wmr_cases')])
  site_file_cases<- site_file_cases[year >= 2018]
  average_value<- mean(site_file_cases$wmr_cases)
  
  # calculate ratio in comparison to year 2020 cases in output
  output_cases<- pre_scale |>
    filter(year == 2020) |>
    select(cases)
  
  ratio<- average_value/output_cases$cases
  
  # add pre-scaled cases to output df as a new column
  dt<- dt |>
    mutate(pre_scaled_cases = cases)
  
  dt<- dt |>
    mutate(cases = cases * ratio)
  
  dt<- dt|>
    mutate(clinical= cases/cohort_size,
           mortality = deaths/ cohort_size,
           dalys_pp = dalys/ cohort_size) |>
    select(-site_name, -urban_rural)
  
  return(dt)
}

# cases averted per fully vaccinated child
pull_outcomes_averted_per_100k_vacc <- function(intvn_output, baseline_output, doses) {
  
  tot_doses <-sum(doses$doses, na.rm = T)
  
  merged <- calculate_cases_averted(intvn_output, baseline_output)
  outcomes <- merged |>
    dplyr::summarise(cases_averted=sum(cases_averted),
                     deaths_averted=sum(deaths_averted),
                     dalys_averted=sum(dalys_averted))
  
  res <-100000*outcomes/tot_doses
  
  return(res)
}
pull_other_case_metrics<- function(site_data, intvn_results, bl_results){
  # pull some numbers out for the report
  # Total cases in the country in WMR 2020 
  cases_wmr_2020 <- site_data$cases_deaths$wmr_cases[which(site_data$cases_deaths$year==2020)]
  
  # Total modelled cases in 2020 were
  cases_mod_2020 <- bl_results |>
    filter(year==2020) |>
    summarise(cases = sum(cases)) |>
    pull(cases)
  
  # Up to 2100, xxxx % of cases were averted by vaccine
  percent_cases_averted_2100 <- 
    round(100 * (1 - sum(intvn_results$cases) / sum(bl_results$cases)), 2)
  
  return(list('cases_wmr_2020' = cases_wmr_2020, 'cases_mod_2020' = cases_mod_2020, 'percent_cases_averted_2100' = percent_cases_averted_2100))
  
}


calculate_cases_averted <- function(intvn_output, baseline_output) {
  
  intvn_output<- intvn_output |> 
    rename(cases_intvn = cases,
           dalys_intvn = dalys,
           deaths_intvn = deaths)
  
  merged<- merge(intvn_output, baseline_output, by= c('year', 'age'))
  
  merged<- merged |>
    mutate(cases_averted = cases - cases_intvn,
           deaths_averted = deaths - deaths_intvn,
           dalys_averted = dalys - dalys_intvn)
  
  return(merged)
}

format_descriptive_data<- function(){
  # pulls descriptive data from global environment
  descriptive_dt<- list('iso3c' = iso3c,
                        'scenario' = scenario,
                        'quick_run' = quick_run,
                        'parameter_draw' = parameter_draw,
                        'description' = description)
  
  return(descriptive_dt)
}

format_input_data<- function(){
  
  
  agg_output<- processed_output |>  # aggregated all-age output
    group_by(year, scenario) |>
    summarise(cases = sum(cases),
              dalys = sum(dalys),
              deaths = sum(deaths),
              pre_scaled_cases = sum(pre_scaled_cases),
              cohort_size = sum(cohort_size)) |>
    mutate(mortality = deaths/cohort_size,
           clinical = cases/ cohort_size,
           dalys_pp = dalys/ cohort_size,
           iso3c= iso3c) 
  
  intvn_scenario<- scenario         # coverage data for intervention scenario
  coverage_data<- coverage_data |>
    filter(scenario == intvn_scenario,
           country_code == iso3c)
  
  # outcomes averted + other case metrics
  outcomes_averted <- round(pull_outcomes_averted_per_100k_vacc(intvn_results , bl_results , doses))
  case_metrics<- pull_other_case_metrics(site_data, intvn_results, bl_results)
  
  return(list('processed_output' = processed_output,
              'site_data' = site_data,
              'coverage_data' = coverage_data,
              'population_data' = pop_data,
              'outcomes_averted' = outcomes_averted,
              'agg_output' = agg_output,
              'case_metrics' = case_metrics))
}
