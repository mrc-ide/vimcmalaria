
#' parameterize site + urbanicty of interest
#' @param   site_name        name of site
#' @param   ur               urbanicity, urban or rural
#' @param   site_data        site file
#' @param   coverage_data    VIMC vaccine forecast for site of interest
#' @param   scenario         vaccine forecast scenario
#' @param   gfa              global fund assumptions for other intervention coverage
#' @param   parameter_draw   parameter draw value
#' @param   quick_run        quick_run setting (boolean)
#' @param   iso3c            country code
#' @returns site file with additional variables 'rtss_coverage', 'rtss_booster_coverage', 'r21_coverage', 'r21_booster_coverage'
#' @export
pull_input_params<- function(site_name,
                             ur,
                             iso3c,
                             site_data,
                             coverage_data,
                             scenario,
                             gfa,
                             parameter_draw,
                             quick_run){

  message('parameterizing')
  # site data
  site <- extract_site(site_file = site_data,
                       site_name = site_name,
                       ur = ur)


  run_params<- pull_age_groups_time_horizon(quick_run)

  # specify vaccine coverage based on forecast  --------------------------------
  site<- expand_intervention_coverage(site,
                                      terminal_year = run_params$term_yr)
  site<- update_coverage_values(site,
                                iso3c = iso3c,
                                coverage_data,
                                scenario_name = scenario)

  # check the site has a non-zero EIR
  check_eir(site)

  # pull parameters for this site ------------------------------------------------
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    burnin = run_params$burnin,
    overrides = list(human_population = run_params$pop_val)
  )


  # set age groups
  params$clinical_incidence_rendering_min_ages = run_params$min_ages
  params$clinical_incidence_rendering_max_ages = run_params$max_ages
  params$severe_incidence_rendering_min_ages = run_params$min_ages
  params$severe_incidence_rendering_max_ages = run_params$max_ages
  params$age_group_rendering_min_ages = run_params$min_ages
  params$age_group_rendering_max_ages = run_params$max_ages

  # if this is a stochastic run, set parameter draw ------------------------------
  params<- parameterize_stochastic_run(params, parameter_draw)

  if(iso3c == 'ETH'){

    cali_eir<- readRDS(paste0('J:/VIMC_malaria/analyses/ethiopia/calibrations/calibrated_site',site_name, '.rds' ))
    cali_EIR<- cali_eir$eir_info$EIR

    message(paste0('calibrating to EIR of ', cali_EIR))
    params<- set_equilibrium(params, init_EIR = cali_EIR)


  }

  params$pev<- TRUE

  inputs <- list(
    'param_list' = params,
    'site_name' = site_name,
    'ur' = ur,
    'iso' = iso3c,
    'scenario' = scenario,
    'gfa' = gfa,
    'parameter_draw' = parameter_draw,
    'pop_val' = run_params$pop_val,
    'burnin' =  run_params$burnin
  )

  return(inputs)

}


#' Make list of parameters that will apply to all model runs
#' @param   quick_run     quick run setting
#' @returns list of parameters for all model runs
#' @export
pull_age_groups_time_horizon<- function(quick_run){

  year<- 365
  burnin<- 15

  if(quick_run == TRUE){

    term_yr<- 2030
    pop_val<- 5000

    min_ages = c(0:5, 6,15,20) * year
    max_ages = c(1:6, 15,20,200) * year -1

  } else{

    pop_val<- 50000
    term_yr<- 2100

    min_ages = c(seq(0, 19, by= 1), seq(20, 90, by= 10)) * year
    max_ages = c(seq(1, 20, by= 1), seq(30, 100, by= 10)) * year -1

  }

  return(list('term_yr' = term_yr,
              'pop_val' = pop_val,
              'min_ages'= min_ages,
              'max_ages' = max_ages,
              'burnin' = burnin))
}

#' Change parameter set based off of stochastic draws
#' @param   params           model input parameters
#' @param   parameter_draw   parameter draw number (0-50)
#' @returns parameters for stochastic run
#' @export
parameterize_stochastic_run<- function(params, parameter_draw){

  if (parameter_draw > 0){

    params<- params |>
      malariasimulation::set_parameter_draw(parameter_draw) |>
      set_equilibrium(init_EIR= params$init_EIR)

  }

  return(params)
}

#' update vaccine coverage based on VIMC inputs from Montagu
#' @param   site             site data file
#' @param   iso3c            country to update coverage values for
#' @param   coverage_data    VIMC vaccine forecast for site of interest
#' @param scenario_name scenario for vaccine forecast
#' @returns site file with additional variables 'rtss_coverage', 'rtss_booster_coverage', 'r21_coverage', 'r21_booster_coverage'
#' @export
update_coverage_values<- function(site, iso3c, coverage_data, scenario_name){

  coverage_data <- coverage_data |>
    dplyr::filter(country_code == iso3c) |>
    dplyr::filter(scenario == scenario_name)


  dt <- coverage_data |>
    rename(vaccine_name = vaccine) |>
    data.table()

  # add identifying type column for vaccine
  dt[{{vaccine_name}} %like% 'RTS', vaccine := 'RTS,S']
  dt[is.na(vaccine), vaccine := 'R21']

  vaccine_val<- unique(dt$vaccine)

  if (length(vaccine_val) > 1){ stop('Can only implement one type of vaccine at a time. Check vaccine inputs.') }

  dt<- data.table::dcast(data.table::data.table(dt),
             year + vaccine ~ vaccine_name,
             value.var= 'coverage')

  # if columns for other vaccines or doses are empty, fill them ----------------
  columns_to_check <- c("R3", "R4", "RTS3", "RTS4")
  missing_columns <- setdiff(columns_to_check, names(dt))

  dt <- dt |>
    tibble::add_column(!!!stats::setNames(rep(0, length(missing_columns)),
                           missing_columns))

  dt <- dt |>
    rename(rtss_coverage = RTS3,
           rtss_booster_coverage = RTS4,
           r21_coverage = R3,
           r21_booster_coverage = R4)

  # transform booster coverage into value per person according to coverage in the preceding year
  if(scenario_name == 'malaria-rts3-rts4-bluesky' | scenario_name == 'malaria-r3-r4-bluesky'){

    dt[rtss_booster_coverage== 0.9, rtss_booster_coverage:= 1]
    dt[r21_booster_coverage== 0.9, r21_booster_coverage:= 1]

  }else{
  for (yr in unique(dt$year)){

    dt[year== yr & rtss_coverage!= 0 & rtss_booster_coverage!= 0,
       rtss_booster_coverage := rtss_booster_coverage / dt[year == yr- 1, rtss_coverage]]

    dt[year== yr & r21_coverage!= 0 & r21_booster_coverage!= 0,
       r21_booster_coverage := r21_booster_coverage / dt[year == yr- 1, r21_coverage]]
  }
}
  intvns<- data.table::data.table(merge(site$interventions, dt, by = 'year', all.x= T))

  intvns[is.na(rtss_coverage), "rtss_coverage" := 0]
  intvns[is.na(rtss_booster_coverage), "rtss_booster_coverage" := 0]
  intvns[is.na(r21_coverage), "r21_coverage" := 0]
  intvns[is.na(r21_booster_coverage), "r21_booster_coverage" := 0]
  intvns[is.na(vaccine), vaccine := vaccine_val]

  site$interventions<- intvns

  return(site)
}


#' expand intervention years out to terminal year of forecast using scene package
#' @param   site             site file
#' @param   terminal_year    terminal year of forecast
#' @returns site file with extrapolated coverage values out to terminal year
#' @export
expand_intervention_coverage<- function(site, terminal_year){

  # first set terminal year to terminal year of forecast
  group_var <- names(site$sites)

  first_yr<- max(site$interventions$year) + 1            # first year in site file
  itn_yr<- first_yr- 3                                   # last year to carry over for ITN usage and model input (3 year cycle)

  site$interventions <- site$interventions |>
    scene::expand_interventions(max_year = terminal_year,
                                group_var = group_var)


  for (yr in c(first_yr:terminal_year)){

    comparator<- site$interventions |>
      dplyr::filter(year == yr - 3)

    intvns <-   data.table::data.table(site$interventions)
    intvns[year == yr, `:=` (itn_use = comparator$itn_use,
                             itn_input_dist = comparator$itn_input_dist)]
    site$interventions <- intvns

  }


  site$interventions <- site$interventions |>
    scene::fill_extrapolate(group_var = group_var)

  return(site)
}

#' Very basic recalibration function (for Ethiopia sites)
#' @param   params           simulation parameters
#' @param   site_name        name of site to recalibrate
#' @param   site_dt site data
#' @returns recalibrated site
#' @export
recalibrate<- function(params, site_name, site_dt){


  summary_mean_pfpr_2_10 <- function (x) {
    message('calibrating')
    x<- data.table(x)
    # Calculate the PfPR2-10:
    prev_2_10 <- mean(x[timestep %in% c((10*365):(11*365))]$n_detect_pcr_730_3649/x[timestep %in% c((10*365):(11* 365))]$n_730_3649) # average over a year

    # Return the calculated PfPR2-10:
    return(prev_2_10)
  }

  # pull target pfpr from 2010 for corresponding site
  target_pfpr <- site_dt$prevalence |> dplyr::filter(year == 2010, name_1 == site_name) |> dplyr::pull(pfpr)

  print(paste0('target pfpr ', target_pfpr ))

  # Add a parameter to the parameter list specifying the number of timesteps
  simparams<- copy(params)
  simparams$timesteps <- 12 * 365

  # Establish a tolerance value:
  pfpr_tolerance <- 0.01

  # Set upper and lower EIR bounds for the calibrate function to check
  lower_EIR <- 0.01; upper_EIR <- 60

  # Run the calibrate() function:
  cali_EIR <- cali::calibrate(target = target_pfpr,
                        summary_function = summary_mean_pfpr_2_10,
                        parameters = simparams,
                        tolerance = pfpr_tolerance,
                        low = lower_EIR, high = upper_EIR)

  print(paste0('calibrated EIR for site ', site_name, ' :', cali_EIR))

  params<- set_equilibrium(params, init_EIR = cali_EIR)
  eir_info<- data.frame('site_name' = site_name, 'EIR' = cali_EIR)

  return(list('params' = params, 'eir_info' = eir_info))
}



#' parameterize site + urbanicty of interest
#' @param   site_name        name of site
#' @param   ur               urbanicity, urban or rural
#' @param   site_data        site file
#' @param   parameter_draw   parameter draw value
#' @param   quick_run        quick_run setting (boolean)
#' @param   iso3c            country code
#' @returns site file with additional variables 'rtss_coverage', 'rtss_booster_coverage', 'r21_coverage', 'r21_booster_coverage'
#' @export
pull_baseline_params<- function(site_name,
                                ur,
                                iso3c,
                                site_data,
                                parameter_draw,
                                quick_run){

  message('parameterizing')
  # site data
  site <- extract_site(site_file = site_data,
                       site_name = site_name,
                       ur = ur)


  site$interventions$rtss_coverage<- 0
  site$interventions$r21_coverage<- 0
  site$interventions$rtss_booster_coverage<- 0
  site$interventions$r21_booster_coverage<- 0

  run_params<- pull_age_groups_time_horizon(quick_run)

  # check the site has a non-zero EIR
  check_eir(site)

  # pull parameters for this site ----------------------------------------------
  params <- site::site_parameters(
    interventions = site$interventions,
    demography = site$demography,
    vectors = site$vectors,
    seasonality = site$seasonality,
    eir = site$eir$eir[1],
    burnin = run_params$burnin,
    overrides = list(human_population = run_params$pop_val)
  )


  # set age groups
  params$clinical_incidence_rendering_min_ages = run_params$min_ages
  params$clinical_incidence_rendering_max_ages = run_params$max_ages
  params$severe_incidence_rendering_min_ages = run_params$min_ages
  params$severe_incidence_rendering_max_ages = run_params$max_ages
  params$age_group_rendering_min_ages = run_params$min_ages
  params$age_group_rendering_max_ages = run_params$max_ages

  # if this is a stochastic run, set parameter draw ----------------------------
  params<- parameterize_stochastic_run(params, parameter_draw)

  if(iso3c == 'ETH'){

    cali_eir<- readRDS(paste0('J:/VIMC_malaria/analyses/ethiopia/calibrations/calibrated_site',site_name, '.rds' ))
    cali_EIR<- cali_eir$eir_info$EIR

    message(paste0('calibrating to EIR of ', cali_EIR))
    params<- set_equilibrium(params, init_EIR = cali_EIR)


  }

  params$pev<- TRUE

  inputs <- list(
    'param_list' = params,
    'site_name' = site_name,
    'ur' = ur,
    'iso' = iso3c,
    'parameter_draw' = parameter_draw
  )

  return(inputs)

}

