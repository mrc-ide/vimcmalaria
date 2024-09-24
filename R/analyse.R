#' model vaccine impact
#' @param site   analysis map with input parameters
#' @param site_data site data
#' @param vimc_input vimc_input
#' @param baseline_outputs baseline_outputs
#' @returns modelled + processed output
#' @export
analyse_site<- function(site,
                        site_data,
                        vimc_input,
                        baseline_outputs){

  model_input<- pull_input_params(site_name = site$site_name,
                                  ur = site$ur,
                                  site_data = site_data,
                                  coverage_data = vimc_input$coverage_input,
                                  scenario = site$scenario,
                                  iso3c = site$iso3c,
                                  parameter_draw = site$parameter_draw,
                                  quick_run = site$quick_run)


  model<- run_scenario_model(model_input, baseline_outputs)
  output<- process_output(model,
                          vimc_input,
                          site_data = site_data,
                          site_name = site$site_name,
                          ur = site$ur,
                          iso3c = site$iso3c,
                          scenario = site$scenario,
                          parameter_draw = site$parameter_draw,
                          quick_run = site$quick_run,
                          description= site$description)

  return(output)
}



#' model vaccine impact
#' @param site   analysis map with input parameters
#' @param site_data site data
#' @param vimc_input vimc_input
#' @returns modelled + processed output
#' @export
analyse_baseline<- function(site,
                        site_data){

  model_input<- pull_baseline_params(site_name = site$site_name,
                                     ur = site$ur,
                                     site_data = site_data,
                                     iso3c = site$iso3c,
                                     parameter_draw = site$parameter_draw,
                                     quick_run = site$quick_run)


  model<- run_baseline_model(model_input)

  return(model)
}

#' make an analysis map of input parameters for vaccine modelling run
#' @param site_df   analysis map with input parameters
#' @param site_data site data
#' @param test      boolean-- if true, only run analysis for two test sites. Good for quick tests of code functionality
#' @param run_all   run all sites regardless of pfpr (boolean)
#' @returns analysis map to be used as an input for analyse_site
#' @export
make_analysis_map<- function(site_df,
                             site_data,
                             test,
                             run_all){

  site_data$prevalence<- site_data$prevalence |>
    dplyr::filter(year == 2019) |>
    mutate(run_model = ifelse(pfpr > 0.10, TRUE, FALSE))

  # make exceptions for Madagascar, Ethiopia, and Sudan
  # hardcode for time's sake but operationalize later
  if(unique(site_df$country) == 'Madagascar'){

    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'Toliary', TRUE, run_model))
  }

  if(unique(site_df$country) == 'Ethiopia'){

    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 %like% 'Gambela', TRUE, run_model))


  }

  if(unique(site_df$country) == 'Sudan'){

    site_data$prevalence <- site_data$prevalence |>
      mutate(run_model = ifelse(name_1 == 'South Darfur', TRUE, run_model)) |>
      mutate(run_model = ifelse(name_1 == 'West Kurdufan', TRUE, run_model))
  }

  prevalence<- site_data$prevalence |>
    select(name_1, urban_rural, iso3c, run_model) |>
    rename(site_name = name_1,
           ur= urban_rural)

  site_df<- site_df |>
    rename(site_name = name_1,
           ur= urban_rural)

  site_info<- merge(prevalence, site_df, by = c('site_name', 'ur', 'iso3c'))

  if(nrow(prevalence) < nrow(site_info)){
    stop('dropped admin units, debug')
  }

  if(scenario == 'no-vaccination' | run_all == TRUE){

    site_info<- site_info |>
      mutate(run_model = TRUE)

  }

  site_info<- site_info |>
    dplyr::filter(run_model == TRUE)


  site_info<- site_info |>
    mutate(scenario = {{scenario}},
           quick_run = {{quick_run}},
           parameter_draw = {{parameter_draw}})


  Encoding(site_info$site_name) <- "UTF-8"
  site_info$site_name<- iconv(site_info$site_name, from="UTF-8", to="ASCII//TRANSLIT")

  if (test == TRUE) {

    site_info<- site_info[1:2,]

  }
  sites<- purrr::map(.x = c(1:nrow(site_info)), .f= ~ site_info[.x,])

  return(sites)
}


