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
                                  country= site$country,
                                  site_data = site_data,
                                  coverage_data = vimc_input$coverage_input,
                                  scenario = site$scenario,
                                  iso3c = site$iso3c,
                                  parameter_draw = site$parameter_draw,
                                  quick_run = site$quick_run)


  model<- run_model(model_input)
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




#' make an analysis map of input parameters for vaccine modelling run
#' @param site_df   analysis map with input parameters
#' @param site_data site data
#' @param test      boolean-- if true, only run analysis for two test sites. Good for quick tests of code functionality
#' @param run_all   run all sites regardless of pfpr (boolean)
#' @returns analysis map to be used as an input for analyse_site
#' @export
make_analysis_map<- function(site_df,
                             test,
                             run_all){


  
  site_info<- prev |> 
    rename(ur= urban_rural,
          site_name = name_1)

    site_df<- site_df |> 
    rename(ur= urban_rural,
          site_name = name_1)

  if(scenario == 'no-vaccination' | run_all == TRUE){
    site_info<- site_info |>
      mutate(run_model = TRUE)
  }

  # Encoding(site_info$site_name) <- "UTF-8"
  # site_info$site_name<- iconv(site_info$site_name, from="UTF-8", to="ASCII//TRANSLIT")

  site_info<- site_info |>
    dplyr::filter(run_model == TRUE) |>
    mutate(scenario = {{scenario}},
           quick_run = {{quick_run}},
           parameter_draw = {{parameter_draw}})


  site_info<- merge(site_info, site_df, by = c('site_name', 'ur', 'iso3c', 'country'))

  if (test == TRUE) { site_info<- site_info[1:2,] }

  sites<- purrr::map(.x = c(1:nrow(site_info)), .f= ~ site_info[.x,])

  return(sites)
}


