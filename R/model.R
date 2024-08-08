#' Run model for site of interest
#' @param   model_input      list with input parameters and identifying info
#' @returns model output
#' @export
run_model<- function(model_input){
  message('running the model')

  params <- model_input$param_list
  params$progress_bar <- TRUE

  set.seed(56)

  model <- retry::retry(
    malariasimulation::run_simulation(timesteps = params$timesteps,
                                      parameters = params),
    max_tries = 5,
    when = 'error reading from connection|embedded nul|unknown type',
    interval = 3
  )

  # add identifying information to output
  model <- model |>
    dplyr::mutate(site_name = model_input$site_name,
           urban_rural = model_input$ur,
           iso = model_input$iso3c,
           description = model_input$description,
           scenario = model_input$scenario,
           gfa = model_input$gfa,
           parameter_draw = model_input$parameter_draw,
           population = model_input$pop_val,
           burnin = model_input$burnin)

  # save model runs somewhere
  message('saving the model')
  return(model)
}



#' Run baseline model for the VIMC scenarios (no vaccination, 15 years of burnin + )
#' @param   model_input      list with input parameters and identifying info
#' @returns model output
#' @export
run_baseline_model<- function(model_input){

  message('running the model')

  params <- model_input$param_list
  params$progress_bar <- TRUE

  set.seed(56)

    first_phase <- retry::retry(
      malariasimulation:::run_resumable_simulation(timesteps = 365*(23 +15), # including burn-in period of 15 years
                                                   parameters = params),
      max_tries = 5,
      when = 'error reading from connection|embedded nul|unknown type',
      interval = 3
    )

    # save model runs somewhere
    message('saving the model')

    first_phase$id<- paste0(model_input$site_name, '_', model_input$ur)

    return(first_phase)
}


#' Run baseline model for the VIMC scenarios (no vaccination, 15 years of burnin + )
#' @param   model_input           list with input parameters and identifying info
#' @param   baseline_outputs      resumable simulations for burn-in period/ 2000-2022
#' @returns model output
#' @export
run_scenario_model<- function(model_input, baseline_outputs){

  message('running the model')

  params <- model_input$param_list
  params$progress_bar <- TRUE

  site_ur<- paste0(model_input$site_name, '_', model_input$ur)
  set.seed(56)

  ids <- data.table::data.table()
  for (item in c(1:length(baseline_outputs))) {
    subset <- baseline_outputs[[item]]

    id <- data.table::data.table('id' = subset$id)

    ids <- rbind(ids, id, fill = T)
  }

  # find the index of the output which contains site of interest
  index <- which(ids$id == site_ur)

  # pull simulation state and data for this index
  first_phase <- baseline_outputs[[index]]

  # run simulation for remaining period
  second_phase <- malariasimulation:::run_resumable_simulation(
    timesteps = params$timesteps,
    params,
    initial_state = first_phase$state,
    restore_random_state = TRUE
  )

  # bind output from first and second phase together
  model <- dplyr::bind_rows(first_phase$data, second_phase$data)

  # add identifying information to output
  model <- model |>
    dplyr::mutate(
      site_name = model_input$site_name,
      urban_rural = model_input$ur,
      iso = model_input$iso3c,
      description = model_input$description,
      scenario = model_input$scenario,
      gfa = model_input$gfa,
      parameter_draw = model_input$parameter_draw,
      population = model_input$pop_val,
      burnin = model_input$burnin
    )


return(model)
}


