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
    mutate(site_name = model_input$site_name,
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

    # add identifying information to output
    first_phase$data <- first_phase$data |>
      mutate(site_name = model_input$site_name,
             urban_rural = model_input$ur,
             iso = model_input$iso3c,
             description = model_input$description,
             gfa = model_input$gfa,
             parameter_draw = model_input$parameter_draw,
             population = model_input$pop_val,
             burnin = model_input$burnin)


    # save model runs somewhere
    message('saving the model')

    first_phase$id<- paste0(model_input$site_name, '_', model_input$ur)

    return(first_phase)
}


#' #' Run baseline model for the VIMC scenarios (no vaccination, 15 years of burnin + )
#' #' @param   model_input      list with input parameters and identifying info
#' #' @returns model output
#' #' @export
#' run_scenario_model<- function(model_input){
#'
#'   message('running the model')
#'
#'   params <- model_input$param_list
#'   params$progress_bar <- TRUE
#'
#'   set.seed(56)
#'
#'   test <- baseline_outputs[[paste]]
#'
#'   # add identifying information to output
#'   first_phase$data <- first_phase$data |>
#'     mutate(site_name = model_input$site_name,
#'            urban_rural = model_input$ur,
#'            iso = model_input$iso3c,
#'            description = model_input$description,
#'            gfa = model_input$gfa,
#'            parameter_draw = model_input$parameter_draw,
#'            population = model_input$pop_val,
#'            burnin = model_input$burnin)
#'
#'
#'   # save model runs somewhere
#'   message('saving the model')
#'
#'   return(list(paste0(model_input$site_name, '_', model_input$ur) = first_phase))
#' }


