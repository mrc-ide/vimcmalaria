#' Run model for site of interest
#' @param   model_input      list with input parameters and identifying info
#' @returns model output
#' @export
run_model<- function(model_input){
  message('running the model')
  
  params <- model_input$param_list
  params$progress_bar <- TRUE
  timesteps <<- model_input$param_list$timesteps
  
  model <- malariasimulation::run_simulation(timesteps = params$timesteps,
                                             parameters = params)
  
  # add identifying information to output
  model <- model |>
    mutate(site_name = model_input$site_name,
           urban_rural = model_input$ur,
           iso = model_input$iso3c,
           description = model_input$description, 
           scenario = model_input$scenario,
           parameter_draw = model_input$parameter_draw,
           population = model_input$pop_val,
           burnin = model_input$burnin)
  
  # save model runs somewhere
  message('saving the model')
  return(model)
}
