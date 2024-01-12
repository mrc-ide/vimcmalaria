



# metadata functions  ----------------------------------------------------------
remove_duplicate_reports<- function(report_name, parameter_map, day= NULL){
  # check if you have run this report before; if so remove from list of parameters to run
  # note you may want to rerun a report with the same parameters if you have changed source code; in that case do not use this function
  
  meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'), options = orderly2::orderly_search_options(allow_remote = TRUE))
  
  meta<- meta|>
    tidyr::separate(col = id, into = c('date', 'other'), sep = '-')|>
    mutate(date= as.numeric(date))
  
  if(day){
    
    meta<- meta |>
      filter(date >= day)
  }
  
  unique(lapply(meta$parameters, names))
  
  nms <- names(meta$parameters[[1]])
  pars <- do.call("data.frame", setNames(lapply(nms, function(nm) sapply(meta$parameters, function(x) x[[nm]])), nms))
  
  # observations that are in the parameter map, but have not already been run (in report metadata)
  # these are the observations you will actually want to run
  diff<- setdiff(parameter_map, pars)
  
  
  return(diff)
}



generate_parameter_map_for_next_report<- function(report_name, parameter_map, day= NULL){
  
  # before you run a report, check that the preceding reports have already been run
  
  meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'),  options = orderly2::orderly_search_options(allow_remote = TRUE))
  
  meta<- meta|>
    tidyr::separate(col = id, into = c('date', 'other'), sep = '-')|>
    mutate(date= as.numeric(date))
  
  if(day){
    
    meta<- meta |>
      filter(date >= day)
  }
  
  unique(lapply(meta$parameters, names))
  nms <- names(meta$parameters[[1]])
  pars <- do.call("data.frame", setNames(lapply(nms, function(nm) sapply(meta$parameters, function(x) x[[nm]])), nms))
  
  # pars contains completed reports
  # these are the observations you will actually want to run
  can_run<- intersect(parameter_map, pars)
  
  return(can_run)
  
}


# launch reports  --------------------------------------------------------------
run_country_report<- function(countries, 
                              report_name,
                              path){
  
  orderly2::orderly_run(report_name,
                        list(
                          iso3c = countries$iso3c,
                          description = countries$description,
                          scenario = countries$scenario,
                          parameter_draw = countries$parameter_draw,
                          quick_run = countries$quick_run),
                        root = path)
  
  message('report complete')
  
}

