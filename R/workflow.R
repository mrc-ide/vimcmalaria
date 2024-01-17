

#' check if you have run this report before; if so remove from list of parameters to run
#' note you may want to rerun a report with the same parameters if you have changed source code;
#'  in that case do not use this function
#' @param report_name  name of orderly report to check
#' @param parameter_map  map of parameters for model runs to check against
#' @param day if you would only like to check reports completed after a certain day, MMDDYYYY format
#' @return parameter map of reports that have not been run before
#' @export
remove_duplicate_reports<- function(report_name, parameter_map, day= NULL){
  
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


#' before you run a report, check that the preceding reports have already been run
#' @param report_name  name of orderly report to check
#' @param parameter_map  map of parameters for model runs to check against
#' @param day if you would only like to check reports completed after a certain day, MMDDYYYY format
#' @return parameter map of reports that have not been run before
#' @export
generate_parameter_map_for_next_report<- function(report_name, parameter_map, day= NULL){
  
  
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


