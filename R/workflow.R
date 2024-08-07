#' Return a list of the reports that have already completed from orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @export
completed_reports<- function(report_name){


  meta <- orderly2::orderly_metadata_extract(name = report_name, extract = c('time', 'parameters'),  options = orderly2::orderly_search_options(allow_remote = TRUE))

  meta<- meta |>
    mutate(directory_name = id) |>
    tidyr::separate(col = id, into = c('date', 'time'), sep = '-')|>
    mutate(date= as.numeric(date)) |>
    mutate(date_time = as.numeric(paste0(date, time)))

  meta<- data.table(meta)
  meta<- meta[, index:= c(1:.N) ]


  unique(lapply(meta$parameters, names))
  nms <- names(meta$parameters[[1]])
  pars <- do.call("data.frame", stats::setNames(lapply(nms, function(nm) sapply(meta$parameters, function(x) x[[nm]])), nms))
  pars<- data.table(pars)
  pars<- pars[, index:= c(1:.N)]

  meta<- meta |>
    select(directory_name, index, date_time)
  map<- merge(pars, meta, by = 'index')
  map<- map |>
    select(-index)

  return(map)
}


#'  Make a map of input parameters for VIMC modelling
#' @param iso3cs  countries to run models for you
#' @param scenarios  scenarios to run models for. Default is all scenarios for the current round
#' @param gfa global fund assumptions for coverage of other interventions (true or false)
#' @param description reason for model run
#' @param parameter_draws draws to run model for
#' @param quick_run quick run setting (boolean)
#' @export
make_parameter_map<- function(iso3cs,
                              scenarios =  c('no-vaccination',
                                             'malaria-r3-bluesky',
                                             'malaria-r3-default',
                                             'malaria-r3-r4-bluesky',
                                             'malaria-r3-r4-default',
                                             'malaria-rts3-bluesky',
                                             'malaria-rts3-default',
                                             'malaria-rts3-rts4-bluesky',
                                             'malaria-rts3-rts4-default'),
                              gfa,
                              description,
                              parameter_draws,
                              quick_run){

  country_map<- data.table('iso3c' = iso3cs) |>
    mutate(description = description,
           quick_run = quick_run,
           gfa= gfa)

  full_map<- data.table()

  for (scen in scenarios){
    for (draw in parameter_draws){
      for(gfas in gfa){
        subset<- country_map|>
          mutate(scenario = scen,
                 parameter_draw = draw,
                 gfa = gfas)

      }


      full_map<- rbind(subset, full_map)
    }}

  full_map<- merge(full_map, site_counts, by = c('iso3c', 'scenario'))
  full_map<- data.table::setorder(full_map, parameter_draw, -site_number)

  full_map<- full_map |>
    mutate(site_number = ifelse(site_number > 32, 32, site_number))

  return(full_map)
}


#' Return a list of the reports that have already completed from orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @param map  parameter map for reports you would like to crossreference
#' @param date_time check if reports completed after a certain date, format YYYYMMDDHHMMSS
#' @export
check_reports_completed<- function(report_name, map, date_time){
  map<- map |> select(-site_number)

  completed<- completed_reports(report_name)
  completed<- completed |>
    select(-directory_name)|>
    dplyr::filter(date_time >= {{date_time}})|>
    select(-date_time)

  intersection<- intersect(map, completed)

  return(intersection)

}


#' Return a list of the reports that are not a rerun based on orderly metadata
#' @param report_name  name of orderly report to extract metadata from
#' @param map  parameter map for reports you would like to crossreference
#' @param date_time check if reports completed after a certain date, format YYYYMMDDHHMMSS
#' @export
check_not_a_rerun<- function(report_name, map, date_time){

  counts<- map |>
    select(scenario, iso3c, site_number)
  counts<- unique(counts)
  map<- map |> select(-site_number)

  completed<- completed_reports(report_name)
  completed<- completed |>
    select(-directory_name)|>
    dplyr::filter(date_time >= {{date_time}}) |>
    select(-date_time)

  different<- setdiff(map, completed)
  different<- merge(different, counts, by = c('scenario', 'iso3c'))

  return(different)

}


#' Loop through reports locally based on parameters in map
#' @param report_name  name of orderly report
#' @param map  parameter map for reports
#' @export
run_local_reports<- function(map, report_name){

  if('site_number' %in% colnames(map)){
    map$site_number<- NULL
  }
  for(index in c(1:nrow(map))){

    print(index)
    message(index)
    params<- as.list(map[index,])
    orderly2::orderly_run(name = report_name, parameters = params)

  }

  message('done')
}


#' Submit jobs by core
#' @param core number of cores to request for job
#' @param dt   parameter map for reports
#' @param test if test is true, do not request additional cores-- just to see if models complete
#' @export
submit_by_core<- function(core, dt, test= FALSE){

  dt<- dt |>
    dplyr::filter(site_number == core)

  message(unique(dt$site_number))

  if(test == TRUE){
    dt <- dt |> select(-site_number)

    hipercow::task_create_bulk_expr(
      orderly2::orderly_run(
        "process_country",
        parameters = list(iso3c = iso3c,
                          description = description,
                          quick_run = quick_run,
                          scenario = scenario,
                          parameter_draw = parameter_draw,
                          gfa= gfa)),
      dt)

  }else{

    hipercow::task_create_bulk_expr(
      orderly2::orderly_run(
        "process_country",
        parameters = list(iso3c = iso3c,
                          description = description,
                          quick_run = quick_run,
                          scenario = scenario,
                          parameter_draw = parameter_draw,
                          gfa= gfa)),
      dt,
      resources = hipercow::hipercow_resources(cores = unique(dt$site_number)))

  }

  message('submitted')
}


#' Pull metadata from process country report
#' @param iso3c         country code
#' @param description   reason for model run
#' @param quick_run     quick run setting
#' @export
pull_most_recent_output<- function(iso3c, description, quick_run){
  completed<- completed_reports('process_country') |>
    dplyr::filter(iso3c == {{iso3c}},
           description == {{description}},
           quick_run == {{quick_run}}) |>
    dplyr::arrange(dplyr::desc(date_time)) |>
    dplyr::distinct(iso3c, scenario, quick_run, parameter_draw, description, gfa, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, scenario, parameter_draw)


  return(completed)
}


#' Pull metadata from postprocessing report
#' @param iso3c         country code
#' @param description   reason for model run
#' @param quick_run     quick run setting
#' @export
pull_postprocessed_output<- function(iso3c, description, quick_run){
  completed<- completed_reports('postprocessing') |>
    dplyr::filter(iso3c == {{iso3c}},
                  description == {{description}},
                  quick_run == {{quick_run}}) |>
    dplyr::arrange(dplyr::desc(date_time)) |>
    dplyr::distinct(iso3c, quick_run, description, .keep_all = TRUE) |>
    dplyr::arrange(iso3c)


  return(completed)
}

#' Pull site level processed output based on metadata input
#' @param index           observation in metadata df
#' @param map             metadata df
#' @param output_filepath filepath where outputs live
#' @export
get_site_output<- function(index, map, output_filepath){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  draw<- metadata$parameter_draw

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/outputs.rds'))                  # get output file
  sites<- data.table::rbindlist(lapply(output$site_output, function(x) return(x$processed_output))) #pull out processed site_level output
  sites<- sites |>
    mutate(parameter_draw = draw)
  return(sites)
}

#' Pull site level processed output based on metadata input
#' @param index           observation in metadata df
#' @param map             metadata df
#' @param output_filepath filepath where outputs live
#' @export
get_dose_output<- function(index, map, output_filepath){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  draw<- metadata$parameter_draw

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/dose_output.rds'))                  # get output file
  return(output)
}

#' Pull site level output based on metadata input
#' @param index           observation in metadata df
#' @param map             metadata df
#' @param output_filepath filepath where outputs live
#' @export
get_raw_output<- function(index, map, output_filepath){

  metadata<- map[ index,]

  directory<- metadata$directory_name
  draw<- metadata$parameter_draw

  message(directory)

  output<- readRDS(paste0(output_filepath, directory, '/outputs.rds'))                  # get output file
  sites<- dplyr::bind_rows(lapply(output$site_output, function(x) return(x$raw_output))) #pull out processed site_level output
  sites<- sites |>
    mutate(parameter_draw = draw)
  return(sites)

}


compile_diagnostics<- function(descrip, date_time){

  completed<- completed_reports('diagnostics')
  completed<- completed[description == {{descrip}} & date_time>= {{date_time}}] |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description)


  copy_report<- function(index, map){

    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    file.copy(from= paste0('J:/VIMC_malaria/archive/diagnostics/', directory_name, '/diagnostic_report_', iso3c, '.html'),
              to= paste0('J:/VIMC_malaria/diagnostics/', iso3c, '.html'),
              overwrite = TRUE)
  }

  lapply(c(1:nrow(completed)), copy_report, map = completed)
}

#' Pull final outputs from workflow
#' @param descrip         description of runs to pull
#' @export
compile_final_outputs<- function(descrip){

  completed<- completed_reports('postprocessing')
  completed<- completed[description == {{descrip}}] |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description)

  pull_output<- function(index, map){

    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- rbindlist(readRDS(paste0('J:/VIMC_malaria/archive/postprocessing/', directory_name, '/final_output.rds')))
    return(output)
  }

  outputs<- rbindlist(lapply(c(1:nrow(completed)), pull_output, map = completed))
}

#' Pull final outputs from workflow
#' @param descrip         description of runs to pull
#' @export
compile_dose_outputs<- function(descrip, iso){

  completed<- completed_reports('postprocessing')

  completed<- completed[description == {{descrip}}] |>
    dplyr::arrange(desc(date_time)) |>
    dplyr::distinct(iso3c, description, .keep_all = TRUE) |>
    dplyr::arrange(iso3c, description)

  if(iso!= 'all'){
    completed<- completed |>

    dplyr::filter(iso3c == iso)
  }
  pull_dose_output<- function(index, map){

    message(index)
    map<- map[ index,]
    directory_name<- map$directory_name
    iso3c<- map$iso3c
    output<- readRDS(paste0('J:/VIMC_malaria/archive/postprocessing/', directory_name, '/dose_output.rds'))


    # then sum all doses + cases + deaths averted up to country level by year
    output<- output |>
      group_by(year, scenario) |>
      summarise(cases_averted = sum(cases_averted),
                deaths_averted = sum(deaths_averted),
                doses_total = sum(doses_total),
                fvp = sum(fvp),
                .groups = 'keep') |>
      filter(doses_total != 0) |>
      mutate(iso3c = iso3c)


    return(output)
  }

  outputs<- rbindlist(lapply(c(1:nrow(completed)), pull_dose_output, map = completed))
}

