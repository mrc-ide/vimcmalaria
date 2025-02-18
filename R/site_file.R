#' Check that the sites you seek to model have non-zero EIRs
#' @param iso3c  Country code
#' @param sites  sites component of site file
#' @return sites component of site file with only sites that will be modelled
#' @export
remove_zero_eirs<- function(iso3c, sites){
  eirs<- data.table::data.table(sites$eir)  # sites for country of interest
  eirs<- eirs[eirs$sp == 'pf' & eirs$eir == 0, ]
  remove<- eirs[, c('name_1', 'urban_rural')]

  site<- sites$sites
  if (nrow(remove) > 0){
    for (i in 1:nrow(remove)) {
      message(paste0('removing site ', i))
      site<- site[!(site$name_1== remove[i, name_1] & site$urban_rural == remove[i, urban_rural]), ]
    }
  } else{
    message('No zero eir sites to remove')
  }
  return(site)
}

#' Check that the EIR of the site of interest is not zero
#' @param site   site file
#' @return sites component of site file with only sites that will be modelled
#' @export
check_eir<- function(site){
  if(site$eir$eir[[1]] == 0){

    stop('Can not model this site beause PfPR EIR is equal to zero. Note this site/ urbanicity combination and exclude from future model runs.')

  }
}


