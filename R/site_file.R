#' Check that the sites you seek to model have non-zero EIRs
#' @param iso3c  Country code
#' @param sites  sites component of site file
#' @return sites component of site file with only sites that will be modelled
#' @export
remove_zero_eirs<- function(iso3c, sites){

  if(nrow(sites$eir) != nrow(sites$sites)){

    full_sites<- sites$sites |>
      mutate(site_ur := paste0(name_1, '_', urban_rural))
    


    full_eirs<- sites$eir |>
      mutate(site_ur := paste0(name_1, '_', urban_rural))


    no_eir<- setdiff(unique(full_sites$site_ur), unique(full_eirs$site_ur))

    remove<- full_sites |>
      filter(site_ur %in% no_eir) |>
      as.data.table()
  }

  site<- data.table(sites$sites)
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
    
    
    
    



