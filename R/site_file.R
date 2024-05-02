#' Check that the sites you seek to model have non-zero EIRs
#' @param iso3c  Country code
#' @param sites  sites component of site file
#' @return sites component of site file with only sites that will be modelled
#' @export
remove_zero_eirs<- function(iso3c, sites){
  eirs<- data.table::data.table(sites$eir)  # sites for country of interest
  eirs<- eirs[eirs$spp == 'pf' & eirs$eir == 0, ]
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

#' Extract a single site input from a country site file
#' @param site_file  Country site file
#' @param site_name  name of site to extract
#' @param ur urbanicity of site to extract
#' @return Single site
#' @export
extract_site <- function(site_file, site_name, ur){

  sites<- data.table::data.table(site_file$sites)
  Encoding(sites$name_1) <- "UTF-8"

  sites$name_1<- iconv(sites$name_1, from="UTF-8", to="ASCII//TRANSLIT")

  index_site <- sites[name_1== site_name & urban_rural== ur]

  to_mod <- c("sites", "interventions", "pyrethroid_resistance", "population",
              "vectors", "seasonality", "prevalence", "eir")

  site <- site_file

  for(level in to_mod){
    mod<- site[[level]]
    Encoding(mod$name_1) <- "UTF-8"

    mod$name_1<- iconv(mod$name_1, from="UTF-8", to="ASCII//TRANSLIT")

    mc <- intersect(colnames(index_site), colnames(mod))
    site[[level]] <- dplyr::left_join(index_site, mod, by = mc)
  }

  return(site)
}

