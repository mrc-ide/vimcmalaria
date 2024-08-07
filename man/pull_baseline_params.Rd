% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameterize.R
\name{pull_baseline_params}
\alias{pull_baseline_params}
\title{parameterize site + urbanicty of interest}
\usage{
pull_baseline_params(
  site_name,
  ur,
  iso3c,
  site_data,
  parameter_draw,
  quick_run
)
}
\arguments{
\item{site_name}{name of site}

\item{ur}{urbanicity, urban or rural}

\item{iso3c}{country code}

\item{site_data}{site file}

\item{parameter_draw}{parameter draw value}

\item{quick_run}{quick_run setting (boolean)}
}
\value{
site file with additional variables 'rtss_coverage', 'rtss_booster_coverage', 'r21_coverage', 'r21_booster_coverage'
}
\description{
parameterize site + urbanicty of interest
}
