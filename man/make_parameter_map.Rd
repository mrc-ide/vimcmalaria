% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workflow.R
\name{make_parameter_map}
\alias{make_parameter_map}
\title{Make a map of input parameters for VIMC modelling}
\usage{
make_parameter_map(
  iso3cs,
  scenarios = c("no-vaccination", "malaria-r3-bluesky", "malaria-r3-default",
    "malaria-r3-r4-bluesky", "malaria-r3-r4-default", "malaria-rts3-bluesky",
    "malaria-rts3-default", "malaria-rts3-rts4-bluesky", "malaria-rts3-rts4-default"),
  gfa,
  description,
  parameter_draws,
  quick_run
)
}
\arguments{
\item{iso3cs}{countries to run models for you}

\item{scenarios}{scenarios to run models for. Default is all scenarios for the current round}

\item{gfa}{global fund assumptions for coverage of other interventions (true or false)}

\item{description}{reason for model run}

\item{parameter_draws}{draws to run model for}

\item{quick_run}{quick run setting (boolean)}
}
\description{
Make a map of input parameters for VIMC modelling
}
