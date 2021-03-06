% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Functions.R
\name{InputData}
\alias{InputData}
\title{Check, combine and prepare input data.
Create NUTS0 coverage graphs}
\usage{
InputData(
  wdir,
  StatusDate,
  IndividuelPopulationFile = NA,
  IndividuelMCVFile = NA,
  AggregatedPopulationFile = NA,
  AggregatedMCVFile = NA,
  AdministrativeMCVFile = NA
)
}
\arguments{
\item{wdir}{Working directory.}

\item{StatusDate}{Date of status calculation.}

\item{IndividuelPopulationFile}{Individual input ;-separated population file}

\item{IndividuelMCVFile}{Individual input ;-separated vaccination file}

\item{AggregatedPopulationFile}{Aggregated input ;-separated population file}

\item{AggregatedMCVFile}{Aggregated input ;-separated vaccination file}

\item{AdministrativeMCVFile}{Administrative input ;-separated vaccination file}
}
\value{
Create a subdirectory in \code{wdir}
}
\description{
Check, combine and prepare input data.
Create NUTS0 coverage graphs
}
