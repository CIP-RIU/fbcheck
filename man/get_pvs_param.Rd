% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trait_function.R
\name{get_pvs_param}
\alias{get_pvs_param}
\title{Function to obtain parameter from Participatory Varietal Selection's fieldbooks (PVS).}
\usage{
get_pvs_param(pvs_data, col_param = "Mother",
  row_param = "Plot_size_(m2)")
}
\arguments{
\item{pvs_data}{fieldbook path}

\item{col_param}{column parameter. By default is "Mother".}

\item{row_param}{row parameter. By default is ""Plot_size_(m2)"}
}
\description{
This function gets parameters or values from PVS fieldbook excel file. Do an excel scrapping.
}
