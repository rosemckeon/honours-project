% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/store_data.R
\name{store_data}
\alias{store_data}
\title{Stores simulation data in working directory filepath}
\usage{
store_data()
}
\arguments{
\item{tmp_files}{charactor vector containing temp file path/s.}

\item{name}{character string defining the name of the filepath subfolder which countains output RDS files. (default = unique random identifier).}

\item{this_sim}{integer representing the simulation number.}

\item{filepath}{character string defining the file path where output files should be stored (relative to working directory). If this folder does not exist it will be created (default = "data").}

\item{keep_tmp}{logical value that defines whether temp files should be kept or removed (default = FALSE, cleans up tmp files that have been stored).}

\item{subdir}{character string representing the subfolder inside the simulation folder that the file should be stored in (default = NULL).}
}
\description{
Copies data from temporary directory and clears temp files to free memory.
}
\examples{
store_data()
}
\author{
Rose McKeon
}
