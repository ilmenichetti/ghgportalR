% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API_wrappers.R
\name{get_series}
\alias{get_series}
\title{Get the readings for one series of measurements}
\usage{
get_series(token, meas_id)
}
\arguments{
\item{token}{output straight from the function \code{\link{get_token_LM}}.}

\item{meas_id}{password}
}
\value{
a list of lists if the measurement is available, otherwise the error message relative to the series
}
\description{
This function is utilized to gccess one specific series of measurements
}
\details{
Things here gets more complicated
}
\examples{
all_meas <- get_meas(mytoken, project_id = 1)
}
\author{
Jani Anttila, \email{jani.anttila@luke.fi}
}
