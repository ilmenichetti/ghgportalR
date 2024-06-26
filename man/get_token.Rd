% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/API_wrappers.R
\name{get_token}
\alias{get_token}
\title{Get token}
\usage{
get_token(uname, upass)
}
\arguments{
\item{uname}{username}

\item{upass}{password}

\item{access_list}{output straight from the function \code{\link{multiply_numbers}}.}
}
\value{
Description of the return value.
}
\description{
This function is utilized to get the token needed to communicate with the API. The token will be regenerated every two days, so this function needs to be run relatively often.
}
\details{
Any additional details about the function.
}
\examples{
reply <- get_token(uname = "myusername", upass = "mypassword", access_list = create_access_list() )
my_token <- reply$token
}
\author{
Jani Anttila, \email{jani.anttila@luke.fi}
}
