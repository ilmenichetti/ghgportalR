

# library(httr2)
# devtools::build_manual()
# devtools::build_vignettes()  # Build the vignettes


#' Create access_list. The function is used only internally.
#'
#' The object `access_list` contains all the remote addresses needed to interact with the ghgportal API. It takes no parameters, just generates the object that will be used by all other functions in this package.
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Jani Anttila, \email{jani.anttila@@luke.fi}
#' @return A list with the following objects:
#' protocol = "https", ip = "chambers.ghgportal.luke.fi", ping_addr = "/api/ping/", wa_addr = "/api/workerassignments/", meas_addr = "/api/measurements/", series_addr = "/api/series/", flux_addr = "/api/flux/", auth_addr ="/api/auth/login/"
#' @export
create_access_list <- function() {
  access <- list( protocol = "https",
                  ip = "chambers.ghgportal.luke.fi", ping_addr = "/api/ping/",
                  wa_addr = "/api/workerassignments/", meas_addr = "/api/measurements/",
                  series_addr = "/api/series/", flux_addr = "/api/flux/", auth_addr ="/api/auth/login/"
  )

  return(access)
  }


#' Get token
#'
#' This function is utilized to get the token needed to communicate with the API. The token will be regenerated every two days, so this function needs to be run relatively often.
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Jani Anttila, \email{jani.anttila@@luke.fi}
#' @param uname username
#' @param upass password
#' @param access_list output straight from the function \code{\link{multiply_numbers}}.
#' @return Description of the return value.
#' @details Any additional details about the function.
#' @examples
#' reply <- get_token_LM(uname = "myusername", upass = "mypassword", access_list = create_access_list() )
#' my_token <- reply$token
#' @import httr2
#' @export
get_token <- function(uname, upass){
  access_list = create_access_list()
  token_url <- paste(access_list$protocol,"://",access_list$ip,access_list$auth_addr,sep="")
  token_req <- request(token_url) |> req_method("POST") |> req_auth_basic(uname, upass)
  resp <- req_perform(token_req)
  resp_json <- resp |> resp_body_json()
  print(resp_json)
  return(resp_json)
}



#' Get projects
#'
#' This function is utilized to get the projects assigned to a certain user
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Jani Anttila, \email{jani.anttila@@luke.fi}
#' @param uname username
#' @param upass password
#' @param toke output straight from the function \code{\link{get_token_LM}}.
#' @return Description of the return value.
#' @details Any additional details about the function.
#' @examples
#' get_projects(mytoken)
#' @export
get_projects <- function(token){
  access_list = create_access_list()
  req <- request(paste(access_list$protocol ,"://",access_list$ip ,access_list$wa_addr ,sep=""))
  req <- req |> req_headers(Authorization = paste("Token",token))
  resp <- req_perform(req)
  resp_json <- resp |> resp_body_json()
  wa <- do.call(rbind.data.frame, resp_json$workerassignments)
  return(wa)
}





