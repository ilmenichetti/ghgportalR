

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
#' reply <- get_token(uname = "myusername", upass = "mypassword", access_list = create_access_list() )
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
#' This function is utilized to get the projects assigned to the user
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Jani Anttila, \email{jani.anttila@@luke.fi}
#' @param token output straight from the function \code{\link{get_token_LM}}.
#' @return a data.frame (which should have 9 variables:  "id", "worker", "worker_name", "project", "project_name", "author", "author_name", "date", "active"). This might change in different versions of the database, eventually contact the package maintainer.
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


#' Get the list of measurement sets
#'
#' This function is utilized to get the list of measurement sets in a given project
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Jani Anttila, \email{jani.anttila@@luke.fi}
#' @param token output straight from the function \code{\link{get_token_LM}}.
#' @param project_id id of the project the user wants to query
#' @return a data frame of 17 variables, and as many rows as the measurements events
#' @details each row corresponds to a measurement set, with the relative metadata
#' @examples
#' all_meas <- get_meas(mytoken, project_id = 1)
#' @export
get_meas <- function(token, project_id){
  access_list = create_access_list()
  req_url <- paste(access_list$protocol ,"://",access_list$ip ,access_list$meas_addr ,project_id ,sep="")
  req <- request(req_url) |> req_headers(Authorization = paste("Token",token))
  resp <- req_perform(req)
  resp_json <- resp |> resp_body_json()
  meas <- do.call(rbind.data.frame, resp_json$measurements)
  return(meas)
}



#' Get the readings for one series of measurements
#'
#' This function is utilized to gccess one specific series of measurements
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Jani Anttila, \email{jani.anttila@@luke.fi}
#' @param token output straight from the function \code{\link{get_token_LM}}.
#' @param meas_id password
#' @return a list of lists
#' @details Things here gets more complicated
#' @examples
#' all_meas <- get_meas(mytoken, project_id = 1)
#' @export
get_series <- function(token, meas_id){
  access_list = create_access_list()
  req_url <- paste(access_list$protocol,"://",access_list$ip,access_list$series_addr,meas_id,sep="")
  req <- request(req_url) |> req_headers(Authorization = paste("Token",token))
  resp <- req_perform(req)
  resp_json <- resp |> resp_body_json()
  return(resp_json$series)
}




#' Get all the subsite id in a series
#'
#' This function is utilized to extract all the subsite ID in a series
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @param series object from the function \code{\link{get_series}}.
#' @return a data frame
#' @details Things here gets more complicated
#' @examples
#' subsite_info <- subsiteID_names(series)
#' @export
# Function to extract subsiteID and names
subsiteID_names <- function(series) {
  subsite_info <- do.call(rbind, lapply(series, function(x) {
    data.frame(subsiteid = x$subsiteid, siteid = x$siteid, stringsAsFactors = FALSE)
  }))
  return(subsite_info)
}


#' Get all the point id in a series
#'
#' This function is utilized to extract all the points (collars) ID in a series
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Jani Anttila, \email{jani.anttila@@luke.fi}
#' @param series object from the function \code{\link{get_series}}.
#' @return a data frame
#' @details Things here gets more complicated
#' @examples
#' points_info <- point_names(series)
#' @export
# Function to extract subsiteID and names
point_names <- function(series) {
  subsite_info <- do.call(rbind, lapply(series, function(x) {
    data.frame(subsiteid = x$subsiteid, siteid = x$siteid, point = x$point, stringsAsFactors = FALSE)
  }))
  return(subsite_info)
}


#' Filter all the elements in a series based on a subsite id
#'
#' This function is utilized to extract all the elements (measurements time series) in a series that correspond to a specific subsite ID
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @param series object from the function \code{\link{get_series}}.
#' @return a list of lists as the function \code{\link{get_token_LM}}, but subsetted for a single subsite ID.
#' @details Things here gets more complicated
#' @examples
#' subsiteID_filter(series, subsiteID_value="DP-MC")
#' @export
# Function to filter based on subsiteID
subsiteID_filter <- function(series, subsiteID_value) {
  filtered_list <- lapply(series, function(x) {
    if (x$subsiteid == subsiteID_value) {
      return(x)
    }
  })

  # Remove NULL elements from the filtered list
  filtered_list <- filtered_list[!sapply(filtered_list, is.null)]
  return(filtered_list)
}



#' Reformat one single element of a series in a more readable format
#'
#' This function is utilized to extract all the elements (measurements time series) in a series that correspond to a specific subsite ID
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @param series object from the function \code{\link{get_series}}.
#' @return a list for one single element of the series
#' @details Things here gets more complicated
#' @examples
#' process_single_series(series_list=series, 11)
#' @export
# Function to filter based on subsiteID
process_single_series <- function(series ,index){
  part <- series[[index]]
  values <- unlist(part$values)
  part$values <- values
  return(part)
}




#'  Check all the measurements in a certain project to see if they are available or if there are problems
#'
#' This function is utilized to extract all the available series in a project. In many cases there might be issues, and this function might help to identify where.
#'
#' @maintainer Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @author Lorenzo Menichetti, \email{lorenzo.menichetti@@luke.fi}
#' @param meas object from the function \code{\link{get_meas}}.
#' @param token output straight from the function \code{\link{get_token_LM}}.
#' @return a list of two elements, the first is a list of all the available series (a list of lists), and the second is the same output than \code{\link{get_meas}} with an attached column with the availability check. The function will also print which series are not available.
#' @details Things here gets more complicated
#' @examples
#' available <- check_meas(meas = all_meas, token = mytoken)
#' available_series <- available$available_series
#' measurements_check <- available$meas_check
#' @export
check_meas <- function(token, meas){
  counter=1
  df_empty <- setNames(data.frame(matrix(ncol = length(names(meas))+1, nrow = 0)), c(names(meas), "empty"))
  pb = txtProgressBar(min = 0, max = length(meas$id), initial = 0)

  available_series <- list()

  for(i in meas$id){
    series = get_series(token = token, meas_id = i)
    df_empty[counter,1:length(names(meas))] <- meas[meas$id == i,]
    if(length(series)>0){
      df_empty[counter,"empty"] = FALSE
      available_series[[length(available_series) + 1]] <- series
    } else {
      df_empty[counter,"empty"] = TRUE
    }
    counter=counter+1
    setTxtProgressBar(pb,counter)
  }

  print(paste("The following id are unaccessible: ",  paste(df_empty[df_empty$empty == T,]$id, collapse = ", ")))
  print(paste("The following id are available: ",  paste(df_empty[df_empty$empty == F,]$id, collapse = ", ")))
  return(list(available_series = available_series, meas_check = df_empty))

}
