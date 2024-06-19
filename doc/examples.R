## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# Suppress the vignette title check warning
options(rmarkdown.html_vignette.check_title = FALSE)
library(ghgportalR)
library(httr2) # should be loaded automatically, here just in case...

## ---- results=FALSE-----------------------------------------------------------
api_username <- Sys.getenv("ghgportal_API_USERNAME")
api_password <- Sys.getenv("ghgportal_API_PASSWORD")
                           
reply <- get_token(uname = api_username, upass = api_password)

## -----------------------------------------------------------------------------
my_token <- reply$token

## -----------------------------------------------------------------------------
get_projects(my_token)

## -----------------------------------------------------------------------------
all_meas <- get_meas(my_token, project_id = 1)

## -----------------------------------------------------------------------------
available <- check_meas(meas = all_meas, token = my_token)
available_series <- available$available_series
measurements_check <- available$meas_check

## -----------------------------------------------------------------------------
available_id <- measurements_check[measurements_check$empty==F,]$id
series <- get_series(token = my_token, meas_id = available_id[1])

## -----------------------------------------------------------------------------
subsite_info <- subsiteID_names(series)
subsite_info

## -----------------------------------------------------------------------------
points_info <- point_names(series)
points_info

## -----------------------------------------------------------------------------
filtered_series_bysubsiteID <- subsiteID_filter(series, subsiteID_value="DP-MC")

## -----------------------------------------------------------------------------
process_single_series(series, 11)

