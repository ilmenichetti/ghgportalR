## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ghgportalR)
library(httr2) # should be loaded automatically, here just in case...

## -----------------------------------------------------------------------------
api_username <- Sys.getenv("ghgportal_API_USERNAME")
api_password <- Sys.getenv("ghgportal_API_PASSWORD")
                           
reply <- get_token(uname = api_username, upass = api_password)

## -----------------------------------------------------------------------------
my_token <- reply$token

## -----------------------------------------------------------------------------
get_projects(my_token)

