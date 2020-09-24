#' Function to connect to UPGo server
#'
#' \code{upgo_connect} opens a connection to the UPGo server and optionally adds
#' tables from the server to the global environment.
#'
#' A function for connecting to the UPGo server. For the function to work, the
#' variables `PGHOST`, `PGUSER` and `PGPASSWORD` need to be defined in the
#' user's .Renviron file. A connection with the name `con` will be added to the
#' global environment. The function will also optionally add tables from the
#' server to the global environment.
#'
#' @param property A logical scalar. Should the property table (named
#' `property_remote`) be added to the global environment?
#' @param daily A logical scalar. Should the daily table (named `daily_remote`)
#' be added to the global environment?
#' @param daily_inactive A logical scalar. Should the daily_inactive table
#' (named `daily_inactive_remote`) be added to the global environment?
#' @param host A logical scalar. Should the host table (named `host_remote`) be
#' added to the global environment?
#' @param host_inactive A logical scalar. Should the host_inactive table (named
#' `host_remote_all`) be added to the global environment?
#' @param review A logical scalar. Should the review table (named
#' `review_remote`) be added to the global environment?
#' @param review_user A logical scalar. Should the review_user table (named
#' `review_user_remote`) be added to the global environment?
#' @param review_text A logical scalar. Should the review_text table (named
#' `review_text_remote`) be added to the global environment?
#' @param geolocation A logical scalar. Should the geolocation table (named
#' `geolocation_remote`) be added to the global environment?
#' @param ha_mapping A logical scalar. Should the ha_mapping table (named
#' `ha_mapping_remote`) be added to the global environment?
#' @param remote A logical scalar. Should a remote connection be opened even if
#' only local tables are being connected?
#' @return The function returns no output, but makes assignments to the global
#' environment.
#' @export

upgo_connect <- function(property = TRUE, daily = TRUE, daily_inactive = FALSE,
                         host = TRUE, host_inactive = FALSE, review = FALSE,
                         review_user = FALSE, review_text = FALSE,
                         geolocation = FALSE, ha_mapping = FALSE,
                         remote = FALSE) {

  property_remote <- daily_remote <- daily_inactive_remote <- host_remote <-
    host_inactive_remote <- review_remote <- review_user_remote <-
    review_text_remote <- geolocation_remote <- ha_mapping_remote <- NULL

  # Check for .con local DB
  if (exists(".con", envir = .GlobalEnv)) local <- TRUE else local <- FALSE

  # Decide if remote connection is necessary
  if (remote | host_inactive | review | review_user | review_text |
      geolocation | !local) {
    assign("con",
           {
             RPostgres::dbConnect(
               RPostgres::Postgres(),
               host = "025wpgs.campus.mcgill.ca",
               dbname = "airdna",
               check_interrupts = TRUE)
             },
           envir = .upgo_env)
  }

  # Open local connections if possible
  if (local) {
    if (property) property_remote <<- dplyr::tbl(.con, "property")
    if (daily) daily_remote <<- dplyr::tbl(.con, "daily")
    if (daily_inactive) daily_inactive_remote <<-
        dplyr::tbl(.con, "daily_inactive")
    if (host) host_remote <<- dplyr::tbl(.con, "host")
    if (ha_mapping) ha_mapping_remote <<- dplyr::tbl(.con, "ha_mapping")

  # Otherwise open remote connections
  } else {
    if (property) property_remote <<- dplyr::tbl(.upgo_env$con, "property")
    if (daily) daily_remote <<- dplyr::tbl(.upgo_env$con, "daily")
    if (daily_inactive) daily_inactive_remote <<-
        dplyr::tbl(.upgo_env$con, "daily_inactive")
    if (host) host_remote <<- dplyr::tbl(.upgo_env$con, "host")
    if (ha_mapping) ha_mapping_remote <<-
        dplyr::tbl(.upgo_env$con, "ha_mapping")
  }

  # Open remote connections for tables only hosted remotely
  if (host_inactive) host_inactive_remote <<-
      dplyr::tbl(.upgo_env$con, "host_inactive")
  if (review) review_remote <<-
      dplyr::tbl(.upgo_env$con, "review")
  if (review_user) review_user_remote <<-
      dplyr::tbl(.upgo_env$con, "review_user")
  if (review_text) review_text_remote <<-
      dplyr::tbl(.upgo_env$con, "review_text")
  if (geolocation) geolocation_remote <<-
      dplyr::tbl(.upgo_env$con, "geolocation")

}

