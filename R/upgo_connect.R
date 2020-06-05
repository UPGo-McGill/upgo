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
#' `property_all`) be added to the global environment?
#' @param daily A logical scalar. Should the daily table (named `daily_all`) be
#' added to the global environment?
#' @param daily_inactive A logical scalar. Should the daily_inactive table
#' (named `daily_inactive_all`) be added to the global environment?
#' @param host A logical scalar. Should the host table (named `host_all`) be
#' added to the global environment?
#' @param host_inactive A logical scalar. Should the host_inactive table (named
#' `host_inactive_all`) be added to the global environment?
#' @param review A logical scalar. Should the review table (named
#' `review_all`) be added to the global environment?
#' @param review_user A logical scalar. Should the review_user table (named
#' `review_user_all`) be added to the global environment?
#' @param review_text A logical scalar. Should the review_text table (named
#' `review_text_all`) be added to the global environment?
#' @param remote A logical scalar. Should a remote connection be opened even if
#' only local tables are being connected?
#' @return The function returns no output, but makes assignments to the global
#' environment.
#' @export

upgo_connect <- function(property = TRUE, daily = TRUE, daily_inactive = FALSE,
                         host = TRUE, host_inactive = FALSE, review = FALSE,
                         review_user = FALSE, review_text = FALSE,
                         remote = FALSE) {

  property_all <- daily_all <- daily_inactive_all <- host_all <-
    host_inactive_all <- review_all <- review_user_all <- review_text_all <-
    NULL

  # Check for .con local DB
  if (exists(".con", envir = .GlobalEnv)) local <- TRUE else local <- FALSE

  # Decide if remote connection is necessary
  if (remote | daily_inactive | host_inactive | review | review_user |
      review_text | !local) {
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
    if (property) property_all <<- dplyr::tbl(.con, "property")
    if (daily) daily_all <<- dplyr::tbl(.con, "daily")
    if (host) host_all <<- dplyr::tbl(.con, "host")

  # Otherwise open remote connections
  } else {
    if (property) property_all <<- dplyr::tbl(.upgo_env$con, "property")
    if (daily) daily_all <<- dplyr::tbl(.upgo_env$con, "daily")
    if (host) host_all <<- dplyr::tbl(.upgo_env$con, "host")
  }

  # Open remote connections for tables only hosted remotely
  if (daily_inactive) daily_inactive_all <<-
      dplyr::tbl(.upgo_env$con, "daily_inactive")
  if (host_inactive) host_inactive_all <<-
      dplyr::tbl(.upgo_env$con, "host_inactive")
  if (review) review_all <<-
      dplyr::tbl(.upgo_env$con, "review")
  if (review_user) review_user_all <<-
      dplyr::tbl(.upgo_env$con, "review_user")
  if (review_text) review_text_all <<-
      dplyr::tbl(.upgo_env$con, "review_text")

}

