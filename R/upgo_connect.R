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
#' @param reviews A logical scalar. Should the reviews table (named
#' `reviews_all`) be added to the global environment?
#' @return The function returns no output, but makes assignments to the global
#' environment.
#' @export

upgo_connect <- function(property = TRUE, daily = TRUE, daily_inactive = FALSE,
                         host = TRUE, host_inactive = FALSE, reviews = FALSE) {

  property_all <- daily_all <- daily_inactive_all <- host_all <-
    host_inactive_all <- reviews_all <- NULL

  con <<- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "025wpgs.campus.mcgill.ca",
    dbname = "airdna",
    check_interrupts = TRUE)

  if (property) property_all <<- dplyr::tbl(con, "property")
  if (daily) daily_all <<- dplyr::tbl(con, "daily")
  if (daily_inactive) daily_inactive_all <<- dplyr::tbl(con, "daily_inactive")
  if (host) host_all <<- dplyr::tbl(con, "host")
  if (host_inactive) host_inactive_all <<- dplyr::tbl(con, "host_inactive")
  if (reviews) reviews_all <<- dplyr::tbl(con, "reviews")

  con <- NULL
}

