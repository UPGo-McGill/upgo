#' Function to connect to UPGo server
#'
#' \code{upgo_disconnect} closes an open connection to the UPGo server and
#' removes the associated tables from the global environment.
#'
#' A function for disconnecting from the UPGo server and any associated tables.
#' An open connection with the name `con` will be removed from the global
#' environment, as will tables named `property_all`, `daily_all`, `ML_all` and
#' `reviews_all`.
#'
#' @return The function returns no output, but removes objects from the global
#' environment.
#' @export

upgo_disconnect <- function() {

  if (exists("property_all")) rm(property_all, envir = .GlobalEnv)
  if (exists("daily_all")) rm(daily_all, envir = .GlobalEnv)
  if (exists("ML_all")) rm(ML_all, envir = .GlobalEnv)
  if (exists("reviews_all")) rm(reviews_all, envir = .GlobalEnv)
  if (exists("con")) rm(con, envir = .GlobalEnv)

}

