#' Function to connect to UPGo server
#'
#' \code{upgo_disconnect} closes an open connection to the UPGo server and
#' removes the associated tables from the global environment.
#'
#' A function for disconnecting from the UPGo server and any associated tables.
#' An open connection with the name `con` will be removed from the special upgo
#' package environment (`.upgo_env`), as will tables named `property_remote`,
#' `daily_remote`, `daily_inactive_remote`, `host_remote`,
#' `host_inactive_remote`, `review_remote`, `review_user_remote`,
#' `review_text_remote`, `geolocation_remote` and `ha_mapping_remote`.
#'
#' @return The function returns no output, but removes objects from the global
#' environment.
#' @export

upgo_disconnect <- function() {

    if (exists("property_remote", envir = .GlobalEnv)) {
    rm("property_remote", envir = .GlobalEnv)}

  if (exists("daily_remote", envir = .GlobalEnv)) {
    rm("daily_remote", envir = .GlobalEnv)}

  if (exists("daily_inactive_remote", envir = .GlobalEnv)) {
    rm("daily_inactive_remote", envir = .GlobalEnv)}

  if (exists("host_remote", envir = .GlobalEnv)) {
    rm("host_remote", envir = .GlobalEnv)}

  if (exists("host_inactive_remote", envir = .GlobalEnv)) {
    rm("host_inactive_remote", envir = .GlobalEnv)}

  if (exists("review_remote", envir = .GlobalEnv)) {
    rm("review_remote", envir = .GlobalEnv)}

  if (exists("review_user_remote", envir = .GlobalEnv)) {
    rm("review_user_remote", envir = .GlobalEnv)}

  if (exists("review_text_remote", envir = .GlobalEnv)) {
    rm("review_text_remote", envir = .GlobalEnv)}

  if (exists("geolocation_remote", envir = .GlobalEnv)) {
    rm("geolocation_remote", envir = .GlobalEnv)}

  if (exists("ha_mapping_remote", envir = .GlobalEnv)) {
    rm("ha_mapping_remote", envir = .GlobalEnv)}

  if (exists("con", envir = .upgo_env)) {
    rm("con", envir = .upgo_env)}

}

