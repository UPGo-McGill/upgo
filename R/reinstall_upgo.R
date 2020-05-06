#' Function to reinstall the upgo package from GitHub
#'
#' @param branch A character string identifying the branch to be installed, if
#' not the master branch (default).
#' @return There is no tangible output to the function; it simply detaches and
#' updates the upgo package.
#' @export

reinstall_upgo <- function(branch = NULL) {

  if (!missing(branch)) {
    path <- glue::glue("UPGo-McGill/upgo@{branch}")
  } else path <- "UPGo-McGill/upgo"

  if("upgo" %in% (.packages())){
    detach("package:upgo", unload=TRUE)
  }

  remotes::install_github(path, upgrade = "never")
}
