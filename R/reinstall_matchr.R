#' Function to reinstall the matchr package from GitHub
#'
#' @param branch A character string identifying the branch to be installed, if
#' not the master branch (default).
#' @return There is no tangible output to the function; it simply detaches and
#' updates the matchr package.
#' @export

reinstall_matchr <- function(branch = NULL) {

  if (!missing(branch)) {
    path <- glue::glue("UPGo-McGill/matchr@{branch}")
  } else path <- "UPGo-McGill/matchr"

  if("matchr" %in% (.packages())){
    detach("package:matchr", unload=TRUE)
  }

  remotes::install_github(path, upgrade = "never")
}
