#' Function to reinstall the strr package from GitHub
#'
#' @param branch A character string identifying the branch to be installed, if
#' not the master branch (default).
#' @return There is no tangible output to the function; it simply detaches and
#' updates the strr package.
#' @export

reinstall_strr <- function(branch = NULL) {

  if (!missing(branch)) {
    path <- glue::glue("UPGo-McGill/strr@{branch}")
  } else path <- "UPGo-McGill/strr"

  if("strr" %in% (.packages())){
    detach("package:strr", unload=TRUE)
  }

  remotes::install_github(path, upgrade = "never")
}
