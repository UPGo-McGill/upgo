#' Function to reinstall the strr package from GitHub
#'
#' @param branch A character string identifying the branch to be installed, if
#' not the master branch (default).
#' @return There is no tangible output to the function, simply the detaching,
#' updating and reattaching of the strr package.
#' @export

reinstall_strr <- function(branch = NULL) {

  if (!missing(branch)) {
    path <- glue::glue("UPGo-McGill/strr@{branch}")
  } else path <- "UPGo-McGill/strr"

  detach("package:strr", unload = TRUE)
  devtools::install_github(path)
  library(strr)
}
