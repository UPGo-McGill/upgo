#' Download URLs with progress reporting
#'
#' \code{upgo_download} is a thin wrapper around helper_download_listing to
#' enable progress reporting when used outside the main UPGo scraping functions.
#'
#' @param urls A character vector of URLs to download.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A list of HTML objects.
#' @export


upgo_download <- function(urls, quiet = FALSE) {

  results <- vector("list", length(urls))

  handler_upgo("Downloading URL")

  if (!quiet) {
    with_progress(
      results <- helper_download_listing(urls)
    )

  } else {
    results <- helper_download_listing(urls)
  }
}
