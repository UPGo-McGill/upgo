#' Download URLs with progress reporting
#'
#' \code{upgo_download} is a thin wrapper around helper_download_listing to
#' enable progress reporting when used outside the main UPGo scraping functions.
#'
#' @param urls A character vector of URLs to download.
#' @param proxies Character vector of IPs to use for proxy connections. If
#' supplied, this must be at least as long as the number of cores.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A list of HTML objects.
#' @export


upgo_download <- function(urls, proxies = NULL, quiet = FALSE) {

  # Prepare for parallel processing
  if(requireNamespace("doFuture", quietly = TRUE)) {
    doFuture::registerDoFuture()
  }

  if (!missing(proxies)) {

    # Put proxy list in .upgo_env so it can be accessed from child functions
    .upgo_env$proxy_list <- proxies

    on.exit(rlang::env_unbind(.upgo_env, "proxy_list"), add = TRUE)

  }

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
