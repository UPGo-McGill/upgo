#' Helper function to download Craigslist or Kijiji listings
#'
#' \code{get_listings_kj} scrapes listings from a list of URLs.
#'
#' @param url_list A character vector of URLs to be scraped.
#' @param timeout,proxies,quiet TKTK
#' @return A list of HTML objects.

get_listings_kj <- function(url_list, timeout = 1, proxies = NULL,
                            quiet = FALSE) {

  ## Initialize variables and environments -------------------------------------

  helper_require("rvest")
  # if (requireNamespace("doFuture", quietly = TRUE)) doFuture::registerDoFuture()
  if (missing(proxies)) proxies <- NULL


  ## Scrape listings -----------------------------------------------------------

  listings <- vector("list", length(url_list))

  handler_upgo("Scraping listing")

  with_progress({

    pb <- progressor(along = url_list)

    for (i in seq_along(url_list)) {

      user_agent <- user_agents[[i %% length(user_agents) + 1]]
      proxy <- NULL
      if (!is.null(proxies)) proxy <- proxies[[i %% length(proxies) + 1]]
      pb()

      tryCatch({result <- httr::GET(url_list[[i]],
                                    httr::user_agent(user_agent),
                                    httr::use_proxy(proxy))},
               error = function(e) result <- NULL)

      if (!is.null(result) && str_detect(rawToChar(result$content), "C9xx")) {
        result <- NULL}
      if (!is.null(result) && result$status_code %in% c(403, 502)) {
        result <- NULL}

      Sys.sleep(timeout)

      listings[[i]] <- result

      }

  })

  ## Clean up and exit ---------------------------------------------------------

  # Make sure that listings[[n]] is the right length if last element is NULL
  if (length(listings) != length(url_list)) {
    listings[length(url_list)] <- list(NULL)
  }

  return(listings)

}
