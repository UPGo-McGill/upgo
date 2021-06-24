#' Helper function to download Kijiji listings
#'
#' \code{download_listings_kj} scrapes listings from a list of URLs.
#'
#' @param urls A character vector of URLs to be scraped.
#' @param timeout,proxies,quiet TKTK
#' @return A list of HTML objects.
#' @export

download_listings_kj <- function(url_list, timeout = 1, proxies = NULL,
                                 quiet = FALSE) {

  helper_require("rvest")

  ## Just pass arguments directly to get_listings_kj if <= 100 -----------------

  if (length(url_list) <= 100) return(get_listings_kj(url_list, timeout,
                                                      proxies, quiet))


  ## Otherwise test first 100 listings -----------------------------------------

  message("Testing connectivity with first 100 URLs.")

  result <- vector("list", length(url_list))
  result <- map(result, ~NULL)

  url_test <- url_list[1:100]
  result_test <- get_listings_kj(url_test, timeout, proxies, quiet)
  result[1:100] <- result_test


  ## If most results are NULL, exit --------------------------------------------

  if (sum(purrr::map_lgl(result_test, is.null)) > 75) {

    message("Most results are NULL; function exiting early.")
    return(result)

  }


  ## If some results are NULL and proxies are set, drop bad proxies ------------

  if (!missing(proxies) && sum(purrr::map_lgl(result_test, is.null)) > 0) {

    proxy_result <- split(result_test, seq_along(proxies))
    proxy_means <- map_dbl(proxy_result, ~mean(map_lgl(.x, is.null)))
    proxies_to_drop <- which(proxy_means > 0.5) - 1
    proxies_to_drop[proxies_to_drop == 0] <- length(proxies)
    proxies <- proxies[!proxies_to_drop]

    if (length(proxies) == 0) {

      message("All proxies returning NULL results; function exiting early.")
      return(result)

    }

    if (length(proxies_to_drop) > 0) {
      message(length(proxies_to_drop), " proxies returning NULL results; ",
              "function proceeding without them.")
    }

  }


  ## Proceed 100 URLs at a time ------------------------------------------------

  result_list <- vector("list")
  i <- 1
  urls_left <- url_list[-c(1:100)]

  while (length(urls_left) > 0) {

    message("Starting batch ", i + 1, " of ", ceiling(length(url_list) / 100),
            ".")
    urls_next <- urls_left[1:min(100, length(urls_left))]
    result_list[[i]] <- get_listings_kj(urls_next, timeout, proxies, quiet)

    # If most results are NULL, exit
    if (mean(purrr::map_lgl(result_list[[i]], is.null)) > 0.75) {
      message("All results are NULL; function exiting early.")
      result_list <- unlist(result_list, recursive = FALSE)
      result[101:(i * 100 + 100)] <- result_list
      result <- result[seq_along(url_list)]
      return(result)
      }

    # If some results are NULL and proxies are set, drop bad proxies
    if (!missing(proxies) &&
        sum(purrr::map_lgl(result_list[[i]], is.null)) > 0) {

      proxy_result <- split(result_list[[i]], seq_along(proxies))
      proxy_means <- map_dbl(proxy_result, ~mean(map_lgl(.x, is.null)))
      proxies_to_drop <- which(proxy_means > 0.5) - 1
      proxies_to_drop[proxies_to_drop == 0] <- length(proxies)
      proxies <- proxies[!proxies_to_drop]

      if (length(proxies) == 0) {
        message("All proxies returning NULL results; function exiting early.")
        result_list <- unlist(result_list, recursive = FALSE)
        result[101:(i * 100 + 100)] <- result_list
        result <- result[seq_along(url_list)]
        return(result)
      }

      if (length(proxies_to_drop) > 0) {
        message(length(proxies_to_drop), " proxies returning NULL results; ",
                "function proceeding without them.")
      }
    }

    i <- i + 1
    urls_left <- urls_left[-seq_along(urls_next)]

  }

  result_list <- unlist(result_list, recursive = FALSE)
  result[101:length(result)] <- result_list
  result <- result[seq_along(url_list)]
  return(result)

}
