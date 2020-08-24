#' Scrape Kijiji listing URLs
#'
#' \code{get_urls_kj} scrapes Kijiji listing URLs for a city.
#'
#' @param city_name A character string: the city to be scraped.
#' @param short_long A character string, either "short" or "long" or "both", to
#' determine whether STR or LTR listing URLs should be scraped.
#' @param proxies Character vector of IPs to use for proxy connections. If
#' the length is less than the number of processes set by `future::plan()`,
#' proxies will be recycled.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A list of URLs.

get_urls_kj <- function(city_name, short_long = "both", timeout = 1,
                        proxies = NULL, quiet = FALSE) {

  ## Initialize variables and environments -------------------------------------

  helper_require("rvest")
  url_start <- "https://www.kijiji.ca"
  url_end <- "?ad=offering&siteLocale=en_CA"


  ## Establish random user_agent and proxy -------------------------------------

  user_agent <- user_agents[[ceiling(runif(1, 1, length(user_agents)))]]

  if (!missing(proxies)) {
    proxy <- proxies[[ceiling(runif(1, 1, length(proxies)))]]
  } else proxy <- proxies <- NULL


  ## Construct listing page URL ------------------------------------------------

  # STR
  if (short_long == "short") {
    city_vec <-
      dplyr::case_when(
        city_name == "Montreal" ~
          c("/b-location-court-terme/ville-de-montreal/", "c42l1700281"),
        city_name == "Toronto" ~
          c("/b-short-term-rental/city-of-toronto/", "c42l1700273"),
        city_name == "Vancouver" ~
          c("/b-short-term-rental/vancouver/", "c42l1700287")
      )

  # LTR
  } else if (short_long == "long") {
    city_vec <-
      dplyr::case_when(
        city_name == "Montreal" ~
          c("/b-apartments-condos/ville-de-montreal/", "c37l1700281"),
        city_name == "Toronto" ~
          c("/b-apartments-condos/city-of-toronto/", "c37l1700273"),
        city_name == "Vancouver" ~
          c("/b-apartments-condos/vancouver/", "c37l1700287")
      )
  }

  listings_url <- paste0(url_start, city_vec[[1]], city_vec[[2]], url_end)


  ## Find number of pages to scrape --------------------------------------------

  # Find number of pages to scrape
  listings_to_scrape <- httr::GET(listings_url,
                                  httr::user_agent(user_agent),
                                  httr::use_proxy(proxy))

  # Only proceed if status code is 200
  if (listings_to_scrape$status_code == 200) {

    listings_to_scrape <-
      listings_to_scrape %>%
      xml2::read_html() %>%
      rvest::html_node(xpath = '//*[@class="showing"]') %>%
      rvest::html_text()

  } else stop("The server returned a ", listings_to_scrape$status_code,
              " response.")

  if (nchar(listings_to_scrape) == 0) {
    stop("The server returned an empty response.")
  }

  listings_to_scrape <-
    listings_to_scrape %>%
    stringr::str_extract('(?<= of ).*(?=( Ads)|( results))') %>%
    stringr::str_replace(",", "") %>%
    as.integer()

# Convert listing count into pages
  pages <- min(ceiling(listings_to_scrape / 40), 100)


  ## Scrape pages --------------------------------------------------------------

  # Scrape in descending order
  handler_upgo("Scraping listing page")

  url_list <- vector("list", pages)

  with_progress({

    pb <- progressor(steps = pages)

    for (i in seq_len(pages)) {

      user_agent <- user_agents[[i %% length(user_agents) + 1]]
      proxy <- NULL
      if (!is.null(proxies)) proxy <- proxies[[i %% length(proxies) + 1]]
      pb()

      url <- paste0(url_start, city_vec[[1]], "page-", i, "/", city_vec[[2]],
                    url_end)

      url_list[[i]] <- helper_scrape_listing_page_kj(url, user_agent, proxy)
      Sys.sleep(timeout)

      }})

  url_list <- paste0(url_start, unique(unlist(url_list)))

  # If pages == 100, scrape again in ascending order
  if (pages == 100) {

    url_list_2 <- vector("list", pages)

    handler_upgo("Scraping (in reverse order) listing page")

    with_progress({

      pb <- progressor(steps = pages)

      for (i in seq_len(pages)) {

        user_agent <- user_agents[[i %% length(user_agents) + 1]]
        proxy <- NULL
        if (!is.null(proxies)) proxy <- proxies[[i %% length(proxies) + 1]]
        pb()

        url <- paste0(url_start, city_vec[[1]], "page-", i, "/", city_vec[[2]],
                      url_end, "&sort=dateAsc")

        url_list_2[[i]] <- helper_scrape_listing_page_kj(url, user_agent, proxy)
        Sys.sleep(timeout)

      }})

    url_list <- unique(c(url_list, paste0(url_start, unlist(url_list_2))))

    }

  return(url_list)

}
