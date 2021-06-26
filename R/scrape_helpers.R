#### SCRAPE HELPERS ############################################################

#' Helper function to scrape Kijiji listing URLs
#'
#' \code{helper_urls_kj} scrapes Kijiji listing URLs for a city.
#'
#' @param city_name A character string: the city to be scraped.
#' @param short_long A character string, either "short" or "long", to determine
#' whether STR or LTR listing URLs should be scraped.
#' @return A list of URLs.

helper_urls_kj <- function(city_name, short_long) {

  helper_require("rvest")


  ## Define environment for %do_upgo% function ---------------------------------

  environment(`%do_upgo%`) <- environment()


  ## Initialize variables ------------------------------------------------------

  url_start <- "https://www.kijiji.ca"
  url_end <- "?ad=offering&siteLocale=en_CA"
  httr::set_config(httr::user_agent(user_agent))


  ## Establish proxy -----------------------------------------------------------

  if (rlang::env_has(.upgo_env, "proxy_list")) {
    rand <- ceiling(runif(1, 1, length(.upgo_env$proxy_list)))
    httr::use_proxy(.upgo_env$proxy_list[[rand]])
  }


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
  } else {
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
  listings_to_scrape <-
    xml2::read_html(httr::GET(listings_url)) %>%
    rvest::html_node(xpath = '//*[@class="showing"]') %>%
    rvest::html_text() %>%
    stringr::str_extract('(?<= of ).*(?=( Ads)|( results))') %>%
    stringr::str_replace(",", "") %>%
    as.integer()

  pages <- min(ceiling(listings_to_scrape / 40), 100)


  ## Prepare progress reporting ------------------------------------------------

  .upgo_env$pb <- progressor(steps = if_else(pages == 100, 200, pages))


  ## Scrape pages --------------------------------------------------------------

  # Scrape in descending order
  url_list <-
    foreach(i = seq_len(pages)) %do_upgo% {

      tryCatch({
        xml2::read_html(httr::GET(paste0(
          url_start, city_vec[[1]], "page-", i, "/", city_vec[[2]],
          url_end))) %>%
          rvest::html_nodes(xpath = '//*[@class="title"]') %>%
          xml2::xml_children() %>%
          rvest::html_attr("href") %>%
          stats::na.omit()
      }, error = function(e) NULL)
    }

  url_list <- paste0(url_start, unique(unlist(url_list)))

  # If pages == 100, scrape again in ascending order
  if (pages == 100) {

    url_list_2 <-
      foreach(i = seq_len(pages)) %do_upgo% {

        tryCatch({
          xml2::read_html(httr::GET(paste0(
            url_start, city_vec[[1]], "page-", i, "/", city_vec[[2]], url_end,
            "&sort=dateAsc"))) %>%
            rvest::html_nodes(xpath = '//*[@class="title"]') %>%
            xml2::xml_children() %>%
            rvest::html_attr("href") %>%
            stats::na.omit()
        }, error = function(e) NULL)
      }

    url_list <-
      unique(c(url_list, paste0(url_start, unlist(url_list_2))))
  }

  return(url_list)

}


#' Helper function to download Craigslist or Kijiji listings
#'
#' \code{helper_download_listing} scrapes listings from a list of URLs.
#'
#' @param urls A character vector of URLs to be scraped.
#' @param proxies TKTK
#' @return A list of HTML objects.
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect

helper_download_listing <- function(urls, proxies = NULL) {

  helper_require("rvest")
  doParallel::registerDoParallel()
  `%do_par%` <- foreach::`%dopar%`


  ## Scrape listings -----------------------------------------------------------

  progressr::with_progress({

    pb <- progressr::progressor(along = urls)

    listings <-
      foreach::foreach(i = seq_along(urls)) %do_par% {

        pb()

        # if (!missing(proxies)) {
        httr::set_config(httr::use_proxy(
          proxies[[(i %% length(proxies)) + 1]]))
        # }

        tryCatch({httr::RETRY("GET", urls[[i]], times = 3, pause_base = 1,
                              pause_cap = 5, terminate_on = c(403, 404))
        }, error = function(e) NULL)
      }

  })


  ## Clean up and exit ---------------------------------------------------------

  # Make sure that listings[[n]] is the right length if last element is NULL
  if (length(listings) != length(urls)) {
    listings[length(urls)] <- list(NULL)
  }

  return(listings)

}


#' Helper function to parse scraped Kijiji listings
#'
#' \code{helper_parse_kj} parses a scraped Kijiji listing.
#'
#' @param .x A single scraped Kijiji listing, as retrieved with read_html.
#' @param .y A single Kijiji URL.
#' @param city_name A character scalar indicating the name of the city in which
#' the listing is located.
#' @return A one-row data frame.
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect str_replace_all

helper_parse_kj <- function(.x, .y, city_name) {

  helper_require("rvest")

  ## Set user agent ------------------------------------------------------------

  httr::set_config(httr::user_agent(user_agent))


  ### Read listing and establish validity ######################################

  .x <-
    tryCatch(xml2::read_html(.x, options = "HUGE"), error = function(e) NULL)


  ## If output is NULL, try download again then exit ---------------------------

  if (is.null(.x)) {

    .x <-
      tryCatch(
        xml2::read_html(httr::GET(paste0(.y, "?siteLocale=en_CA")),
                        options = "HUGE"),
        error = function(e) NULL)

    if (is.null(.x)) return(helper_error_kj())
  }


  ## Exit if the input is still not valid --------------------------------------

  fail <-
    tryCatch({rvest::html_node(.x, "head"); FALSE}, error = function(e) TRUE)

  if (fail) return(helper_error_kj())


  ## Check for 404 redirects and expired links ---------------------------------

  # Should be 0 for valid listing, and 2 for 404 redirected
  redirect_check <-
    .x %>%
    rvest::html_node(xpath = 'body[@id = "PageSRP"]') %>%
    length()

  if (redirect_check == 2) return(helper_error_kj())

  expired_check <-
    .x %>%
    rvest::html_node(xpath = '//*[@id = "PageExpiredVIP"]') %>%
    rvest::html_text()

  if (!is.na(expired_check)) return(helper_error_kj())


  ## Check for missing text field ----------------------------------------------

  text_check <-
    tryCatch({
      .x %>%
        rvest::html_node(
          xpath = '//*[@class = "descriptionContainer-3544745383"]') %>%
        rvest::html_node('div') %>%
        rvest::html_text()
      TRUE},
    error = function(e) FALSE)

  tries <- 0

  # Final check for valid listing
  if (!text_check) {
    tryCatch({
      .x <- xml2::read_html(httr::GET(paste0(.y, "?siteLocale=en_CA"),
                                      httr::user_agent(user_agent)),
                            options = "HUGE")

      text_check <-
        tryCatch({
          .x %>%
            rvest::html_node(
              xpath = '//*[@class = "descriptionContainer-3544745383"]') %>%
            rvest::html_node('div') %>%
            rvest::html_text()
          TRUE},
          error = function(e) FALSE)

      },
      error = function(e) return(helper_error_kj()))
  }

  # If the text field still isn't present, exit function
  if (!text_check) return(helper_error_kj())


  ## Find details class --------------------------------------------------------

  x_details <-
    .x %>%
    rvest::html_node(xpath = '//*[@id="mainPageContent"]') %>%
    xml2::xml_child(2) %>%
    rvest::html_text()

  # If the field isn't correctly retrieved, try again with fresh download
  if (is.na(x_details)) {
    .x <- xml2::read_html(paste0(.y, "?siteLocale=en_CA"), options = "HUGE")

    x_details <-
      .x %>%
      rvest::html_node(xpath = '//*[@id="mainPageContent"]') %>%
      xml2::xml_child(2) %>%
      rvest::html_text()

    if (is.na(x_details)) return(helper_error_kj())
  }


  ### Parse input ##############################################################

  ## Get bed_bath object -------------------------------------------------------

  bed_bath <- str_extract(x_details, 'Bedrooms.{1,20}Bathrooms[^ ,].{6}')

  bed_field <-
    bed_bath %>%
    stringr::str_extract('(?<=Bedrooms).*(?=Bathrooms)') %>%
    stringr::str_replace(': ', '') %>%
    stringr::str_trim()

  bath_field <-
    bed_bath %>%
    stringr::str_extract('(?<=Bathrooms).*(?=Ov|Fu|Ut|UR)') %>%
    stringr::str_replace(': ', '') %>%
    stringr::str_trim()

  if (is.na(bath_field)) {
    bath_field <-
      x_details %>%
      stringr::str_extract('(?<=Bathrooms).{1,5}?(?=Furnished|Overview)') %>%
      stringr::str_replace(': ', '') %>%
      stringr::str_trim()
  }

  ## Produce output tibble -----------------------------------------------------

  tibble(
    id =
      .x %>%
      rvest::html_node(xpath = '//*[@class = "adId-4111206830"]') %>%
      rvest::html_text(),
    url =
      .y,
    title =
      .x %>%
      rvest::html_node("head") %>%
      rvest::html_node("title") %>%
      rvest::html_text(),
    short_long = if_else(
      stringr::str_detect(url, "v-location-court-terme|v-short-term-rental"),
      "short",
      "long"),
    created =
      .x %>%
      rvest::html_node(xpath = '//*/time/@datetime') %>%
      rvest::html_text() %>%
      as.Date(),
    scraped = Sys.Date(),
    price =
      .x %>%
      rvest::html_node(xpath = '//*[@class = "priceContainer-1419890179"]') %>%
      rvest::html_node(xpath = 'span') %>%
      rvest::html_node(xpath = 'span/@content') %>%
      rvest::html_text() %>%
      stringr::str_replace("\\..*$", "") %>%
      as.numeric(),
    city =
      city_name,
    location =
      .x %>%
      rvest::html_node(xpath = '//*[@class = "address-3617944557"]') %>%
      rvest::html_text(),
    bedrooms =
      bed_field,
    bathrooms =
      bath_field,
    furnished =
      x_details %>%
      stringr::str_extract('(?<=Meubl\u00e9|Furnished)(Yes|No)'),
    details =
      x_details,
    text =
      .x %>%
      rvest::html_node(xpath =
                         '//*[@class = "descriptionContainer-3544745383"]') %>%
      rvest::html_node('div') %>%
      rvest::html_text(),
    photos = suppressWarnings(list(
      .x %>%
        rvest::html_nodes(
          xpath =
            '//*[@class = "heroImageBackground-4116888288 backgroundImage"]'
          ) %>%
        stringr::str_extract('(?<=image:url..).*(?=..;back)')))
  ) %>%
    mutate(bedrooms = case_when(
      bedrooms == "1 chambre et salon"              ~ "1 + Den",
      bedrooms == "2 chambres et coin d\u00e9tente" ~ "2 + Den",
      bedrooms == "6+"                              ~ "6+",
      bedrooms == "6 chambres ou plus"              ~ "5+",
      bedrooms == "Studio"                          ~ "Bachelor/Studio",
      TRUE ~ bedrooms
    )) %>%
    mutate(furnished = case_when(.data$furnished %in% c("Oui", "Yes") ~ TRUE,
                                 .data$furnished %in% c("Non", "No") ~ FALSE,
                                 is.na(.data$furnished) ~ NA))
}


#' Helper function to generate error Kijiji output
#'
#' @return A zero-row data frame.

helper_error_kj <- function() {

  dplyr::tibble(
    id = character(),
    url = character(),
    title = character(),
    short_long = character(),
    created = as.Date(integer(), "1970-01-01"),
    scraped = as.Date(integer(), "1970-01-01"),
    price = numeric(),
    city = character(),
    location = character(),
    bedrooms = character(),
    bathrooms = character(),
    furnished = logical(),
    details = character(),
    text = character(),
    photos = list()
  )

}


#' Helper function to generate Kijiji detail field
#'
#' @param .x A single scraped Kijiji listing.
#' @return A character scalar.

helper_detail_parse <- function(.x) {

  x_details <-
    .x %>%
    rvest::html_node(xpath = '//*[@class = "itemAttributeCards-2416600896"]'
    ) %>%
    rvest::html_text() %>%
    stringr::str_replace_all("\n", "")

  if (is.na(x_details)) {
    x_details <-
      .x %>%
      rvest::html_node(xpath =
                         '//*[@class = "attributeListWrapper-2108313769"]') %>%
      rvest::html_text() %>%
      stringr::str_replace_all("\n", "")
  }

  if (is.na(x_details)) {
    x_details <-
      .x %>%
      rvest::html_node(xpath =
                         paste0('//*[@class = "itemAttributeCards-2416600896 ',
                                'itemAttributeCards__fourCards-3070740560"]')
                       ) %>%
      rvest::html_text() %>%
      stringr::str_replace_all("\n", "")
  }

  if (is.na(x_details)) {
    x_details <-
      .x %>%
      rvest::html_node(xpath =
                         paste0('//*[@class = "itemAttributeCards-2416600896 ',
                                'itemAttributeCards__twoCards-934340663"]')
                       ) %>%
      rvest::html_text() %>%
      stringr::str_replace_all("\n", "")
  }

  x_details
}


#' Helper function to scrape listing page for Kijiji

#' @param url,user_agent,proxy Arguments passed on from the main function.

helper_scrape_listing_page_kj <- function(url, user_agent, proxy) {

  page <- tryCatch({url %>% httr::GET(httr::user_agent(user_agent),
                                      httr::use_proxy(proxy))},
                   error = function(e) NULL)

  if (is.null(page)) return(NULL)

  if (page$status_code == 200) {
    page <-
      page %>%
      xml2::read_html() %>%
      rvest::html_nodes(xpath = '//*[@class="title"]') %>%
      xml2::xml_children() %>%
      rvest::html_attr("href") %>%
      stats::na.omit()
  } else stop("The server returned a ", page$status_code, " response.")

  if (length(page) == 0) {
    stop("The server returned empty results.")
  }

  return(page)

}



#' Helper function to scrape registration information from an Airbnb listing
#'
#' \code{helper_scrape_ab_registration} scrapes the registration number from a
#' single Airbnb listing and forwards to the calling function for further
#' processing.
#'
#' @param PID An Airbnb property ID to be scraped.

helper_scrape_ab_registration <- function(PID) {

  ### Initialize objects #######################################################

  scrape_result <-
    dplyr::tibble(property_ID = paste0("ab-", PID),
                  date = Sys.Date(),
                  registration = "NO LISTING"
                  )


  ### Navigate to listing and verify it is loaded ##############################

  remDr$setImplicitWaitTimeout(0)
  remDr$navigate(paste0("https://www.airbnb.ca/rooms/", PID))

  load_check <- FALSE
  iters <- 0

  while (!load_check && iters <= 5) {

    iters <- iters + 1

    load_check <-
      suppressMessages(
        tryCatch({
          # This element is only true once the page is loaded
          remDr$findElement("xpath", '//*[@data-triggered = "true"]')
          TRUE
        }, error = function(e) {
          # If the key element is loaded, wait one second then try again
          Sys.sleep(1)
          FALSE
        }
        ))
  }

  ### Return NULL if the listing hasn't loaded in 5 seconds ####################

  if (!load_check) return(NULL)


  ### Test URL for missing listing and exit early if so ########################

  if (remDr$getCurrentUrl()[[1]] == "https://www.airbnb.ca/s/homes") {

    return(scrape_result)
  }


  ### Get field ################################################################

  reg <- remDr$findElements("class", "_1xib9m0")
  reg <- reg[[1]]$getElementAttribute("outerHTML")[[1]]
  reg <- stringr::str_extract(reg, paste0("(?<=Licence number</span><br>)",
                                          ".*(?=</span></div>)"))

  scrape_result[1,]$registration <- reg

  return(scrape_result)

}
