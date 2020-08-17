#' Helper function to scrape Craigslist listing URLs
#'
#' \code{helper_urls_cl} scrapes Craigslist listing URLs for a city.
#'
#' @param city_name A character string: the city to be scraped.
#' @importFrom progressr handlers handler_progress progressor
#' @importFrom stats na.omit
#' @return A list of URLs.

helper_urls_cl <- function(city_name) {

  helper_require("rvest")


  ## Define environment for %do_upgo% function ---------------------------------

  environment(`%do_upgo%`) <- environment()


  ## Establish proxy -----------------------------------------------------------

  if (rlang::env_has(.upgo_env, "proxy_list")) {
    rand <- ceiling(runif(1, 1, length(.upgo_env$proxy_list)))
    httr::set_config(httr::use_proxy(.upgo_env$proxy_list[[rand]]))
  }


  ## Construct listing page URL ------------------------------------------------

  listings_url <-
    paste0("https://", city_name,
           ".craigslist.org/search/apa?s=0&lang=en&cc=us")


  ## Find number of pages to scrape --------------------------------------------

  listings_to_scrape <-
    listings_url %>%
    httr::GET() %>%
    xml2::read_html() %>%
    rvest::html_node(".totalcount") %>%
    rvest::html_text()

  pages <- ceiling(as.numeric(listings_to_scrape) / 120)


  ## Prepare progress reporting ------------------------------------------------

  .upgo_env$pb <- progressor(steps = pages)


  ## Scrape pages --------------------------------------------------------------

  url_list <-
    foreach::foreach(i = seq_len(pages)) %do_upgo% {
      tryCatch({
        xml2::read_html(httr::GET(paste0(
          "https://", city_name, ".craigslist.org/search/apa?s=",
          120 * (i - 1), "&lang=en&cc=us"))) %>%
          rvest::html_nodes(".result-row") %>%
          xml2::xml_children() %>%
          rvest::html_attr("href") %>%
          na.omit()
      }, error = function(e) NULL)
    }


  ## Merge and clean up list ---------------------------------------------------

  url_list <- unique(unlist(url_list)) %>% str_replace("\\?.*", "")

  return(url_list)

}


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


  ## Set user agent ------------------------------------------------------------

  httr::set_config(
    httr::user_agent(paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                            "AppleWebKit/537.36 (KHTML, like Gecko) ",
                            "Chrome/80.0.3987.149 Safari/537.36")))


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

    listings_url <- paste0(url_start, city_vec[[1]], city_vec[[2]], url_end)
  }

  # LTR
  if (short_long %in% c("long", "both")) {

    city_vec <-
      dplyr::case_when(
        city_name == "Montreal" ~
          c("/b-apartments-condos/ville-de-montreal/", "c37l1700281"),
        city_name == "Toronto" ~
          c("/b-apartments-condos/city-of-toronto/", "c37l1700273"),
        city_name == "Vancouver" ~
          c("/b-apartments-condos/vancouver/", "c37l1700287")
        )

    listings_url <- paste0(url_start, city_vec[[1]], city_vec[[2]], url_end)
  }


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
          na.omit()
      }, error = function(e) NULL)
    }

  url_list <- paste0(url_start, unique(unlist(url_list)))

  # If pages == 100, scrape again in ascending order
  if (pages == 100) {

    url_list_2 <-
      foreach (i = seq_len(pages)) %do_upgo% {

        tryCatch({
          xml2::read_html(httr::GET(paste0(
            url_start, city_vec[[1]], "page-", i, "/", city_vec[[2]], url_end,
            "&sort=dateAsc"))) %>%
            rvest::html_nodes(xpath = '//*[@class="title"]') %>%
            xml2::xml_children() %>%
            rvest::html_attr("href") %>%
            na.omit()
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
#' @return A list of HTML objects.
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect

helper_download_listing <- function(urls) {

  helper_require("rvest")


  ## Define environment for %do_upgo% function ---------------------------------

  environment(`%do_upgo%`) <- environment()


  ## Prepare progress reporting ------------------------------------------------

  .upgo_env$pb <- progressor(along = urls)


  ## Scrape listings -----------------------------------------------------------

  listings <-
    foreach::foreach(i = seq_along(urls)) %do_upgo% {
      tryCatch({
        tryCatch(httr::GET(urls[[i]], httr::timeout(1)),
                 error = function(e) {
                   httr::reset_config()
                   httr::set_config(
                     httr::user_agent(
                       paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                              "AppleWebKit/537.36 (KHTML, like Gecko) ",
                              "Chrome/80.0.3987.149 Safari/537.36")))
                   httr::RETRY("GET", urls[[i]], times = 5, pause_base = 0.2,
                               pause_cap = 5, terminate_on = 404)
                   })
        },
        error = function(e) NULL)
  }


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

  httr::set_config(
    httr::user_agent(paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                            "AppleWebKit/537.36 (KHTML, like Gecko) ",
                            "Chrome/80.0.3987.149 Safari/537.36")))


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

  # Listing should be valid by now, so retry download aggressively
  while (!text_check && tries < 5) {

    tries <- tries + 1

    .x <- xml2::read_html(
      httr::GET(paste0(.y, "?siteLocale=en_CA"),
                httr::user_agent(
                  paste0("Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
                         "AppleWebKit/537.36 (KHTML, like Gecko) ",
                         "Chrome/80.0.3987.149 Safari/537.36"))),
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


#' Helper function to parse scraped Craigslist listings
#'
#' \code{helper_parse_cl} parses a scraped Craiglist listing.
#'
#' @param .x A single scraped Craigslist listing, as retrieved with read_html.
#' @param .y A single Craigslist URL.
#' @param city_name A character scalar indicating the name of the city in which
#' the listing is located.
#' @return A one-row data frame.

helper_parse_cl <- function(.x, .y, city_name) {

  # Read listing, and exit early on failure
  .x <-
    tryCatch(xml2::read_html(.x, options = "HUGE"), error = function(e) NULL)

  if (is.null(.x)) return(helper_error_cl())

  # Generate details object
  x_details <-
    tryCatch({
      .x %>%
        rvest::html_node(xpath =
                           '/html/body/section[@class="page-container"]') %>%
        rvest::html_node(xpath = 'section[@class="body"]') %>%
        rvest::html_node(xpath = 'section[@class="userbody"]') %>%
        rvest::html_node(xpath = 'div[@class = "mapAndAttrs"]') %>%
        rvest::html_nodes(xpath = 'p') %>%
        rvest::html_text() %>%
        stringr::str_replace_all("\n", "") %>%
        stringr::str_replace_all("  ", " ") %>%
        stringr::str_replace_all("  ", " ") %>%
        stringr::str_replace_all("  ", " ") %>%
        stringr::str_replace_all("  ", " ") %>%
        stringr::str_extract('(?<= ).*(?= )') %>%
        paste(collapse = "; ")
    }, error = function(e) NA_character_)

  tibble(
    id = tryCatch({
      .x %>%
        rvest::html_node(xpath =
                           '/html/body/section[@class="page-container"]') %>%
        rvest::html_node(xpath = 'section[@class="body"]') %>%
        rvest::html_node(xpath = 'section[@class="userbody"]') %>%
        rvest::html_node(xpath = 'div[@class="postinginfos"]') %>%
        rvest::html_node("p") %>%
        rvest::html_text() %>%
        stringr::str_extract('(?<=id: ).*')
    }, error = function(e) NA_character_),
    url =
      .x %>%
      rvest::html_node(xpath = '/html/head/link[@rel="canonical"]/@href') %>%
      rvest::html_text(),
    title =
      .x %>%
      rvest::html_node(xpath = '/html/head/title') %>%
      rvest::html_text(),
    created =
      .x %>%
      rvest::html_node(xpath = '//*[@id="display-date"]/time/@datetime') %>%
      rvest::html_text() %>%
      as.Date(origin = "1970-01-01"),
    scraped =
      Sys.Date(),
    price =
      tryCatch({
        .x %>%
          rvest::html_node(xpath = '/html/body') %>%
          rvest::html_node(xpath = 'section[@class = "page-container"]') %>%
          rvest::html_node(xpath = 'section[@class = "body"]') %>%
          rvest::html_node(xpath = 'h2[@class = "postingtitle"]') %>%
          rvest::html_node(xpath = 'span/span[@class = "price"]') %>%
          rvest::html_text() %>%
          stringr::str_replace("\\$", "") %>%
          as.numeric()
      }, error = function(e) NA_real_),
    city =
      city_name,
    location =
      .x %>%
      rvest::html_node(xpath = '/html/head') %>%
      rvest::html_node(xpath = 'meta[@name = "geo.position"]/@content') %>%
      rvest::html_text(),
    bedrooms =
      str_extract(x_details, '^.(?=BR)'),
    bathrooms =
      str_extract(x_details, '(?<=BR . ).{1,3}(?=Ba)'),
    furnished =
      if_else(str_detect(x_details, "furnished"), TRUE, FALSE),
    details =
      x_details,
    text =
      tryCatch({
        .x %>%
          rvest::html_node(xpath =
                             '/html/body/section[@class="page-container"]') %>%
          rvest::html_node(xpath = 'section[@class= "body"]') %>%
          rvest::html_node(xpath = 'section[@class= "userbody"]') %>%
          rvest::html_node(xpath = 'section[@id = "postingbody"]') %>%
          rvest::html_text() %>%
          stringr::str_replace(
            "\n            QR Code Link to This Post\n            \n        \n",
            "") %>%
          stringr::str_replace_all("\u2022\t", "") %>%
          stringr::str_replace_all("\n", " ")
      }, error = function(e) NA_character_),
    photos =
      list(.x %>%
             rvest::html_nodes(xpath = '//*/img') %>%
             rvest::html_node(xpath = '@src') %>%
             rvest::html_text() %>%
             stringr::str_replace("50x50c", "600x450") %>%
             unique())
  )

}


#' Helper function to generate error Craigslist output
#'
#' @return A zero-row data frame.
#' @importFrom dplyr %>% tibble

helper_error_cl <- function() {

  dplyr::tibble(
    id = character(),
    url = character(),
    title = character(),
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


#' Helper function to scrape location information from an Airbnb listing
#'
#' \code{helper_scrape_ab} scrapes the location (city, region and
#' country) from a single Airbnb listing and forwards to the calling function
#' for further processing.
#'
#' @param PID An Airbnb property ID to be scraped.
#' @importFrom dplyr case_when last tibble
#' @importFrom stringr str_detect str_extract str_extract_all str_replace
#' @importFrom stringr str_split

helper_scrape_ab <- function(PID) {

  ### Initialize objects #######################################################

  scrape_result <-
    tibble(property_ID = character(), raw = list(), note = character())
  scrape_result[1, 1] <- paste0("ab-", PID)


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

    scrape_result[1,]$raw <- list("NO LISTING")
    scrape_result[1,]$note <- "no_listing"

    return(scrape_result)
  }


  ### Try regular listing type #################################################

  # This class is sometimes present at the top of listings
  element_top <- remDr$findElements("class", "_5twioja")

  if (length(element_top) > 0) {

    element_top <-
      purrr::map_chr(element_top, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    element_top_processed <-
      element_top %>%
      stringr::str_extract('(?<=">).*(?=</a>)') %>%
      stringr::str_replace("<span>", "") %>%
      stringr::str_replace("</span>", "") %>%
      stringr::str_split(", ") %>%
      unlist()

    if (length(element_top_processed) > 0) {
      scrape_result[1,]$raw <- list(element_top)
      scrape_result[1,]$note <- "top"

      return(scrape_result)
    }
  }


  ### Deal with Luxe and Plus listings #########################################

  if (stringr::str_detect(remDr$getCurrentUrl(), "luxury")) {
    element_bottom <-
      remDr$findElements("class", "_4mq26") %>%
      purrr::map_chr(~.x$getElementAttribute("outerHTML")[[1]])

    scrape_result[1,]$raw <- list(element_bottom)
    scrape_result[1,]$note <- "luxe"

    return(scrape_result)

  }

  if (str_detect(remDr$getCurrentUrl(), "plus")) {
    element_bottom <-
      remDr$findElements("class", "_ylytgbo") %>%
      purrr::map_chr(~.x$getElementAttribute("outerHTML")[[1]])

    scrape_result[1,]$raw <- list(element_bottom)
    scrape_result[1,]$note <- "plus"

    return(scrape_result)

  }


  ### Try _abw475 ##############################################################

  element_bottom <- remDr$findElements("class", "_abw475")

  if (length(element_bottom) %in% 1:3) {

    element_bottom <-
      purrr::map_chr(element_bottom, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    scrape_result[1,]$raw <- list(element_bottom)
    scrape_result[1,]$note <- "_abw475"

    return(scrape_result)

  }


  ### Try _s1tlw0m #############################################################

  element_bottom <- remDr$findElements("class", "_s1tlw0m")

  if (length(element_bottom) > 0) {

    element_bottom <-
      purrr::map_chr(element_bottom, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    scrape_result[1,]$raw <- list(element_bottom)
    scrape_result[1,]$note <- "_s1tlw0m"

    return(scrape_result)

  }


  ### Try _czm8crp #############################################################

  element_bottom <- remDr$findElements("class", "_czm8crp")

  if (length(element_bottom) < 0) {

    element_bottom <-
      purrr::map_chr(element_bottom, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    scrape_result[1,]$raw <- list(element_bottom)
    scrape_result[1,]$note <- "_czm8crp"


  }


  ### If nothing else works, return NULL #######################################

  return(NULL)

}


#' Helper function to parse location information from an Airbnb listing
#'
#' \code{helper_parse_ab} parses the location (city, region and from a single
#' scraped Airbnb listing.
#'
#' @param scrape_result A one-row data frame produced as an interim result from
#' \code{helper_scrape_ab}.
#' @importFrom dplyr bind_rows case_when filter last mutate select tibble
#' @importFrom stringr str_detect str_extract str_replace str_split
#' @importFrom purrr map map_chr map_int map_lgl

helper_parse_ab <- function(scrape_result) {

  ### Exit early on NULL #######################################################

  if (is.null(scrape_result)) return(NULL)


  ### Process missing ##########################################################

  if (scrape_result$note == "no_listing") {

    return(
      scrape_result %>%
        mutate(city = NA_character_,
               region = NA_character_,
               country = NA_character_,
               date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process Luxe #############################################################

  if (scrape_result$note == "luxe") {

    return(

      scrape_result %>%
        mutate(
          location = purrr::map(.data$raw, ~{
            .x[stringr::str_detect(.x, ",")] %>%
              stringr::str_extract('(?=>).+(?=<)') %>%
              stringr::str_replace(">", "") %>%
              stringr::str_split(", ") %>%
              unlist()
          }),
          city = purrr::map_chr(.data$location, ~.x[1]),
          region = purrr::map_chr(.data$location, ~{
            if (.x[2] %in% US_states) {
              case_when(
                .x[2] == "AL" ~ "Alabama",  .x[2] == "AK" ~ "Alaska",
                .x[2] == "AZ" ~ "Arizona", .x[2] == "AR" ~ "Arkansas",
                .x[2] == "CA" ~ "California", .x[2] == "CO" ~ "Colorado",
                .x[2] == "CT" ~ "Connecticut", .x[2] == "DE" ~ "Delaware",
                .x[2] == "FL" ~ "Florida", .x[2] == "GA" ~ "Georgia",
                .x[2] == "HI" ~ "Hawaii", .x[2] == "ID" ~ "Idaho",
                .x[2] == "IL" ~ "Illinois", .x[2] == "IN" ~ "Indiana",
                .x[2] == "IA" ~ "Iowa", .x[2] == "KS" ~ "Kansas",
                .x[2] == "KY" ~ "Kentucky", .x[2] == "LA" ~ "Louisiana",
                .x[2] == "ME" ~ "Maine", .x[2] == "MD" ~ "Maryland",
                .x[2] == "MA" ~ "Massachusetts", .x[2] == "MI" ~ "Michigan",
                .x[2] == "MN" ~ "Minnesota", .x[2] == "MS" ~ "Mississippi",
                .x[2] == "MO" ~ "Missouri", .x[2] == "MT" ~ "Montana",
                .x[2] == "NE" ~ "Nebraska", .x[2] == "NV" ~ "Nevada",
                .x[2] == "NH" ~ "New Hampshire", .x[2] == "NJ" ~ "New Jersey",
                .x[2] == "NM" ~ "New Mexico", .x[2] == "NY" ~ "New York",
                .x[2] == "NC" ~ "North Carolina", .x[2] == "ND" ~
                  "North Dakota",
                .x[2] == "OH" ~ "Ohio", .x[2] == "OK" ~ "Oklahoma",
                .x[2] == "OR" ~ "Oregon", .x[2] == "PA" ~ "Pennsylvania",
                .x[2] == "RI" ~ "Rhode Island", .x[2] == "SC" ~
                  "South Carolina",
                .x[2] == "SD" ~ "South Dakota", .x[2] == "TN" ~ "Tennessee",
                .x[2] == "TX" ~ "Texas", .x[2] == "UT" ~ "Utah",
                .x[2] == "VT" ~ "Vermont", .x[2] == "VA" ~ "Virginia",
                .x[2] == "WA" ~ "Washington", .x[2] == "WV" ~ "West Virginia",
                .x[2] == "WI" ~ "Wisconsin", .x[2] == "WY" ~ "Wyoming"
              )
            } else NA_character_
          }),
          country = purrr::map_chr(.data$location, ~{
            if (.x[2] %in% US_states) "United States" else .x[2]
          }),
          date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process Plus #############################################################

  if (scrape_result$note == "plus") {

    return(
      scrape_result %>%
        mutate(location = purrr::map(.data$raw, ~{
          .x %>%
            stringr::str_extract('(?=>).+(?=<)') %>%
            stringr::str_replace(">", "") %>%
            stringr::str_split(", ") %>%
            unlist()
        })) %>%
        mutate(city = purrr::map_chr(.data$location, ~.x[1]),
               region = purrr::map_chr(.data$location, ~{
                 if (length(.x) == 2) NA_character_ else .x[2]
               }),
               country = purrr::map_chr(.data$location, ~{
                 if (length(.x) == 2) .x[2] else .x[3]
               }),
               date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process element_top results ##############################################

  if (scrape_result$note == "top") {

    content <-
      scrape_result$raw[[1]] %>%
      stringr::str_extract('(?<=">).*(?=</a>)') %>%
      stringr::str_replace("<span>", "") %>%
      stringr::str_replace("</span>", "") %>%
      stringr::str_split(", ") %>%
      unlist()

    # Deal with Bonaire comma
    if (content[[length(content)]] == "Sint Eustatius and Saba") {
      content[[length(content) - 1]] <- "Bonaire, Sint Eustatius and Saba"
      content <- content[1:(length(content) - 1)]
    }

    if (length(content) == 1) {

      if (content %in% country_list) {
        return(
          scrape_result %>%
            mutate(
              city = NA_character_,
              region = NA_character_,
              country = content,
              date = Sys.Date()
            ) %>%
            select(.data$property_ID, .data$city:.data$country, .data$raw,
                   .data$date)
        )
      } else return(NULL)

    }

    return(
      scrape_result %>%
        mutate(
          city = content[[1]],
          region = if_else(length(content) == 3, content[[2]], NA_character_),
          country = content[[length(content)]],
          date = Sys.Date()
        ) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process _s1tlw0m results #################################################

  if (scrape_result$note == "_s1tlw0m") {

    content <-
      # The valid element seems to be the only one with a comma
      scrape_result$raw[[1]][stringr::str_detect(scrape_result$raw[[1]],
                                                 ",")] %>%
      stringr::str_extract('(?<=_s1tlw0m..).*(?=<)') %>%
      stringr::str_split(", ") %>%
      unlist()

    # Deal with Bonaire comma
    if (content[[length(content)]] == "Sint Eustatius and Saba") {
      content[[length(content) - 1]] <- "Bonaire, Sint Eustatius and Saba"
      content <- content[1:(length(content) - 1)]
    }

    return(
      scrape_result %>%
        mutate(
          city = content[[1]],
          region = if_else(length(content) == 3, content[[2]], NA_character_),
          country = content[[length(content)]],
          date = Sys.Date()
        ) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process _czm8crp results #################################################

  if (scrape_result$note == "_czm8crp") {

    return(
      scrape_result %>%
        mutate(
          location = purrr::map(.data$raw, ~{
            .x[stringr::str_detect(.x, "place is located in")] %>%
              stringr::str_extract("(?<=located in ).*(?=.</div)") %>%
              stringr::str_split(", ") %>%
              unlist()
          }),
          city = purrr::map_chr(.data$location, ~{
            if (length(.x) >= 2) .x[1] else NA_character_
          }),
          region = purrr::map_chr(.data$location, ~{
            if (length(.x) == 3) .x[2] else NA_character_
          }),
          country = purrr::map_chr(.data$location, ~{
            if (length(.x) >= 1) last(.x) else "PARSE ERROR TO CHECK"
          }),
          date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }

  ### Process 3-length results #################################################

  if (length(scrape_result$raw[[1]]) == 3) {

    return(
      scrape_result %>%
        mutate(
          city = purrr::map_chr(.data$raw, ~{
            .x[1] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          region = purrr::map_chr(.data$raw, ~{
            .x[2] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          country = purrr::map_chr(.data$raw, ~{
            .x[3] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          date = Sys.Date()
        ) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process 2-length results #################################################

  if (length(scrape_result$raw[[1]]) == 2) {

    return(
      scrape_result %>%
        mutate(
          city = purrr::map_chr(.data$raw, ~{
            .x[1] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          region = NA_character_,
          country = purrr::map_chr(.data$raw, ~{
            .x[2] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          date = Sys.Date()
        ) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process long-length results ##############################################

  if (length(scrape_result$raw[[1]]) > 3) {

    return(
      scrape_result %>%
        mutate(
          location = purrr::map(.data$raw, ~{
            .x[stringr::str_detect(.x, "place is located in")] %>%
              stringr::str_extract("(?<=located in ).*(?=.</div)") %>%
              stringr::str_split(", ") %>%
              unlist()
          }),
          city = purrr::map_chr(.data$location, ~{
            if (length(.x) >= 2) .x[1] else NA_character_
          }),
          region = purrr::map_chr(.data$location, ~{
            if (length(.x) == 3) .x[2] else NA_character_
          }),
          country = purrr::map_chr(.data$location, ~{
            if (length(.x) >= 1) last(.x) else "PARSE ERROR TO CHECK"
          }),
          date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }
}

