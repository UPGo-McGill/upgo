#' Helper function to scrape Craigslist listing URLs
#'
#' \code{helper_cl_urls} scrapes Craigslist listing URLs for a city.
#'
#' @param city_name A character string: the city to be scraped.
#' @importFrom progressr handlers handler_progress progressor
#' @importFrom rvest html_attr
#' @importFrom stats na.omit
#' @importFrom xml2 xml_children
#' @return A list of URLs.

helper_cl_urls <- function(city_name) {

  ## Define environment for %do_upgo% function ---------------------------------

  environment(`%do_upgo%`) <- environment()


  ## Construct listing page URL ------------------------------------------------

  listings_url <-
    paste0("https://", city_name,
           ".craigslist.org/search/apa?s=0&lang=en&cc=us")


  ## Find number of pages to scrape --------------------------------------------

  listings_to_scrape <-
    listings_url %>%
    xml2::read_html() %>%
    rvest::html_node(".totalcount") %>%
    rvest::html_text()

  pages <- ceiling(as.numeric(listings_to_scrape) / 120)


  ## Prepare progress reporting ------------------------------------------------

  .upgo_env$pb <- progressor(steps = pages)


  ## Scrape pages --------------------------------------------------------------

  url_list <-
    foreach(i = seq_len(pages)) %do_upgo% {
      xml2::read_html(httr::GET(paste0(
        "https://", city_name, ".craigslist.org/search/apa?s=",
        120 * (i - 1), "&lang=en&cc=us"))) %>%
        rvest::html_nodes(".result-row") %>%
        xml2::xml_children() %>%
        rvest::html_attr("href") %>%
        na.omit()
    }


  ## Merge and clean up list ---------------------------------------------------

  url_list <- unique(unlist(url_list)) %>% str_replace("\\?.*", "")

  return(url_list)

}


#' Helper function to download Craigslist or Kijiji listings
#'
#' \code{helper_download_listing} scrapes listings from a list of URLs.
#'
#' @param urls A character vector of URLs to be scraped.
#' @return A list of HTML objects.
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom foreach foreach
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom readr parse_number
#' @importFrom stringr str_detect

helper_download_listing <- function(urls) {

  ## Define environment for %do_upgo% function ---------------------------------

  environment(`%do_upgo%`) <- environment()


  ## Prepare progress reporting ------------------------------------------------

  .upgo_env$pb <- progressor(along = urls)


  ## Scrape listings then write to temp directory ------------------------------

  listings <-
    foreach(i = seq_along(urls)) %do_upgo% {
      tryCatch({
        httr::GET(urls[[i]], httr::timeout(1))
      },
               error = function(e) {
                 httr::reset_config()
                 httr::GET(urls[[i]], httr::timeout(1))
               })
  }


  ## Reconstitute listings and delete temp files -------------------------------

  listings <- purrr::map(listings, read_html, options = "HUGE")

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
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom readr parse_number
#' @importFrom stringr str_detect str_replace_all

helper_parse_kj <- function(.x, .y, city_name) {

  ### Test if the input is valid, and redownload if not ########################

  retry <- tryCatch({html_node(.x, "head"); FALSE}, error = function(e) TRUE)

  if (retry) {
    .x <- read_html(paste0(.y, "?siteLocale=en_CA"), options = "HUGE")
  }


  ### Exit if the input is still not valid #####################################

  fail <- tryCatch({html_node(.x, "head"); FALSE}, error = function(e) TRUE)

  if (fail) return(helper_error_result(.y, city_name))


  ### Check for 404 redirects and expired links ################################

  # Should be 0 for valid listing, and 2 for 404 redirected
  redirect_check <-
    .x %>%
    html_node(xpath = 'body[@id = "PageSRP"]') %>%
    length()

  if (redirect_check == 2) return(helper_error_result(.y, city_name))

  expired_check <-
    .x %>%
    html_node(xpath = '//*[@id = "PageExpiredVIP"]') %>%
    html_text()

  if (!is.na(expired_check)) return(helper_error_result(.y, city_name))


  ### Parse input ##############################################################

  ## Find details class --------------------------------------------------------

  x_details <-
    helper_detail_parse(.x)

  # If the field isn't correctly retrieved, try again with fresh download
  if (is.na(x_details)) {
    .x <- read_html(paste0(.y, "?siteLocale=en_CA"), options = "HUGE")

    x_details <- helper_detail_parse(.x)
  }


  ## Produce output tibble -----------------------------------------------------

  tibble(
    id =
      .x %>%
      html_node(xpath = '//*[@class = "adId-4111206830"]') %>%
      html_text(),
    url =
      .x %>%
      html_node("head") %>%
      html_node(xpath = 'link[@rel = "canonical"]/@href') %>%
      html_text(),
    title =
      .x %>%
      html_node("head") %>%
      html_node("title") %>%
      html_text(),
    short_long = if_else(
      str_detect(url, "v-location-court-terme|v-short-term-rental"),
      "short",
      "long"),
    created =
      .x %>%
      html_node(xpath = '//*/time/@datetime') %>%
      html_text() %>%
      as.Date(),
    scraped = Sys.Date(),
    price =
      .x %>%
      html_node(xpath = '//*[@class = "priceContainer-1419890179"]') %>%
      html_node(xpath = 'span') %>%
      html_node(xpath = 'span/@content') %>%
      html_text() %>%
      parse_number(),
    city =
      city_name,
    location =
      .x %>%
      html_node(xpath = '//*[@class = "address-3617944557"]') %>%
      html_text(),
    bedrooms =
      x_details %>%
      str_extract('(?<=coucher|Bedrooms).*(?=Salles|Bathrooms)'),
    bathrooms =
      x_details %>%
      str_extract('(?<=bain|Bathrooms).*(?=Meubl|Furnished)'),
    furnished =
      x_details %>% str_extract('(?<=Meubl\u00e9|Furnished).*(?=Animaux|Pet)'),
    details =
      x_details,
    text =
      .x %>%
      html_node(xpath =
                  '//*[@class = "descriptionContainer-3544745383"]') %>%
      html_node('div') %>%
      html_text(),
    photos = suppressWarnings(list(
      .x %>%
        html_nodes(
          xpath =
            '//*[@class = "heroImageBackground-4116888288 backgroundImage"]'
        ) %>%
        str_extract('(?<=image:url..).*(?=..;back)')))
  ) %>%
    mutate(furnished = case_when(.data$furnished %in% c("Oui", "Yes") ~ TRUE,
                                 .data$furnished %in% c("Non", "No") ~ FALSE,
                                 is.na(.data$furnished) ~ NA))
}


#' Helper function to generate error Kijiji output
#'
#' @param .y A single Kijiji URL.
#' @param city_name A character scalar indicating the name of the city in which
#' the listing is located.
#' @return A one-row data frame.
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom readr parse_number
#' @importFrom stringr str_detect

helper_error_result <- function(.y, city_name) {

  tibble(
    id =
      .y %>%
      str_extract('(?<=/)[:digit:]*$'),
    url =
      paste0("https://www.kijiji.ca", .y),
    title = NA_character_,
    short_long = if_else(
      str_detect(url, "v-location-court-terme|v-short-term-rental"),
      "short",
      "long"),
    created = as.Date(NA),
    scraped = Sys.Date(),
    price = NA_real_,
    city = city_name,
    location = NA_character_,
    bedrooms = NA_character_,
    bathrooms = NA_character_,
    furnished = NA,
    details = NA_character_,
    text = NA_character_,
    photos = vector("list", 1)
  )

}


#' Helper function to generate Kijiji detail field
#'
#' @param .x A single scraped Kijiji listing.
#' @return A character scalar.
#' @importFrom rvest html_node html_nodes html_text

helper_detail_parse <- function(.x) {

  x_details <-
    .x %>%
    html_node(xpath = '//*[@class = "itemAttributeCards-2416600896"]'
    ) %>%
    html_text() %>%
    str_replace_all("\n", "")

  if (is.na(x_details)) {
    x_details <-
      .x %>%
      html_node(xpath =
                  '//*[@class = "attributeListWrapper-2108313769"]') %>%
      html_text() %>%
      str_replace_all("\n", "")
  }

  if (is.na(x_details)) {
    x_details <-
      .x %>%
      html_node(xpath = paste0('//*[@class = "itemAttributeCards-2416600896 ',
                               'itemAttributeCards__fourCards-3070740560"]')
      ) %>%
      html_text() %>%
      str_replace_all("\n", "")
  }

  if (is.na(x_details)) {
    x_details <-
      .x %>%
      html_node(xpath = paste0('//*[@class = "itemAttributeCards-2416600896 ',
                               'itemAttributeCards__twoCards-934340663"]')
      ) %>%
      html_text() %>%
      str_replace_all("\n", "")
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
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom readr parse_number
#' @importFrom stringr str_detect str_replace_all

helper_parse_cl <- function(.x, .y, city_name) {

  tibble(
    id = tryCatch({
      .x %>%
        html_node(xpath = '/html/body/section[@class="page-container"]') %>%
        html_node(xpath = 'section[@class="body"]') %>%
        html_node(xpath = 'section[@class="userbody"]') %>%
        html_node(xpath = 'div[@class="postinginfos"]') %>%
        html_node("p") %>%
        html_text() %>%
        str_extract('(?<=id: ).*')
    }, error = function(e) NA_character_),
    url =
      .x %>%
      html_node(xpath = '/html/head/link[@rel="canonical"]/@href') %>%
      html_text(),
    title =
      .x %>%
      html_node(xpath = '/html/head/title') %>%
      html_text(),
    created =
      .x %>%
      html_node(xpath = '//*[@id="display-date"]/time/@datetime') %>%
      html_text(),
    scraped =
      Sys.Date(),
    price =
      tryCatch({
        .x %>%
          html_node(xpath = '/html/body') %>%
          html_node(xpath = 'section[@class = "page-container"]') %>%
          html_node(xpath = 'section[@class = "body"]') %>%
          html_node(xpath = 'h2[@class = "postingtitle"]') %>%
          html_node(xpath = 'span/span[@class = "price"]') %>%
          html_text() %>%
          readr::parse_number()
      }, error = function(e) NA_real_),
    city =
      city_name,
    location =
      .x %>%
      html_node(xpath = '/html/head') %>%
      html_node(xpath = 'meta[@name = "geo.position"]/@content') %>%
      html_text(),
    bedrooms =
      NA_character_,
    bathrooms =
      NA_character_,
    furnished =
      NA,
    details =
      tryCatch({
        .x %>%
          html_node(xpath = '/html/body/section[@class="page-container"]') %>%
          html_node(xpath = 'section[@class="body"]') %>%
          html_node(xpath = 'section[@class="userbody"]') %>%
          html_node(xpath = 'div[@class = "mapAndAttrs"]') %>%
          html_nodes(xpath = 'p') %>%
          html_text() %>%
          str_replace_all("\n", "") %>%
          str_replace_all("  ", " ") %>%
          str_replace_all("  ", " ") %>%
          str_replace_all("  ", " ") %>%
          str_replace_all("  ", " ") %>%
          str_extract('(?<= ).*(?= )') %>%
          paste(collapse = "; ")
      }, error = function(e) NA_character_),
    text =
      tryCatch({
        .x %>%
          html_node(xpath = '/html/body/section[@class="page-container"]') %>%
          html_node(xpath = 'section[@class= "body"]') %>%
          html_node(xpath = 'section[@class= "userbody"]') %>%
          html_node(xpath = 'section[@id = "postingbody"]') %>%
          html_text() %>%
          str_replace(
            "\n            QR Code Link to This Post\n            \n        \n",
            "") %>%
          str_replace_all("\u2022\t", "") %>%
          str_replace_all("\n", " ")
      }, error = function(e) NA_character_),
    photos =
      list(.x %>%
             html_nodes(xpath = '//*/img') %>%
             html_node(xpath = '@src') %>%
             html_text() %>%
             # `[`(2:length(.)) %>%
             str_replace("50x50c", "600x450") %>%
             unique())
  )

}



