#' Helper function to parse scraped Kijiji listings
#'
#' \code{helper_parse_kj} parses a scraped Kijiji listing.
#'
#' @param x A single scraped Kijiji listing, as retrieved with
#' \code{get_listings_kj}.
#' @param city_name A character scalar indicating the name of the city in which
#' the listing is located.
#' @return A one-row data frame.
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom purrr map_dfr
#' @importFrom stringr str_detect str_replace_all
#' @export

parse_listing_kj <- function(x, city_name, proxies = NULL, quiet = FALSE) {

  ## Initialize variables and environments -------------------------------------

  helper_require("rvest")


  ## Catch 404s and exit early -------------------------------------------------

  if (x$status_code == 404) return(helper_error_kj())


  ## Read listing and check for expired links ----------------------------------

  listing <- xml2::read_html(x, options = "HUGE")

  expired_check <-
    listing %>%
    rvest::html_node(xpath = '//*[@id = "PageExpiredVIP"]') %>%
    rvest::html_text()

  if (!is.na(expired_check)) return(helper_error_kj())


  ## Check for missing text field ----------------------------------------------

  # text_check <-
  #   tryCatch({
  #     listing %>%
  #       rvest::html_node(
  #         xpath = '//*[@class = "descriptionContainer-3544745383"]') %>%
  #       rvest::html_node('div') %>%
  #       rvest::html_text()
  #     TRUE},
  #     error = function(e) FALSE)
  #
  # # If the text field still isn't present, exit function
  # if (!text_check) return(helper_error_kj())


  ## Find details class --------------------------------------------------------

  x_details <-
    listing %>%
    rvest::html_node(xpath = '//*[@id="mainPageContent"]') %>%
    xml2::xml_child(2) %>%
    rvest::html_text()

  if (is.na(x_details)) return(helper_error_kj())


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
      listing %>%
      rvest::html_node(xpath = '//*[@class = "adId-4111206830"]') %>%
      rvest::html_text() %>%
      paste0("kj-", .),
    url =
      x$request[[2]],
    title =
      listing %>%
      rvest::html_node("head") %>%
      rvest::html_node("title") %>%
      rvest::html_text(),
    short_long = if_else(
      stringr::str_detect(url, "v-location-court-terme|v-short-term-rental"),
      "short",
      "long"),
    created =
      listing %>%
      rvest::html_node(xpath = '//*/time/@datetime') %>%
      rvest::html_text() %>%
      as.Date(),
    scraped = Sys.Date(),
    price =
      listing %>%
      rvest::html_node(xpath = '//*[@class = "priceContainer-1419890179"]') %>%
      rvest::html_node(xpath = 'span') %>%
      rvest::html_node(xpath = 'span/@content') %>%
      rvest::html_text() %>%
      stringr::str_replace("\\..*$", "") %>%
      as.numeric(),
    city =
      city_name,
    location =
      listing %>%
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
      listing %>%
      rvest::html_node(xpath =
                         '//*[@class = "descriptionContainer-3544745383"]') %>%
      rvest::html_node('div') %>%
      rvest::html_text(),
    photos = suppressWarnings(list(
      listing %>%
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
