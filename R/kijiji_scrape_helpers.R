#' Helper function to download Craigslist or Kijiji listings
#'
#' \code{helper_download_listing} scrapes listings from a list of URLs.
#'
#' @param url_list A character vector of URLs to be scraped.
#' @param url_prefix A character scalar to be prepended to each URL.
#' @param url_suffix A character scalar to be appended to each URL.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function?
#' @return A list of HTML objects.
#' @importFrom dplyr %>% if_else mutate select tibble
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom readr parse_number
#' @importFrom stringr str_detect

helper_download_listing <- function(url_list, url_prefix = "", url_suffix = "",
                                    quiet) {

  i <- NULL

  # Default to no progress bar
  opts <- list()

  if (!quiet) {
    pb <- progress_bar$new(
      format = silver(italic(
        "Scraping listing :current of :total [:bar] :percent, ETA: :eta")),
      total = length(url_list),
      show_after = 0
    )

    pb$tick(0)
    pb_fun <- function(n) pb$tick()
    opts <- list(progress = pb_fun)
  }

  # Create temp directory to store html output
  dir_name <- paste0("temp_", sample(1E8, 1))
  dir.create(dir_name)

  # Scrape listings then write to temp directory
  foreach (i = seq_along(url_list), .options.snow = opts) %dopar% {

    listing <-
      tryCatch({
        read_html(GET(paste0(url_prefix, url_list[[i]], url_suffix),
                      options = "HUGE"))
      }, error = function(e) NULL)

    tryCatch({
      write_html(listing, paste0(dir_name, "/temp_", i, ".html"))
    }, error = function(e) NULL)

  }

  # Reconstitute listings
  listings <-
    map(seq_along(url_list), ~{
      tryCatch({
        read_html(paste0(dir_name, "/temp_", .x, ".html"))
      }, error = function(e) NULL)
    })

  # Delete temp files
  unlink(dir_name, recursive = TRUE)

  # Make sure that listings[[n]] is the right length if last element is NULL
  if (length(listings) != length(url_list)) {
    listings[length(url_list)] <- list(NULL)
  }

  listings

}


#' Helper function to parse scraped Kijiji listings
#'
#' \code{helper_parse_kijiji} parses a scraped Kijiji listing.
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

helper_parse_kijiji <- function(.x, .y, city_name) {

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
    mutate(
      bedrooms =
        str_extract(.data$details,
                    '(?<=coucher|Bedrooms).*(?=Salles|Bathrooms)'),
      bathrooms =
        str_extract(.data$details,
                    '(?<=bain|Bathrooms).*(?=Meubl|Furnished)'),
      furnished =
        str_extract(.data$details,
                    '(?<=Meubl\u00e9|Furnished).*(?=Animaux|Pet)')
    ) %>%
    mutate(
      furnished = case_when(
        .data$furnished %in% c("Oui", "Yes") ~ TRUE,
        .data$furnished %in% c("Non", "No") ~ FALSE,
        is.na(.data$furnished) ~ NA
      )
    ) %>%
    select(.data$id:.data$location, .data$bedrooms:.data$furnished,
           .data$details:.data$photos)
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



