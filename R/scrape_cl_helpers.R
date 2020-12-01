#' Helper function to scrape Craigslist listing URLs
#'
#' \code{helper_urls_cl} scrapes Craigslist listing URLs for a city.
#'
#' @param city_name A character string: the city to be scraped.
#' @param proxies Character vector of IPs to use for proxy connections.
#' @importFrom progressr handlers handler_progress progressor
#' @return A list of URLs.

helper_urls_cl <- function(city_name, proxies = NULL) {

  helper_require("rvest")
  doParallel::registerDoParallel()
  `%do_par%` <- foreach::`%dopar%`


  ## Establish proxy -----------------------------------------------------------

  if (!missing(proxies)) {
    rand <- ceiling(runif(1, 1, length(proxies)))
    httr::set_config(httr::use_proxy(proxies[[rand]]))
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


  ## Scrape pages --------------------------------------------------------------

  handler_upgo("Scraping page")

  progressr::with_progress({

    pb <- progressor(steps = pages)

    url_list <-
      foreach::foreach(i = seq_len(pages)) %do_par% {

        # if (!missing(proxies)) {
        httr::set_config(httr::use_proxy(
          proxies[[(i %% length(proxies)) + 1]]))
        # }

        pb()

        tryCatch({
          xml2::read_html(httr::GET(paste0(
            "https://", city_name, ".craigslist.org/search/apa?s=",
            120 * (i - 1), "&lang=en&cc=us"))) %>%
            rvest::html_nodes(".result-row") %>%
            xml2::xml_children() %>%
            rvest::html_attr("href") %>%
            stats::na.omit()
        }, error = function(e) NULL)
      }

  })


  ## Merge and clean up list ---------------------------------------------------

  url_list  <- unique(unlist(url_list)) %>% str_replace("\\?.*", "")

  return(url_list)

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
          rvest::html_node(xpath = 'h1[@class = "postingtitle"]') %>%
          rvest::html_node(xpath = 'span/span[@class = "price"]') %>%
          rvest::html_text() %>%
          stringr::str_replace_all("\\$|[:space:]|,", "") %>%
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
