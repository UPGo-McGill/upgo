#' Function to scrape Kijiji listings
#'
#' \code{upgo_scrape_kijiji} scrapes all data from Kijiji rental listings in
#' Montreal, Toronto or Vancouver.
#'
#' @param city A character scalar indicating the city to scrape. Currently
#' accepts "montreal", "toronto" or "vancouver" as inputs.
#' @param old_results A data frame. If the output of a previous run of
#' \code{upgo_scrape_kijiji} is supplied, listings previously scraped will not
#' be scraped again.
#' @param short_long A character scalar. Should short-term rentals ("short"),
#' long-term rentals ("long") or both ("both", the default) be scraped?
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom crayon bold cyan italic silver
#' @importFrom dplyr %>%
#' @importFrom glue glue
#' @importFrom progress progress_bar
#' @importFrom purrr map2_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom stringr str_extract
#' @importFrom xml2 read_html
#' @export

upgo_scrape_kijiji <- function(city, old_results = NULL, short_long = "both",
                               quiet = FALSE) {

  ### Setup ####################################################################

  .temp_url_list_short <- .temp_url_list_long <- .temp_url_list <-
    .temp_listings <- NULL


  ### Construct listing page URL ###############################################

  url_start <- "https://www.kijiji.ca/"
  url_end <- "?ad=offering&siteLocale=en_CA"

  if (city %in% c("montreal", "Montreal", "montr\u00e9al", "Montr\u00e9al")) {

    city_name <- "Montreal"

    if (short_long %in% c("short", "both")) {

      city_short_1 <- "b-location-court-terme/ville-de-montreal/"
      city_short_2 <- "c42l1700281"

      listings_url_short <-
        paste0(url_start, city_short_1, city_short_2, url_end)

    }

    if (short_long %in% c("long", "both")) {

      city_long_1 <- "b-apartments-condos/ville-de-montreal/"
      city_long_2 <- "c37l1700281"

      listings_url_long <-
        paste0(url_start, city_long_1, city_long_2, url_end)

    }

  } else if (city %in% c("toronto", "Toronto")) {

    city_name <- "Toronto"

    if (short_long %in% c("short", "both")) {

      city_short_1 <- "b-short-term-rental/city-of-toronto/"
      city_short_2 <- "c42l1700273"

      listings_url_short <-
        paste0(url_start, city_short_1, city_short_2, url_end)

    }

    if (short_long %in% c("long", "both")) {

      city_long_1 <- "b-apartments-condos/city-of-toronto/"
      city_long_2 <- "c37l1700273"

      listings_url_long <-
        paste0(url_start, city_long_1, city_long_2, url_end)

    }

  } else if (city %in% c("vancouver", "Vancouver")) {

    city_name <- "Vancouver"

    if (short_long %in% c("short", "both")) {

      city_short_1 <- "b-short-term-rental/vancouver/"
      city_short_2 <- "c42l1700287"

      listings_url_short <-
        paste0(url_start, city_short_1, city_short_2, url_end)

    }

    if (short_long %in% c("long", "both")) {

      city_long_1 <- "b-apartments-condos/vancouver/"
      city_long_2 <- "c37l1700287"

      listings_url_long <-
        paste0(url_start, city_long_1, city_long_2, url_end)

    }
  }

  ### Get STR URLs #############################################################

  if (!quiet) message(silver(bold(glue(
    "Scraping Kijiji rental listings in {city_name}."))))

  if (short_long %in% c("short", "both")) {

    start_time <- Sys.time()

    listings_page <-
      read_html(listings_url_short)

    listings_to_scrape <-
      listings_page %>%
      html_node(xpath = '//*[@class="showing"]') %>%
      html_text() %>%
      str_extract('(?<= of ).*(?=( Ads)|( results))') %>%
      parse_number() %>%
      as.integer()

    pages <- min(ceiling(listings_to_scrape / 40), 100)

    url_list_short <- vector("list", pages)

    on.exit(.temp_url_list_short <<- url_list_short)

    if (!quiet) {
      pb <- progress_bar$new(
        format = silver(italic(
          "Scraping STR page :current of :total [:bar] :percent, ETA: :eta")),
        total = pages,
        show_after = 0
      )

      pb$tick(0)
    }

    for (i in seq_len(pages)) {

      if (!quiet) pb$tick()

      listings <-
        read_html(paste0(
          url_start,
          city_short_1,
          "page-",
          i,
          "/",
          city_short_2,
          url_end
          ))

      url_list_short[[i]] <-
        suppressWarnings(
          listings %>%
          html_nodes(xpath = '//*[@class="title"]') %>%
          str_extract('(?<=href=").*(?=" c)')
        )

    }

    url_list_short <- unique(unlist(url_list_short))

    if (pages == 100) {

      url_list_short_2 <- vector("list", pages)

      if (!quiet) {
        pb <- progress_bar$new(
          format = silver(italic(
            "Scraping STR page :current of :total [:bar] :percent, ETA: :eta")),
          total = pages,
          show_after = 0
        )

        pb$tick(0)
      }

      for (i in seq_len(pages)) {

        if (!quiet) pb$tick()

        listings <-
          read_html(paste0(
            url_start,
            city_short_1,
            "page-",
            i,
            "/",
            city_short_2,
            url_end,
            "&sort=dateAsc"
          ))

        url_list_short_2[[i]] <-
          suppressWarnings(
            listings %>%
              html_nodes(xpath = '//*[@class="title"]') %>%
              str_extract('(?<=href=").*(?=" c)')
          )

      }

      url_list_short <- unique(c(url_list_short, unlist(url_list_short_2)))

    }

    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(url_list_short), "STR listing URLs scraped in "),
        cyan(time_final_1, time_final_2), silver("."))
      }
  }


  ### Get LTR URLs #############################################################

  if (short_long %in% c("long", "both")) {

    start_time <- Sys.time()

    listings_page <-
      read_html(listings_url_long)

    listings_to_scrape <-
      listings_page %>%
      html_node(xpath = '//*[@class="showing"]') %>%
      html_text() %>%
      str_extract('(?<= of ).*(?=( Ads)|( results))') %>%
      parse_number() %>%
      as.integer()

    pages <- min(ceiling(listings_to_scrape / 40), 100L)

    url_list_long <- vector("list", pages)

    on.exit(.temp_url_list_long <<- url_list_long, add = TRUE)

    if (!quiet) {
      pb <- progress_bar$new(
        format = silver(italic(
          "Scraping LTR page :current of :total [:bar] :percent, ETA: :eta")),
        total = pages,
        show_after = 0
      )

      pb$tick(0)
    }

    for (i in seq_len(pages)) {

      if (!quiet) pb$tick()

      tryCatch({

        listings <-
          read_html(paste0(
            url_start,
            city_long_1,
            "page-",
            i,
            "/",
            city_long_2,
            url_end
          ))

        url_list_long[[i]] <-
          suppressWarnings(
            listings %>%
              html_nodes(xpath = '//*[@class="title"]') %>%
              str_extract('(?<=href=").*(?=" c)')
          )

      }, error = function(e) url_list_long[[i]] <- NULL)

    }

    url_list_long <- unique(unlist(url_list_long))

    if (pages == 100) {

      url_list_long_2 <- vector("list", pages)

      if (!quiet) {
        pb <- progress_bar$new(
          format = silver(italic(
            "Scraping LTR page :current of :total [:bar] :percent, ETA: :eta")),
          total = pages,
          show_after = 0
        )

        pb$tick(0)
      }

      for (i in seq_len(pages)) {

        if (!quiet) pb$tick()

        tryCatch({

          listings <-
            read_html(paste0(
              url_start,
              city_long_1,
              "page-",
              i,
              "/",
              city_long_2,
              url_end,
              "&sort=dateAsc"
            ))

          url_list_long_2[[i]] <-
            suppressWarnings(
              listings %>%
                html_nodes(xpath = '//*[@class="title"]') %>%
                str_extract('(?<=href=").*(?=" c)')
            )

        }, error = function(e) url_list_long_2[[i]] <- NULL)
      }

      url_list_long <- unique(c(url_list_long, unlist(url_list_long_2)))

    }

    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(url_list_short), "LTR listing URLs scraped in "),
              cyan(time_final_1, time_final_2), silver("."))
    }
  }


  ### Combine URLs into single list ############################################

  if (short_long == "both") {
    url_list <- unique(c(url_list_short, url_list_long))
  } else if (short_long == "short") {
    url_list <- url_list_short
  } else if (short_long == "long") {
    url_list <- url_list_long
  }

  url_list <- url_list[!is.na(url_list)]

  on.exit(.temp_url_list <<- url_list)

  ## Remove duplicate listings if old_results is provided

  if (!missing(old_results)) {

    url_list <-
      url_list[!str_replace(url_list, '^/', 'https://www.kijiji.ca/') %in%
                 old_results$url]

  }


  ### Scrape individual pages ##################################################

  listings <- vector("list", length(url_list))

  on.exit(.temp_listings <<- listings, add = TRUE)

  start_time <- Sys.time()

  if (!quiet) {
    pb <- progress_bar$new(
      format = silver(italic(
        "Scraping listing :current of :total [:bar] :percent, ETA: :eta")),
      total = length(url_list),
      show_after = 0
    )

    pb$tick(0)
  }

  for (i in seq_along(url_list)) {

    if (!quiet) pb$tick()

    listings[[i]] <-
      tryCatch({
        paste0("https://www.kijiji.ca", url_list[[i]], "?siteLocale=en_CA") %>%
          read_html(options = "HUGE")
      }, error = function(e) NULL)
  }


  ### Clean up #################################################################

  listings <- listings[!map_lgl(listings, is.null)]

  total_time <- Sys.time() - start_time
  time_final_1 <- substr(total_time, 1, 4)
  time_final_2 <- attr(total_time, 'units')

  if (!quiet) {
    message(silver(length(url_list_short), "listings scraped in "),
            cyan(time_final_1, time_final_2), silver("."))
  }


  ### Parse HTML ###############################################################

  if (!quiet) {
    pb <- progress_bar$new(
      format = silver(italic(
        "Parsing result :current of :total [:bar] :percent, ETA: :eta")),
      total = length(listings),
      show_after = 0
    )

    pb$tick(0)
  }

  results <-
    map2_dfr(listings, url_list, ~{

      if (!quiet) pb$tick()

      tryCatch({
        parse_results_kijiji(.x, .y)
      }, error = function(e) {

        redownload <- read_html(
          paste0("https://www.kijiji.ca", .y, "?siteLocale=en_CA"),
          options = "HUGE")

        parse_results_kijiji(redownload)

      })
    })

  on.exit()

  results

}


#' Helper function to parse scraped Kijiji listings
#'
#' \code{parse_results_kijiji} parses a scraped Kijiji listing.
#'
#' @param .x A single scraped Kijiji listing, as retrieved with read_html.
#' @param .y A single Kijiji URL.
#' @return A one-row data frame.
#' @importFrom dplyr %>% if_else tibble
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom readr parse_number
#' @importFrom stringr str_detect

parse_results_kijiji <- function(.x, .y) {

  # Should be 0 for valid listing, and 2 for 404 redirected
  redirect_check <-
    .x %>%
    html_node(xpath = 'body[@id = "PageSRP"]') %>%
    length()

  if (redirect_check == 2) {
    return(
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
        date = as.Date(NA),
        price = NA_real_,
        location = NA_character_,
        details = NA_character_,
        text = NA_character_,
        photos = vector("list", 1)
      )
    )
  }

  expired_check <-
    .x %>%
    html_node(xpath = '//*[@id = "PageExpiredVIP"]') %>%
    html_text()

  if (!is.na(expired_check)) {
    return(
      tibble(
        id =
          .x %>%
          html_node("head") %>%
          html_node(xpath = 'link[@rel = "canonical"]/@href') %>%
          html_text() %>%
          str_extract('(?<=/)[:digit:]*$'),
        url =
          paste0("https://www.kijiji.ca", .y),
        title = NA_character_,
        short_long = if_else(
          str_detect(url, "v-location-court-terme|v-short-term-rental"),
          "short",
          "long"),
        date = as.Date(NA),
        price = NA_real_,
        location = NA_character_,
        details = NA_character_,
        text = NA_character_,
        photos = vector("list", 1)
      )
    )
  }

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
    date =
      .x %>%
      html_node(xpath = '//*/time/@datetime') %>%
      html_text() %>%
      as.Date(),
    price =
      .x %>%
      html_node(xpath = '//*[@class = "priceContainer-1419890179"]') %>%
      html_node(xpath = 'span') %>%
      html_node(xpath = 'span/@content') %>%
      html_text() %>%
      parse_number(),
    location =
      .x %>%
      html_node(xpath = '//*[@class = "address-3617944557"]') %>%
      html_text(),
    details =
      .x %>%
      html_node(xpath =
                  '//*[@class = "attributeListWrapper-2108313769"]') %>%
      html_text(),
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
    )
}

