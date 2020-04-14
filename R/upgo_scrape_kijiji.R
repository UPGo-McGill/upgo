#' Function to scrape Kijiji listings
#'
#' \code{upgo_scrape_kijiji} scrapes all data from Kijiji rental listings in
#' Montreal, Toronto or Vancouver.
#'
#' @param city A character scalar indicating the city to scrape. Currently
#' accepts "montreal", "toronto" or "vancouver" as inputs.
#' @param short_long A character scalar. Should short-term rentals ("short"),
#' long-term rentals ("long") or both ("both", the default) be scraped?
#' @param old_results A data frame. If the output of a previous run of
#' \code{upgo_scrape_kijiji} is supplied, listings previously scraped will not
#' be scraped again.
#' @param timeout A positive numeric scalar. How long in seconds should the
#' function pause between scrape attempts to avoid triggering cooldowns?
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom dplyr %>%
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom stringr str_extract
#' @importFrom xml2 read_html
#' @export

upgo_scrape_kijiji <- function(city, short_long = "both", old_results = NULL,
                               timeout = 0.2, quiet = FALSE) {

  ### Setup ####################################################################

  .temp_url_list_short <- .temp_url_list_long <- .temp_url_list <-
    .temp_listings <- NULL


  ### Construct listing page URL ###############################################

  url_start <- "https://www.kijiji.ca/"
  url_end <- "?ad=offering&siteLocale=en_CA"

  if (city %in% c("montreal", "Montreal", "montr\u00e9al", "Montr\u00e9al")) {

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

  if (short_long %in% c("short", "both")) {

    on.exit(.temp_url_list_short <<- url_list_short)

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

    for (i in seq_len(pages)) {

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

      if (!quiet) message("\rSTR page ", i, " of ", pages, " scraped.",
                          appendLF = FALSE)

    }

    url_list_short <- unique(unlist(url_list_short))

    if (pages == 100) {

      url_list_short_2 <- vector("list", pages)

      for (i in seq_len(pages)) {

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

        if (!quiet) message("\r STR page ", i, " of ", pages,
                            " scraped (in ascending order).",
                            appendLF = FALSE)

      }

      url_list_short <- unique(c(url_list_short, unlist(url_list_short_2)))

    }

    message("\n", length(url_list_short), " STR listing URLs scraped.")
  }


  ### Get LTR URLs #############################################################

  if (short_long %in% c("long", "both")) {

    on.exit(.temp_url_list_long <<- url_list_long, add = TRUE)

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

    for (i in seq_len(pages)) {

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

      if (!quiet) message("\rLTR page ", i, " of ", pages, " scraped.",
                          appendLF = FALSE)

    }

    url_list_long <- unique(unlist(url_list_long))

    if (pages == 100) {

      url_list_long_2 <- vector("list", pages)

      for (i in seq_len(pages)) {
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

        if (!quiet) message("\rLTR page ", i, " of ", pages,
                            " scraped (in ascending order).",
                            appendLF = FALSE)

      }

      url_list_long <- unique(c(url_list_long, unlist(url_list_long_2)))

    }

    message("\n", length(url_list_long), " LTR listing URLs scraped.")
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

  message(length(url_list), " listings ready to be scraped.",
          appendLF = TRUE)


  ### Scrape individual pages ##################################################

  listings <- vector("list", length(url_list))

  on.exit(.temp_listings <<- listings, add = TRUE)

  for (i in seq_along(url_list)) {

    listings[[i]] <-
      tryCatch({
        paste0("https://www.kijiji.ca", url_list[[i]], "?siteLocale=en_CA") %>%
          read_html(options = "HUGE")
      }, error = function(e) NULL)

    if (!quiet) message("\rListing ", i, " of ", length(url_list), " scraped.",
                        appendLF = FALSE)

    Sys.sleep(timeout)

  }


  ### Clean up #################################################################

  listings <- listings[!map_lgl(listings, is.null)]

  message("\n", length(listings), " successfully scraped.")


  ### Parse HTML ###############################################################

  results <-
    listings %>%
    map_dfr(~{

      tryCatch({
        parse_results_kijiji(.x)
      }, error = function(e) {

        url <-
          .x %>%
          html_node("head") %>%
          html_node(xpath = 'link[@rel = "canonical"]/@href') %>%
          html_text() %>%
          paste0("?siteLocale=en_CA")

        redownload <- read_html(url, options = "HUGE")

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
#' @return A one-row data frame.
#' @importFrom dplyr %>% if_else tibble
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom readr parse_number
#' @importFrom stringr str_detect

parse_results_kijiji <- function(.x) {

  expired_check <-
    .x %>%
    html_node(xpath = '//*[@id = "PageExpiredVIP"]') %>%
    html_text()

  if (!is.na(expired_check)) {
    tibble(
      id =
        .x %>%
        html_node("head") %>%
        html_node(xpath = 'link[@rel = "canonical"]/@href') %>%
        html_text() %>%
        str_extract('(?<=/)[:digit:]*$'),
      url =
        .x %>%
        html_node("head") %>%
        html_node(xpath = 'link[@rel = "canonical"]/@href') %>%
        html_text(),
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
  } else {
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
          str_extract('(?<=image:url..).*(?=..;back)'))))
  }

}

