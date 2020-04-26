#' Function to scrape Kijiji listings
#'
#' \code{upgo_scrape_kj} scrapes all data from Kijiji rental listings in
#' Montreal, Toronto or Vancouver.
#'
#' @param city A character vector indicating the city or cities to scrape.
#' Currently accepts "montreal", "toronto" or "vancouver" as inputs.
#' @param old_results A data frame. If the output of a previous run of
#' \code{upgo_scrape_kj} is supplied, listings previously scraped will be
#' incorporated into the new results.
#' @param short_long A character scalar. Should short-term rentals ("short"),
#' long-term rentals ("long") or both ("both", the default) be scraped?
#' @param recovery A logical vector. Should the function attempt to recover
#' results from a previous, unsuccessful function call?
#' @param proxies Character vector of IPs to use for proxy connections. If
#' supplied, this must be at least as long as the number of cores.
#' @param cores A positive integer scalar. How many processing cores should be
#' used to scrape?
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom crayon bold cyan italic silver
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr %>% arrange bind_rows if_else
#' @importFrom glue glue
#' @importFrom httr GET set_config use_proxy
#' @importFrom progress progress_bar
#' @importFrom purrr map map2_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom stringr str_extract
#' @importFrom xml2 read_html write_html
#' @export

upgo_scrape_kj <- function(city, old_results = NULL, short_long = "both",
                               recovery = FALSE, proxies = NULL, cores = 1L,
                               quiet = FALSE) {

  ### SETUP ####################################################################

  ## Initialize variables ------------------------------------------------------

  .temp_url_list_short <- .temp_url_list_long <- .temp_url_list <-
    .temp_listings <- .temp_results <- .temp_finished_flag <- i <- NULL

  url_start <- "https://www.kijiji.ca"
  url_end <- "?ad=offering&siteLocale=en_CA"

  # Default to no progress bar
  opts <- list()

  if (!quiet) {
    pb_fun <- function(n) pb$tick()
    opts <- list(progress = pb_fun)
  }


  ## Validate city argument ----------------------------------------------------

  if (!all(city %in% c("montreal", "Montreal", "montr\u00e9al", "Montr\u00e9al",
                       "toronto", "Toronto", "vancouver", "Vancouver"))) {

    stop("Invalid input to `city` argument. ",
         "Currently recognized cities are Montreal, Toronto and Vancouver.")
  }


  ## Restore object if recovery == TRUE ----------------------------------------

  if (recovery) {

    finished_flag <- get(".temp_finished_flag", envir = .GlobalEnv)

    # Check if finished_flag vector is the right length
    if (length(finished_flag) != length(city)) {
      stop("The recovery data does not match the inputs. ",
           "Try re-running with recovery = FALSE.")
    }

    if (short_long %in% c("short", "both")) {
      url_list_short <- get(".temp_url_list_short", envir = .GlobalEnv)
      on.exit(.temp_url_list_short <<- url_list_short)
    }

    if (short_long %in% c("long", "both")) {
      url_list_long <- get(".temp_url_list_long", envir = .GlobalEnv)
      on.exit(.temp_url_list_long <<- url_list_long, add = TRUE)
    }

    url_list <- get(".temp_url_list", envir = .GlobalEnv)
    on.exit(.temp_url_list <<- url_list, add = TRUE)

    listings <- get(".temp_listings", envir = .GlobalEnv)
    on.exit(.temp_listings <<- listings, add = TRUE)

    results <- get(".temp_results", envir = .GlobalEnv)
    on.exit(.temp_results <<- results, add = TRUE)

    on.exit(.temp_finished_flag <<- finished_flag, add = TRUE)


  ## Initialize objects if recovery == FALSE -----------------------------------

  } else {

    if (short_long %in% c("short", "both")) {
      url_list_short <- vector("list", length(city))
      on.exit(.temp_url_list_short <<- url_list_short)
    }

    if (short_long %in% c("long", "both")) {
      url_list_long <- vector("list", length(city))
      on.exit(.temp_url_list_long <<- url_list_long, add = TRUE)
    }

    url_list <- vector("list", length(city))
    on.exit(.temp_url_list <<- url_list, add = TRUE)

    listings <- vector("list", length(city))
    on.exit(.temp_listings <<- listings, add = TRUE)

    results <- vector("list", length(city))
    on.exit(.temp_results <<- results, add = TRUE)

    finished_flag <- map(seq_along(city), ~FALSE)
    on.exit(.temp_finished_flag <<- finished_flag, add = TRUE)

  }


  ## Initialize multicore processing and proxies -------------------------------

  if (!quiet && cores > 1) message(silver(glue(
    "Initializing {cores} processing threads.")))

  `%dopar%` <- foreach::`%dopar%`

  (cl <- cores %>% makeCluster()) %>% registerDoSNOW()

  if (!missing(proxies)) {

    proxy_reps <- ceiling(cores/length(proxies))
    proxy_list <- rep(proxies, proxy_reps)[seq_len(cores)]

    clusterApply(cl, proxy_list, function(x) {
      set_config(use_proxy(str_extract(x, '(?<=server=).*')))
    })
  }


  ### MAIN SCRAPING LOOP #######################################################

  for (n in seq_along(city)) {

    ## Get city_name -----------------------------------------------------------

    if (city[[n]] %in% c("montreal", "Montreal", "montr\u00e9al",
                         "Montr\u00e9al")) {
      city_name <- "Montreal"

    } else if (city[[n]] %in% c("toronto", "Toronto")) {
      city_name <- "Toronto"

    } else if (city[[n]] %in% c("vancouver", "Vancouver")) {
      city_name <- "Vancouver"
    }


    ## Skip city if it is already finished in recovery data --------------------

    if (finished_flag[[n]]) {
      if (!quiet) message(silver(bold(glue(
        "Recovery data for {city_name} detected; skipping scrape."))))
      next
    }

    if (!quiet) message(silver(bold(glue(
      "Scraping Kijiji rental listings in {city_name}."))))


    ## Construct listing page URLs ---------------------------------------------

    # STR
    if (short_long %in% c("short", "both")) {

      city_short <- case_when(
        city_name == "Montreal" ~
          c("/b-location-court-terme/ville-de-montreal/", "c42l1700281"),
        city_name == "Toronto" ~
          c("/b-short-term-rental/city-of-toronto/", "c42l1700273"),
        city_name == "Vancouver" ~
          c("/b-short-term-rental/vancouver/", "c42l1700287")
      )

      listings_url_short <-
        paste0(url_start, city_short[[1]], city_short[[2]], url_end)
    }


    # LTR
    if (short_long %in% c("long", "both")) {

      city_long <- case_when(
        city_name == "Montreal" ~
          c("/b-apartments-condos/ville-de-montreal/", "c37l1700281"),
        city_name == "Toronto" ~
          c("/b-apartments-condos/city-of-toronto/", "c37l1700273"),
        city_name == "Vancouver" ~
          c("/b-apartments-condos/vancouver/", "c37l1700287")
      )

      listings_url_long <-
        paste0(url_start, city_long[[1]], city_long[[2]], url_end)
    }


    ## Get STR URLs ------------------------------------------------------------

    if (short_long %in% c("short", "both")) {

      start_time <- Sys.time()

      # Find number of pages to scrape
      listings_to_scrape <-
        read_html(listings_url_short) %>%
        html_node(xpath = '//*[@class="showing"]') %>%
        html_text() %>%
        str_extract('(?<= of ).*(?=( Ads)|( results))') %>%
        parse_number() %>%
        as.integer()

      pages <- min(ceiling(listings_to_scrape / 40), 100)

      # Prepare progress bar
      if (!quiet) {
        pb <- progress_bar$new(format = silver(italic(
          "Scraping STR page :current of :total [:bar] :percent, ETA: :eta")),
          # If pages == 100, need to scrape again in ascending order
          total = if_else(pages == 100, 200, pages), show_after = 0)

        pb$tick(0)
      }

      # Scrape in descending order
      url_list_short[[n]] <-
        foreach (i = seq_len(pages), .options.snow = opts) %dopar% {

          tryCatch({
            suppressWarnings(
              read_html(GET(paste0(
                url_start, city_short[[1]], "page-", i, "/", city_short[[2]],
                url_end))) %>%
                html_nodes(xpath = '//*[@class="title"]') %>%
                str_extract('(?<=href=").*(?=" c)')
              )
            }, error = function(e) NULL)
        }

      url_list_short[[n]] <-
        paste0(url_start, unique(unlist(url_list_short[[n]])))

      # If pages == 100, scrape again in ascending order
      if (pages == 100) {

        url_list_short_2 <-
          foreach (i = seq_len(pages), .options.snow = opts) %dopar% {

          tryCatch({
            suppressWarnings(
              read_html(GET(paste0(
                url_start, city_short[[1]], "page-", i, "/", city_short[[2]],
                url_end, "&sort=dateAsc"))) %>%
                html_nodes(xpath = '//*[@class="title"]') %>%
                str_extract('(?<=href=").*(?=" c)')
              )
            }, error = function(e) NULL)
        }

        url_list_short[[n]] <-
          unique(c(
            url_list_short[[n]],
            paste0(url_start, unlist(url_list_short_2))
            ))
      }

      # Clean up
      total_time <- Sys.time() - start_time
      time_final_1 <- substr(total_time, 1, 4)
      time_final_2 <- attr(total_time, 'units')

      if (!quiet) {
        message(silver(length(url_list_short[[n]]),
                       "STR listing URLs scraped in "),
                cyan(time_final_1, time_final_2), silver("."))
      }
    }


    ## Get LTR URLs ------------------------------------------------------------

    if (short_long %in% c("long", "both")) {

      start_time <- Sys.time()

      # Find number of pages to scrape
      listings_to_scrape <-
        read_html(listings_url_long) %>%
        html_node(xpath = '//*[@class="showing"]') %>%
        html_text() %>%
        str_extract('(?<= of ).*(?=( Ads)|( results))') %>%
        parse_number() %>%
        as.integer()

      pages <- min(ceiling(listings_to_scrape / 40), 100L)

      # Prepare progress bar
      if (!quiet) {
        pb <- progress_bar$new(format = silver(italic(
          "Scraping LTR page :current of :total [:bar] :percent, ETA: :eta")),
          # If pages == 100, need to scrape again in ascending order
          total = if_else(pages == 100, 200, pages), show_after = 0)

        pb$tick(0)
      }

      # Scrape in descending order
      url_list_long[[n]] <-
        foreach (i = seq_len(pages), .options.snow = opts) %dopar% {

          tryCatch({
            suppressWarnings(
              read_html(paste0(
                url_start, city_long[[1]], "page-", i, "/", city_long[[2]],
                url_end)) %>%
                html_nodes(xpath = '//*[@class="title"]') %>%
                str_extract('(?<=href=").*(?=" c)')
              )
            }, error = function(e) NULL)
        }

      url_list_long[[n]] <-
        paste0(url_start, unique(unlist(url_list_long[[n]])))

      # If pages == 100, scrape again in ascending order
      if (pages == 100) {

        url_list_long_2 <-
          foreach (i = seq_len(pages), .options.snow = opts) %dopar% {

            tryCatch({
              suppressWarnings(
                read_html(paste0(
                  url_start, city_long[[1]], "page-", i, "/", city_long[[2]],
                  url_end, "&sort=dateAsc")) %>%
                  html_nodes(xpath = '//*[@class="title"]') %>%
                  str_extract('(?<=href=").*(?=" c)')
                )
              }, error = function(e) NULL)
          }

        url_list_long[[n]] <-
          unique(c(
            url_list_long[[n]],
            paste0(url_start, unlist(url_list_long_2))))
      }

      # Clean up
      total_time <- Sys.time() - start_time
      time_final_1 <- substr(total_time, 1, 4)
      time_final_2 <- attr(total_time, 'units')

      if (!quiet) {
        message(silver(length(url_list_long[[n]]),
                       "LTR listing URLs scraped in "),
                cyan(time_final_1, time_final_2), silver("."))
      }
    }


    ## Combine URLs into single list -------------------------------------------

    if (short_long == "both") {
      url_list[[n]] <- unique(c(url_list_short[[n]], url_list_long[[n]]))
    } else if (short_long == "short") {
      url_list[[n]] <- url_list_short[[n]]
    } else if (short_long == "long") {
      url_list[[n]] <- url_list_long[[n]]
    }

    url_list[[n]] <- url_list[[n]][url_list[[n]] != "https://www.kijiji.caNA"]
    url_list[[n]] <- str_replace(url_list[[n]], " ", "")


    ## Process duplicate listings if old_results is provided -------------------

    if (!missing(old_results)) {

      updated_results <-
        old_results %>%
        filter(city == city_name, url %in% url_list[[n]]) %>%
        mutate(scraped = Sys.Date())

      if (!quiet) message(silver(glue(
        "{nrow(updated_results)} previously scraped listings still active.")))

      old_results <-
        old_results %>%
        filter(city != city_name |
                 (city == city_name & !url %in% url_list[[n]])) %>%
        bind_rows(updated_results)

      url_list[[n]] <-
        url_list[[n]][!url_list[[n]] %in%
                        old_results[old_results$city == city_name,]$url]

    }


    ## Scrape individual pages -------------------------------------------------

    start_time <- Sys.time()

    listings[[n]] <-
      paste0(url_list[[n]], "?siteLocale=en_CA") %>%
      helper_download_listing()

    # Clean up
    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(listings[[n]]), "listings scraped in "),
              cyan(time_final_1, time_final_2), silver("."))
    }


    ## Parse HTML --------------------------------------------------------------

    start_time <- Sys.time()

    if (!quiet) {
      pb <- progress_bar$new(format = silver(italic(
        "Parsing result :current of :total [:bar] :percent, ETA: :eta")),
        total = length(listings[[n]]), show_after = 0)

      pb$tick(0)
    }

    results[[n]] <-
      map2_dfr(listings[[n]], url_list[[n]], ~{
        if (!quiet) pb$tick()
        helper_parse_kijiji(.x, .y, city_name)
        })


    ## Rbind with old_results if present, then arrange -------------------------

    if (!missing(old_results)) {
      results[[n]] <-
        old_results %>%
        filter(city == city_name) %>%
        bind_rows(results[[n]])
    }

    results[[n]] <-
      results[[n]] %>%
      arrange(.data$id)


    ## Set finished_flag upon successfully completing a city -------------------

    finished_flag[[n]] <- TRUE


    ## Clean up ----------------------------------------------------------------

    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(listings[[n]]), "listings parsed in "),
              cyan(time_final_1, time_final_2), silver("."))
    }
  }


  ### RBIND AND RETURN RESULTS #################################################

  results <- bind_rows(results)

  on.exit()

  results

}


