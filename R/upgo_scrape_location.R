#' Function to scrape location information from Airbnb listings
#'
#' \code{upgo_scrape_location} scrapes location (city, region and country) from
#' Airbnb listings.
#'
#' TKTK
#'
#' @param property An input table with a field named `property_ID` which will be
#' used to generate URLs for scraping.
#' @param chunk_size A positive integer. The number of listings to be scraped in
#' one batch. Higher values will increase performance, but may be more
#' vulnerable to rate-limiting and data loss.
#' @param proxies Character vector of IPs to use for proxy connections. If
#' supplied, this must be at least as long as the number of cores.
#' @param scrape_rate A positive integer. How many listings to scrape per
#' minute? This is a temporary setting while proper defaults are tested
#' empirically.
#' @param cores A positive integer scalar. How many processing cores should be
#' used to perform the computationally intensive intersection steps? The
#' implementation of multicore processing does not support Windows, so this
#' argument should be left with its default value of 1 in those cases.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with property_ID, city, region, and country, along with the
#' raw strings used to identify the location and the date on which the scrape
#' was performed.
#' @import doParallel
#' @import foreach
#' @import RSelenium
#' @importFrom crayon cyan silver
#' @importFrom dplyr arrange bind_rows filter last pull slice
#' @importFrom glue glue
#' @importFrom parallel clusterApply clusterCall clusterEvalQ makeCluster
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_extract_all str_replace
#' @importFrom stringr str_split str_starts
#' @importFrom utils flush.console
#' @export


upgo_scrape_location <- function(property, chunk_size = 100, proxies = NULL,
                                 scrape_rate = 75, cores = 1L, quiet = FALSE) {

  ### Initialization ###########################################################

  `%dopar%` <- foreach::`%dopar%`

  start_time <- Sys.time()

  .temp_results <- i <- proxy <- NULL


  ### Start clusters and web drivers ###########################################

  (cl <- cores %>% makeCluster()) %>% registerDoParallel()

  if (missing(proxies)) {
    clusterEvalQ(cl, {
      eCaps <- list(chromeOptions = list(
        args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
        w3c = FALSE
      ))

      remDr <-
        RSelenium::remoteDriver(browserName = "chrome",
                                extraCapabilities = eCaps)

      remDr$open()
    })
  } else {

    scrape_rate <- 0

    clusterApply(cl, proxies, function(x) {proxy <<- x})

    clusterEvalQ(cl, {
      eCaps <- list(chromeOptions = list(
        args = c('--headless', '--disable-gpu', '--window-size=1280,800',
                 proxy),
        w3c = FALSE
      ))

      remDr <-
        RSelenium::remoteDriver(browserName = "chrome",
                                extraCapabilities = eCaps)

      remDr$open()
    })


  }




  ### Extract HA listings ######################################################

  results <- results_new <-
    property %>%
    filter(str_starts(.data$property_ID, "ha-")) %>%
    select(.data$property_ID) %>%
    mutate(city = NA_character_,
           region = NA_character_,
           country = NA_character_,
           raw = list("HOMEAWAY"),
           date = Sys.Date())

  property <-
    property %>%
    filter(str_starts(.data$property_ID, "ab-"))

  if (!quiet) {
    message(silver(glue(
      "{nrow(results)} HomeAway listings removed. ",
      "{nrow(property)} listings to be scraped."
    )))
  }


  ### Prepare for main loop ####################################################

  # Set max tries slightly above the minimum needed to process all entries
  max_tries <-
    ceiling((nrow(property) / chunk_size) +
              2 * sqrt(nrow(property) / chunk_size))

  # Set other flags
  tries <- 0 # How many tries so far


  ### Main loop ################################################################

  while (nrow(filter(property,
                     !.data$property_ID %in% results$property_ID)) > 0 &
         tries < max_tries) {

    # Get time to measure total loop duration
    loop_start <- Sys.time()

    # Increment tries to set whether the loop can continue
    tries <- tries + 1

    # Get list of valid Airbnb PIDs to scrape
    PIDs <-
      property %>%
      filter(!.data$property_ID %in% results$property_ID) %>%
      slice(1:chunk_size) %>%
      pull(.data$property_ID) %>%
      substr(4, 15)

    # Do minimum possible work inside parallel processes
    results_raw <-
      foreach(i = seq_along(PIDs)) %dopar% {

        tryCatch(

          helper_scrape_location(PIDs[i], cores, scrape_rate),
          # Aggressive error handling to keep the function from exiting
          error = function(e) {
            tibble(property_ID = paste0("ab-", PIDs[i]),
                   raw = list(0))

          })
      }


    ### Parse new results and combine with main results object #################

    # Remove empty rows and parse results
    results_new <-
      results_raw %>%
      bind_rows() %>%
      filter(.data$property_ID %in% property$property_ID, !is.na(.data$raw)) %>%
      helper_scrape_location_parse()

    # Add new results to main object
    results <-
      results_new %>%
      bind_rows(results) %>%
      arrange(.data$property_ID)

    # Save backup in case of a future error
    .temp_results <<- results


    ### Report results of loop iteration #######################################

    loop_time <- Sys.time() - loop_start

    scrape_r <-
      round(nrow(results_new) / as.numeric(loop_time, units = 'mins'))

    remaining_listings <-
      nrow(filter(property, !.data$property_ID %in% results$property_ID))

    if (!quiet) {
      message(
        silver(glue("{nrow(results)} listings scraped (current rate ")),
        cyan(glue(
          "{scrape_r}/minute")),
        silver(glue(
          "). {remaining_listings} listings left."
          )))
      flush.console()
    }


    ### Wait to exit loop if scraping is too fast ##############################

    ## 80 listings/minute is safe maximum

    # time_allow <- chunk_size * 60 / scrape_rate
    # time_leftover <- max(time_allow - as.numeric(loop_time, units = 'secs'), 0)
    #
    # Sys.sleep(time_leftover)

  }


  ### Clean up and prepare output ##############################################

  # Shut down browsers
  clusterEvalQ(cl, {
    remDr$close()
  })

  # Stop parallel cluster
  doParallel::stopImplicitCluster()

  total_time <- Sys.time() - start_time

  if (nrow(property %>%
           filter(!.data$property_ID %in% results$property_ID)) > 0 &
      !quiet) {
    message("Scraping incomplete. ", nrow(results), " listings scraped in ",
            substr(total_time, 1, 5), " ",
            attr(total_time, "units"), ". ",
            nrow(property %>%
                   filter(!.data$property_ID %in% results$property_ID)),
            " listings not scraped.")

  } else if (!quiet) {
    message(crayon::bold(cyan(glue(
      "Scraping complete. {nrow(results)} listings scraped in ",
      "{substr(total_time, 1, 5)} {attr(total_time, 'units')}."
      ))))
  }

  # Delete temporary results object once it's clear the real object is safe
  rm(.temp_results, envir = .GlobalEnv)

  return(results)

}



