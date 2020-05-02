#' Function to scrape location information from Airbnb listings
#'
#' \code{upgo_scrape_ab} scrapes location (city, region and country) from
#' Airbnb listings.
#'
#' TKTK
#'
#' @param property An input table with a field named `property_ID` which will be
#' used to generate URLs for scraping.
#' @param proxies Character vector of IPs to use for proxy connections. If
#' supplied, this must be at least as long as the number of cores.
#' @param cores A positive integer scalar. How many processing cores should be
#' used to scrape?
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


upgo_scrape_ab <- function(property, proxies = NULL, cores = 1L,
                           quiet = FALSE) {

  ### Initialization ###########################################################

  `%dopar%` <- foreach::`%dopar%`

  start_time <- Sys.time()

  .temp_results <- i <- NULL

  results <-
    tibble(property_ID = character(),
           city = character(),
           region = character(),
           country = character(),
           raw = list(),
           date = Sys.Date())

  # Put null progress bar in .upgo_env
  .upgo_env$pb <-progressor(0)


  ### Set initial on.exit statement ############################################

  on.exit({

    results <- bind_rows(results, results_HA)

    total_time <- Sys.time() - start_time

    if (!quiet) {
      message("Scraping incomplete. ", nrow(results), " listings scraped in ",
              substr(total_time, 1, 5), " ",
              attr(total_time, "units"), ". ",
              nrow(property %>%
                     filter(!.data$property_ID %in% results$property_ID)),
              " listings not scraped.")

    }

    return(results)

  })


  ### Start clusters and web drivers ###########################################

  cl <- makeCluster(cores)
  future::plan(future::cluster, workers = cl, persistent = TRUE)
  doFuture::registerDoFuture()


  ## Start up with no proxies --------------------------------------------------

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


  ## Start up with proxies -----------------------------------------------------

  } else {

    proxy_reps <- ceiling(cores / length(proxies))
    proxies <- rep(proxies, proxy_reps)[seq_len(cores)]
    proxies <- paste0("--proxy-server=", proxies)

    if (!quiet) {
      message(silver(glue("Scraping with {cores} proxies.")))
    }

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

  results_HA <-
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
      "{nrow(results_HA)} HomeAway listings removed. ",
      "{nrow(property)} listings to be scraped."
    )))
  }


  ### Main loop ################################################################

  # Get list of valid Airbnb PIDs to scrape
  PIDs <-
    property %>%
    filter(!.data$property_ID %in% results$property_ID) %>%
    pull(.data$property_ID) %>%
    substr(4, 15)

  # Set up progress bar
  if (!quiet) {
    handler_upgo("Scraping listing")
  }

  # Get number of iterations
  chunk_size <- 100
  scrape_rounds <- ceiling(length(PIDs) / chunk_size)

  # Loop over the number of iterations
  with_progress({

    .upgo_env$pb <- progressor(along = PIDs)
    .upgo_env$pb(0)

    for (i in seq_len(scrape_rounds)) {

      PIDs_to_scrape <-
        PIDs[(((i - 1) * chunk_size) + 1):(i * chunk_size)]

      results_new <-
        foreach(j = seq_along(PIDs_to_scrape)) %dopar% {

          .upgo_env$pb()

          tryCatch({
            PIDs_to_scrape[[j]] %>%
              helper_scrape_ab() %>%
              helper_parse_ab()
            }, error = function(e) NULL)
          }

      results <- bind_rows(results, results_new)

    }

  })


  ### Clean up and prepare output ##############################################

  # Shut down browsers
  clusterEvalQ(cl, {
    remDr$close()
  })

  # Stop parallel cluster
  doParallel::stopImplicitCluster()

  # Rbind HA listings into scraped output
  results <- bind_rows(results, results_HA)

  total_time <- Sys.time() - start_time

  if (!quiet) {
    message(crayon::bold(crayon::cyan(glue(
      "Scraping complete. {nrow(results)} listings scraped in ",
      "{substr(total_time, 1, 5)} {attr(total_time, 'units')}."
    ))))
  }

  on.exit()

  # Satisfy R CMD check
  proxy <- NULL

  return(results)

}

