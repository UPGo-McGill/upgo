#' Scrape registration numbers from Airbnb listings
#'
#' \code{upgo_scrape_ab_registration} scrapes registration numbers from
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
#' @param quiet A logical scalar Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with property_ID, registration and date fields.
#' @import foreach
#' @importFrom crayon cyan silver
#' @importFrom dplyr arrange bind_rows filter last pull slice tibble
#' @importFrom glue glue
#' @importFrom purrr map_chr
#' @importFrom stringr str_starts
#' @export


upgo_scrape_ab_registration <- function(property, proxies = NULL, cores = 1L,
                                        quiet = FALSE) {

  ### Check for necessary packages #############################################

  helper_require("doFuture")
  helper_require("future")
  helper_require("parallel")
  helper_require("RSelenium")


  ### Initialization ###########################################################

  `%dopar%` <- foreach::`%dopar%`

  start_time <- Sys.time()

  .temp_results <- i <- NULL

  results <-
    tibble(property_ID = character(),
           registration = character(),
           date = Sys.Date())


  ### Set initial on.exit statement ############################################

  on.exit({

    if (exists(results_HA)) results <- bind_rows(results, results_HA)

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

  # Need "sequential" here as workaround to R 4 / RStudio 1.3 bug
  cl <- parallel::makeCluster(cores, setup_strategy = "sequential")
  future::plan(future::cluster, workers = cl, persistent = TRUE)
  doFuture::registerDoFuture()


  ## Start up with no proxies --------------------------------------------------

  if (missing(proxies)) {
    parallel::clusterEvalQ(cl, {
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

    parallel::clusterApply(cl, proxies, function(x) {proxy <<- x})

    parallel::clusterEvalQ(cl, {
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
    mutate(registration = "HOMEAWAY",
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


  ## Loop with progress bar ----------------------------------------------------

  if (!quiet) {

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
                helper_scrape_ab_registration()
            }, error = function(e) NULL)
          }

        results <- bind_rows(results, results_new)
      }
    })


    ## Loop without progress bar -------------------------------------------------

  } else {

    for (i in seq_len(scrape_rounds)) {

      PIDs_to_scrape <-
        PIDs[(((i - 1) * chunk_size) + 1):(i * chunk_size)]

      results_new <-
        foreach(j = seq_along(PIDs_to_scrape)) %dopar% {

          tryCatch({
            PIDs_to_scrape[[j]] %>%
              helper_scrape_ab_registration()
          }, error = function(e) NULL)
        }

      results <- bind_rows(results, results_new)
    }
  }



  ### Clean up and prepare output ##############################################

  # Shut down browsers
  parallel::clusterEvalQ(cl, {
    remDr$close()
  })

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

