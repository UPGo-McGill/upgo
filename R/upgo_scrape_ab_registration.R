#' Scrape registration numbers from Airbnb listings
#'
#' \code{upgo_scrape_ab_registration} scrapes registration numbers from
#' Airbnb listings.
#'
#' TKTK
#'
#' @param property An input table with a field named `property_ID` which will be
#' used to generate URLs for scraping.
#' @param timeout TKTK
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


upgo_scrape_ab_registration <- function(property, timeout = 1, quiet = FALSE) {

  ### Initialization ###########################################################

  start_time <- Sys.time()
  i <- NULL

  helper_require("parallel")
  helper_require("doParallel")
  helper_require("RSelenium")

  # Check for upgo_scrape_connect cluster
  if (!exists("cl", envir = .upgo_env)) {
    stop("No Selenium servers running. ",
         "Start servers with `upgo_scrape_connect()`.",
         call. = FALSE)
  }

  # Initialize cluster/future
  `%dopar%` <- foreach::`%dopar%`
  old_do_par <- doParallel::registerDoParallel(.upgo_env$cl)

  # Initialize results table
  results <-
    tibble(property_ID = character(),
           date = Sys.Date(),
           registration = character())


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


  ### Extract HA listings ######################################################

  results_HA <-
    property %>%
    filter(str_starts(.data$property_ID, "ha-")) %>%
    select(.data$property_ID) %>%
    mutate(registration = "HOMEAWAY", date = Sys.Date())

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

  PIDs <-
    property %>%
    filter(!.data$property_ID %in% results$property_ID) %>%
    pull(.data$property_ID) %>%
    str_remove("ab-")

  # Get number of iterations
  chunk_size <- 100
  scrape_rounds <- ceiling(length(PIDs) / chunk_size)

  # Get browsers open
  parallel::clusterEvalQ(.upgo_env$cl, {
    remDr <- rD[["client"]]
    remDr$open()
    remDr$setImplicitWaitTimeout(0)
  })

  # Set up progress bar
  handler_upgo("Scraping listing")
  prog_bar <- as.logical(as.numeric(!quiet) * progressr::handlers(global = NA))
  pb <- progressr::progressor(along = PIDs, enable = prog_bar)


  ## Main loop -----------------------------------------------------------------

  for (i in seq_len(scrape_rounds)) {
    PIDs_to_scrape <- PIDs[(((i - 1) * chunk_size) + 1):(i * chunk_size)]

    results_new <- foreach(j = PIDs_to_scrape) %dopar% {
      out <- tryCatch(helper_scrape_ab_registration(j, timeout = timeout),
                      error = function(e) NULL)
      if (!is.null(out) && out[1,]$registration == "DENIED")
        stop("ACCESS DENIED", call. = FALSE)
      out
    }

    pb(amount = length(PIDs_to_scrape))
    results <- bind_rows(results, results_new)

  }


  ### Clean up and prepare output ##############################################

  # Rbind HA listings into scraped output
  results <- bind_rows(results, results_HA)

  total_time <- Sys.time() - start_time

  if (!quiet) {
    message(crayon::bold(crayon::cyan(glue(
      "Scraping complete. {nrow(results)} listings scraped in ",
      "{substr(total_time, 1, 5)} {attr(total_time, 'units')}."))))
  }

  on.exit()
  return(results)

}

