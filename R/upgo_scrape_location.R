#' Function to scrape location information from Airbnb listings
#'
#' \code{upgo_scrape_location} scrapes location (city, region and country) from
#' Airbnb listings.
#'
#' TKTK
#'
#' @param property An input table with a field named \code{property_ID} which
#' will be used to generate URLs for scraping.
#' @param port A positive integer scalar. What port is the Selenium server
#' running on? Set this to match the value passed through
#' [func(upgo_scrape_connect)].
#' @param chunk_size The number of listings to scrape in a single batch.
#' Higher numbers will be slightly faster, but provide less frequent progress
#' feedback and higher risk of lost data if the scraper encounters an error.
#' @param cores A positive integer scalar. How many processing cores should be
#' used to perform the computationally intensive intersection steps? The
#' implementation of multicore processing does not support Windows, so this
#' argument should be left with its default value of 1 in those cases.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @import doParallel
#' @import foreach
#' @import RSelenium
#' @importFrom dplyr bind_rows filter last pull slice
#' @importFrom parallel clusterCall clusterEvalQ makeCluster
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_extract_all str_replace
#' @importFrom stringr str_split
#' @export


upgo_scrape_location <- function(property, port = 4445L, chunk_size = 100,
                                 cores = 1L, quiet = FALSE) {

  start_time <- Sys.time()

  last_try <- FALSE

  (cl <- cores %>% makeCluster) %>% registerDoParallel

  clusterCall(cl, function(port) {
    eCaps <- list(chromeOptions = list(
      args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
      w3c = FALSE
    ))

    remDr <-
      RSelenium::remoteDriver(port = port, browserName = "chrome",
                              extraCapabilities = eCaps)

    remDr$open()
  }, port)


  ### Prepare for main loop

  results <- results_new <-
    tibble::tibble(property_ID = character(0),
                   city = character(0),
                   region = character(0),
                   country = character(0),
                   raw = character(0),
                   date = lubridate::ymd())

  # Set max tries slightly above the minimum needed to process all entries
  max_tries <-
    ceiling((nrow(property) / chunk_size) + sqrt(nrow(property) / chunk_size))

  tries <- 1
  missing_last <- 0
  no_skip <- FALSE


  ### Main loop

  while (nrow(filter(property, !property_ID %in% results$property_ID)) > 0 &
         tries < max_tries) {

    loop_start <- Sys.time()

    tries <- tries + 1

    if (tries > 2 & no_skip) {
      missing_last <- chunk_size - nrow(results_new) + missing_last
    }

    if (missing_last >= nrow(filter(property,
                                    !property_ID %in% results$property_ID))) {
      missing_last <- 0
      no_skip <- TRUE
    }

    PIDs <-
      property %>%
      filter(!property_ID %in% results$property_ID) %>%
      slice((1 + missing_last):(100 + missing_last)) %>%
      pull(property_ID)

    results_new <-
      foreach(i = seq_along(PIDs)) %dopar% {

        if (stringr::str_detect(PIDs[i], "ha-")) {

          scrape_result <- tibble::tibble(property_ID = character(0),
                                          city = character(0),
                                          region = character(0),
                                          country = character(0),
                                          raw = character(0),
                                          date = lubridate::ymd())

          scrape_result[1, 1] <- PIDs[i]
          scrape_result[1, 5] <- "HOMEAWAY"
          scrape_result[1, 6] <- Sys.Date()

          scrape_result

        } else {

          PID <- stringr::str_extract(PIDs[i], "(?<=ab-).*")

          scrape_result <-
            tryCatch(
              upgo_scrape_location_helper(PID),
              error = function(e) {
                tibble(property_ID = character(0),
                       city = character(0),
                       region = character(0),
                       country = character(0),
                       raw = character(0),
                       date = lubridate::ymd())
                })

          scrape_result
          }}

    results_new <-
      results_new %>%
      bind_rows() %>%
      filter(property_ID %in% property$property_ID, !is.na(raw))

    results <-
      results_new %>%
      bind_rows(results)

    .temp_results <<- results

    loop_time <- Sys.time() - loop_start

    remaining <- nrow(filter(property, !property_ID %in% results$property_ID))

    ## Try to catch stalled scraper and restart workers

    if (nrow(results_new) < (chunk_size / 10) & remaining > (chunk_size / 2)) {

      if (last_try) break

      last_try <- TRUE

      if (!quiet) message("Scraping stalled. Restarting processes.")

      clusterEvalQ(cl, {
        remDr$close()
      })

      gc()

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


    }


    if (!quiet & !last_try) {
      message(nrow(results), " listings scraped (current rate ",
            round(nrow(results_new) / as.numeric(loop_time, units = "mins")),
            "/minute). ",
            nrow(filter(property, !property_ID %in% results$property_ID)),
            " listings left.")
    }

  }


  ### Clean up and prepare output

  clusterEvalQ(cl, {
    remDr$close()
  })

  doParallel::stopImplicitCluster()

  total_time <- Sys.time() - start_time

  if (nrow(property %>% filter(!property_ID %in% results$property_ID)) > 0 &
      !quiet) {
    message("Scraping incomplete. ", nrow(results), " listings scraped in ",
            substr(total_time, 1, 5), " ",
            attr(total_time, "units"), ". ",
            nrow(property %>% filter(!property_ID %in% results$property_ID)),
            " listings not scraped.")

  } else if (!quiet) {
    message("Scraping complete. ", nrow(results), " listings scraped in ",
            substr(total_time, 1, 5), " ",
            attr(total_time, "units"), ".")
  }

  rm(.temp_results, envir = .GlobalEnv)

  return(results)

}



#' Helper function to scrape location information from an Airbnb listing
#'
#' \code{upgo_scrape_location_helper} scrapes the location (city, region and
#' country) from a single Airbnb listing.
#'
#' TKTK
#'
#' @param PID An Airbnb property ID to be scraped.
#' @import RSelenium
#' @importFrom dplyr case_when last
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_extract_all str_replace
#' @importFrom stringr str_split
#' @importFrom purrr map_chr

upgo_scrape_location_helper <- function(PID) {

  ## Initialize connection

  scrape_result <- tibble(property_ID = character(0),
                          city = character(0),
                          region = character(0),
                          country = character(0),
                          raw = character(0),
                          date = lubridate::ymd())

  scrape_result[1, 1] <- paste0("ab-", PID)
  scrape_result[1, 6] <- Sys.Date()


  # Extract URL and navigate to page
  remDr$navigate(paste0("https://www.airbnb.ca/rooms/", PID))

  # Set waiting time
  remDr$setImplicitWaitTimeout(5000)


  ## Get elements

  location <- remDr$findElements("class", "_czm8crp")

  elements <-
    map_chr(location, ~{
      .x$getElementAttribute("outerHTML")[[1]]
    })

  # Try to refetch data five times before giving up
  tries <- 5

  while (length(elements) == 0 & tries > 0) {

    # If listing redirects to homepage, break out of while loop
    if (remDr$getTitle()[[1]] == "Anywhere · Stays · Airbnb") {
      break
    }

    # If listing is Luxe, break out of while loop
    if (str_detect(remDr$getCurrentUrl(), "luxury")) {
      break
    }

    Sys.sleep(1)

    location <- remDr$findElements("class", "_czm8crp")

    elements <-
      map_chr(location, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    tries <- tries - 1
  }


  ## Advance for loop if listing is defunct

  if (remDr$getTitle()[[1]] == "Anywhere · Stays · Airbnb") {

    scrape_result[1, 5] <- "NO LISTING"

    # .temp_scraping_table <<- geography

    # message("Listing ", i, " no longer exists.")

    return(scrape_result)
  }

  ### Begin conditional chain for listing processing

  ## First process possible Luxe listing

  if (str_detect(remDr$getCurrentUrl(), "luxury")) {

    location <- remDr$findElements("class", "_4mq26")

    elements <-
      map_chr(location, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    elements <-
      elements[str_detect(elements, ",")]

    elements_extracted <-
      elements %>%
      str_extract('(?=>).+(?=<)') %>%
      str_replace(">", "") %>%
      str_split(", ") %>%
      unlist()

    # Check for US state abbreviations
    if (elements_extracted[2] %in% c(
      "AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
      "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
      "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
      "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
      "WI", "WY")) {
      elements_extracted[3] <- "United States"
      elements_extracted[2] <- case_when(
        elements_extracted[2] == "AL" ~ "Alabama",
        elements_extracted[2] == "AK" ~ "Alaska",
        elements_extracted[2] == "AZ" ~ "Arizona",
        elements_extracted[2] == "AR" ~ "Arkansas",
        elements_extracted[2] == "CA" ~ "California",
        elements_extracted[2] == "CO" ~ "Colorado",
        elements_extracted[2] == "CT" ~ "Connecticut",
        elements_extracted[2] == "DE" ~ "Delaware",
        elements_extracted[2] == "FL" ~ "Florida",
        elements_extracted[2] == "GA" ~ "Georgia",
        elements_extracted[2] == "HI" ~ "Hawaii",
        elements_extracted[2] == "ID" ~ "Idaho",
        elements_extracted[2] == "IL" ~ "Illinois",
        elements_extracted[2] == "IN" ~ "Indiana",
        elements_extracted[2] == "IA" ~ "Iowa",
        elements_extracted[2] == "KS" ~ "Kansas",
        elements_extracted[2] == "KY" ~ "Kentucky",
        elements_extracted[2] == "LA" ~ "Louisiana",
        elements_extracted[2] == "ME" ~ "Maine",
        elements_extracted[2] == "MD" ~ "Maryland",
        elements_extracted[2] == "MA" ~ "Massachusetts",
        elements_extracted[2] == "MI" ~ "Michigan",
        elements_extracted[2] == "MN" ~ "Minnesota",
        elements_extracted[2] == "MS" ~ "Mississippi",
        elements_extracted[2] == "MO" ~ "Missouri",
        elements_extracted[2] == "MT" ~ "Montana",
        elements_extracted[2] == "NE" ~ "Nebraska",
        elements_extracted[2] == "NV" ~ "Nevada",
        elements_extracted[2] == "NH" ~ "New Hampshire",
        elements_extracted[2] == "NJ" ~ "New Jersey",
        elements_extracted[2] == "NM" ~ "New Mexico",
        elements_extracted[2] == "NY" ~ "New York",
        elements_extracted[2] == "NC" ~ "North Carolina",
        elements_extracted[2] == "ND" ~ "North Dakota",
        elements_extracted[2] == "OH" ~ "Ohio",
        elements_extracted[2] == "OK" ~ "Oklahoma",
        elements_extracted[2] == "OR" ~ "Oregon",
        elements_extracted[2] == "PA" ~ "Pennsylvania",
        elements_extracted[2] == "RI" ~ "Rhode Island",
        elements_extracted[2] == "SC" ~ "South Carolina",
        elements_extracted[2] == "SD" ~ "South Dakota",
        elements_extracted[2] == "TN" ~ "Tennessee",
        elements_extracted[2] == "TX" ~ "Texas",
        elements_extracted[2] == "UT" ~ "Utah",
        elements_extracted[2] == "VT" ~ "Vermont",
        elements_extracted[2] == "VA" ~ "Virginia",
        elements_extracted[2] == "WA" ~ "Washington",
        elements_extracted[2] == "WV" ~ "West Virginia",
        elements_extracted[2] == "WI" ~ "Wisconsin",
        elements_extracted[2] == "WY" ~ "Wyoming"
      )
    }

    ## Abort if elements is still empty

  } else if (length(elements) == 0) {

    # message("Connection problem 1; scraping aborted.")
    return(scrape_result)


    ## Check to see if the listing is Airbnb plus

  } else if (str_detect(remDr$getCurrentUrl(), "plus")) {

    location <- remDr$findElements("class", "_ylytgbo")

    elements <-
      map_chr(location, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    elements_extracted <-
      elements %>%
      str_extract('(?=>).+(?=<)') %>%
      str_replace(">", "") %>%
      str_split(", ") %>%
      unlist()


    ## Conditional for all non-Luxe/plus locations

  } else {

    # Filter elements to ones containing key phrase
    elements <-
      elements[str_detect(elements, "’s place is located in")]

    # Try to filter out spurious uses of the "place is located in" phrase
    elements <-
      elements[str_detect(elements, '"ltr"', negate = TRUE)]

    # If there are multiple candidates but only one with "_abw475", pick it
    if (length(elements) > 1 & sum(str_detect(elements, "_abw475")) > 0 &
        sum(str_detect(elements, "_abw475")) < length(elements)) {
      elements <-
        elements[str_detect(elements, "_abw475")]
    }

    # Aggressive exception catching to avoid the function terminating
    if (length(elements) == 0) {

      location <- remDr$findElements("class", "_czm8crp")

      elements <-
        map_chr(location, ~{
          .x$getElementAttribute("outerHTML")[[1]]
        })

      elements <-
        elements[str_detect(elements, "’s place is located in")]

      elements <-
        elements[str_detect(elements, '"ltr"', negate = TRUE)]

      if (length(elements) > 1 & sum(str_detect(elements, "_abw475")) > 0 &
          sum(str_detect(elements, "_abw475")) < length(elements)) {
        elements <-
          elements[str_detect(elements, "_abw475")]
      }
    }

    if (length(elements) == 0) {

      location <- remDr$findElements("class", "_czm8crp")

      elements <-
        map_chr(location, ~{
          .x$getElementAttribute("outerHTML")[[1]]
        })

      elements <-
        elements[str_detect(elements, "’s place is located in")]

      elements <-
        elements[str_detect(elements, '"ltr"', negate = TRUE)]

      if (length(elements) > 1 & sum(str_detect(elements, "_abw475")) > 0 &
          sum(str_detect(elements, "_abw475")) < length(elements)) {
        elements <-
          elements[str_detect(elements, "_abw475")]
      }
    }

    if (length(elements) == 0) {

      # message("Connection problem 2; scraping aborted.")

      return(scrape_result)
    }

    # Take first element if there are still multiple ones
    if(length(elements) > 1) elements <- elements[1]

    # Logic for cases with standard place name separation
    if(str_detect(elements, "span")) {
      elements_extracted <-
        elements %>%
        str_extract_all(('(?<="><span>).*?(?=</span>)')) %>%
        unlist() %>%
        map_chr(str_replace, ",$", "")

      # Logic for cases with missing places in commas
    } else if (str_detect(elements, "\n, \n\n\n,")) {
      elements_extracted <-
        elements %>%
        str_extract('(?<=, ).*(?=.<)')

      # Logic for cases with one missing place in commas and exit early
    } else if (str_detect(elements, " \n\n,")) {
      elements_extracted <-
        elements %>%
        str_extract('(?<=, ).*(?=.<)') %>%
        str_split(", ") %>%
        unlist()

      scrape_result[1, 3] <- elements_extracted[1]
      scrape_result[1, 4] <- elements_extracted[2]
      scrape_result[1, 5] <- elements

      # message("Listing ", i, " successfully scraped.")
      # .temp_scraping_table <<- geography
      return(scrape_result)

      # Logic for cases with a single plain-text string
    } else {

      # Deal with Bonaire, Sint Eustatius and Saba
      if (str_detect(elements, "Bonaire")) {
        elements_extracted <-
          elements %>%
          str_extract('(?<=located in ).*(?=.<)') %>%
          str_split(", ") %>%
          unlist()

        elements_extracted[length(elements_extracted) - 1] <-
          paste(elements_extracted[length(elements_extracted) - 1],
                elements_extracted[length(elements_extracted)], sep = ", ")

        elements_extracted <- elements_extracted[-length(elements_extracted)]

        # Deal with other cases of a single plain-text string
      } else {
        elements_extracted <-
          elements %>%
          str_extract('(?<=located in ).*(?=.<)') %>%
          str_split(", ") %>%
          unlist()
      }
    }
  }


  ## Allocate output to geography table

  # With a single element, put it in the country field
  if (length(elements_extracted) == 1) {
    scrape_result[1, 4] <- elements_extracted

    # With two elements, put them in city and country
  } else if (length(elements_extracted) == 2) {
    scrape_result[1, 2] <- elements_extracted[1]
    scrape_result[1, 4] <- elements_extracted[2]

    # With three elements, put them in city, region and country
  } else if (length(elements_extracted) == 3) {
    scrape_result[1, 2] <- elements_extracted[1]
    scrape_result[1, 3] <- elements_extracted[2]
    scrape_result[1, 4] <- elements_extracted[3]

    # With more than three elements, only take country
  } else if (length(elements_extracted) > 3) {
    scrape_result[1, 4] <- last(elements_extracted)

    # Throw exception if none of the length conditions is satisfied
  } else stop("Length conditions violated.")

  scrape_result[1, 5] <- elements

  # .temp_scraping_table <<- geography

  # message("Listing ", i, " successfully scraped.")

  return(scrape_result)
}





