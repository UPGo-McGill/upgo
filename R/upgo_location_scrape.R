#' Function to scrape location information from Airbnb listings
#'
#' \code{upgo_location_scrape} scrapes location (city, region and country) from
#' Airbnb listings.
#'
#' TKTK
#'
#' @param property An input table with a field named \code{property_ID} which
#' will be used to generate URLs for scraping.
#' @param port The port to use to connect to Docker container. If NULL (the
#' default), an interactive Chrome window will be opened instead.
#' @param delay A numeric scalar. The number of seconds the function should
#' delay in between listings to prevent connection errors or Airbnb shutting
#' down access.
#' @param geography An existing output table which the function should add to.
#' By default the function creates a new table instead.
#' @import RSelenium
#' @importFrom dplyr last
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_extract_all str_replace
#' @importFrom stringr str_split
#' @importFrom purrr map_chr
#' @export


upgo_location_scrape <- function(property, port = NULL, delay = 10,
                                 geography = NULL) {

  ### Initialize table if geography is NULL

  if (missing(geography)) {
    geography <- tibble(property_ID = character(0),
                        city = character(0),
                        region = character(0),
                        country = character(0),
                        raw = character(0))
  }


  ### Start browser
  if (missing(port)) {
    rD <- rsDriver(browser = "chrome", chromever = "77.0.3865.40")
    remDr <- rD$client

  } else {
    remDr <- remoteDriver(browserName = "chrome", port = as.integer(port))
    remDr$open()
  }

  PIDs <- property$property_ID


  ### For loop to retrieve geographies

  for (i in seq_along(PIDs)) {

    ## Determine whether to skip the row

    # Skip row if it has already been scraped
    if (!is.na(geography[i, 5])) {
      message("Listing ", i, " was previously scraped.")
      next
      }

    geography[i, 1] <- PIDs[i]

    geography[i, 2] <- geography[i, 3] <- geography[i, 4] <- geography[i, 5] <-
      NA_character_

    # Skip row if it is HomeAway
    if (str_detect(PIDs[i], "ha-")) {
      geography[i, 5] <- "HOMEAWAY"
      message("Listing ", i, " is a HomeAway listing.")
      next
    }


    ## Initialize connection

    # Extract URL and navigate to page, then wait
    PID <- str_extract(PIDs[i], "(?<=ab-).*")
    remDr$navigate(paste0("https://www.airbnb.ca/rooms/", PID))
    Sys.sleep(delay)

    # If listing redirects to homepage, note this and move on
    if (remDr$getTitle()[[1]] == "Anywhere · Stays · Airbnb") {
      geography[i, 5] <- "NO LISTING"
      message("Listing ", i, " no longer exists.")
      next
    }


    ## Get elements

    # Check to see if the listing is Airbnb plus
    if (str_detect(remDr$getCurrentUrl(), "plus")) {
      location <- remDr$findElements("class", "_ylytgbo")

    # If not, proceed normally
    } else {
      location <- remDr$findElements("class", "_czm8crp")
    }

    elements <-
      map_chr(location, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    tries <- 3

    # Try to refetch data three times before giving up
    while (length(elements) == 0 & tries > 0) {

      Sys.sleep(2)

      location <- remDr$findElements("class", "_czm8crp")

      elements <-
        map_chr(location, ~{
          .x$getElementAttribute("outerHTML")[[1]]
        })

      tries <- tries - 1
    }

    if (length(elements) == 0) {
      message("Connection problem; scraping aborted.")
      remDr$close()
      rD$server$stop()
      return(geography)
    }

    # Extract Airbnb plus locations
    if (str_detect(remDr$getCurrentUrl(), "plus")) {
      elements_extracted <-
        elements %>%
        str_extract('(?=>).+(?=<)') %>%
        str_replace(">", "") %>%
        str_split(", ") %>%
        unlist()

    # Conditional for all non-plus locations
    } else {

      # Filter elements to ones containing key phrase
      elements <-
        elements[str_detect(elements, "’s place is located in")]

      # Aggressive exception catching to avoid the function terminating
      if (length(elements) == 0) {
        message("Connection problem; scraping aborted.")
        remDr$close()
        rD$server$stop()
        return(geography)
      }

      # Try to filter out spurious uses of the "place is located in" phrase
      if(length(elements) > 1 & sum(str_detect(elements, "span")) > 0) {
        elements <-
          elements[str_detect(elements, "_abw475")]
      }

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

        geography[i, 3] <- elements_extracted[1]
        geography[i, 4] <- elements_extracted[2]
        geography[i, 5] <- elements

        message("Listing ", i, " successfully scraped.")
        .temp_scraping_table <<- geography
        next

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
      geography[i, 4] <- elements_extracted

    # With two elements, put them in city and country
    } else if (length(elements_extracted) == 2) {
      geography[i, 2] <- elements_extracted[1]
      geography[i, 4] <- elements_extracted[2]

    # With three elements, put them in city, region and country
    } else if (length(elements_extracted) == 3) {
      geography[i, 2] <- elements_extracted[1]
      geography[i, 3] <- elements_extracted[2]
      geography[i, 4] <- elements_extracted[3]

    # With more than three elements, only take country
    } else if (length(elements_extracted) > 3) {
      geography[i, 4] <- last(elements_extracted)

    # Throw exception if none of the length conditions is satisfied
    } else stop()

    geography[i, 5] <- elements

    .temp_scraping_table <<- geography

    message("Listing ", i, " successfully scraped.")

  }

  ### Close connection and return output

  remDr$close()
  if (missing(port)) rD$server$stop()

  rm(.temp_scraping_table, envir = .GlobalEnv)
  return(geography)

}
