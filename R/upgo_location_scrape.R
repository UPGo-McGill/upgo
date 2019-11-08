#' Function to scrape location information from Airbnb listings
#'
#' \code{upgo_location_scrape} scrapes location (city, region and country) from
#' Airbnb listings.
#'
#' A function for scraping location information from Airbnb listings through
#' RSelenium, using either a headless Chrome browser or a Docker container.
#'
#' @param property An input table with a field named \code{property_ID} which
#' will be used to generate URLs for scraping.
#' @param port The port to use to connect to Docker container. If NULL (the
#' default), an interactive Chrome window will be opened instead.
#' @param docker A logical scalar. Should the scraping be done via a Docker
#' container, or by launching a headless Chrome window (default)?
#' @import RSelenium
#' @importFrom dplyr last
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_extract_all str_replace
#' @importFrom stringr str_split
#' @importFrom purrr map_chr
#' @export


upgo_location_scrape <- function(property, port = 4444L, docker = FALSE) {

  ### Initialize table

  geography <- tibble(
    property_ID = character(0),
    city = character(0),
    region = character(0),
    country = character(0),
    raw = character(0)
  )


  ### Start browser

  if (!docker) {
    eCaps <- list(chromeOptions = list(
      args = c("--headless", "--disable-gpu", "--window-size=1280,800"),
      w3c = FALSE
    ))

    rD <- rsDriver(
      port = as.integer(port), browser = "chrome",
      chromever = "78.0.3904.70", extraCapabilities = eCaps
    )
    remDr <- rD$client
  } else {
    remDr <- remoteDriver(browserName = "chrome", port = as.integer(port))
    remDr$open()
  }

  PIDs <- property$property_ID


  ### For loop to retrieve geographies

  for (i in seq_along(PIDs)) {
    geography[i, 1] <- PIDs[i]

    geography[i, 2] <- geography[i, 3] <- geography[i, 4] <- geography[i, 5] <-
      NA_character_

    ## Skip row if it is HomeAway

    if (str_detect(PIDs[i], "ha-")) {
      geography[i, 5] <- "HOMEAWAY"
      message("Listing ", i, " is a HomeAway listing.")
      next
    }


    ## Initialize connection

    # Extract URL and navigate to page
    PID <- str_extract(PIDs[i], "(?<=ab-).*")
    remDr$navigate(paste0("https://www.airbnb.ca/rooms/", PID))

    # Set waiting time
    remDr$setImplicitWaitTimeout(5000)


    ## Get elements

    location <- remDr$findElements("class", "_czm8crp")

    elements <-
      map_chr(location, ~ {
        .x$getElementAttribute("outerHTML")[[1]]
      })

    # Try to refetch data five times before giving up
    tries <- 5

    while (length(elements) == 0 & tries > 0) {

      # If listing redirects to homepage, break out of while loop
      if (remDr$getTitle()[[1]] == "Anywhere · Stays · Airbnb") {
        break
      }

      Sys.sleep(1)

      location <- remDr$findElements("class", "_czm8crp")

      elements <-
        map_chr(location, ~ {
          .x$getElementAttribute("outerHTML")[[1]]
        })

      tries <- tries - 1
    }


    ## Advance for loop if listing is defunct

    if (remDr$getTitle()[[1]] == "Anywhere · Stays · Airbnb") {
      geography[i, 5] <- "NO LISTING"
      message("Listing ", i, " no longer exists.")
      next
    }

    ## Abort if elements is still empty

    if (length(elements) == 0) {
      message("Connection problem 1; scraping aborted.")
      remDr$close()
      if (!docker) rD$server$stop()
      return(geography)
    }


    ## Check to see if the listing is Airbnb plus

    if (str_detect(remDr$getCurrentUrl(), "plus")) {
      location <- remDr$findElements("class", "_ylytgbo")

      elements <-
        map_chr(location, ~ {
          .x$getElementAttribute("outerHTML")[[1]]
        })

      elements_extracted <-
        elements %>%
        str_extract("(?=>).+(?=<)") %>%
        str_replace(">", "") %>%
        str_split(", ") %>%
        unlist()


      ## Conditional for all non-plus locations
    } else {

      # Filter elements to ones containing key phrase
      elements <-
        elements[str_detect(elements, "’s place is located in")]

      # Aggressive exception catching to avoid the function terminating
      if (length(elements) == 0) {
        location <- remDr$findElements("class", "_czm8crp")

        elements <-
          map_chr(location, ~ {
            .x$getElementAttribute("outerHTML")[[1]]
          })

        elements <-
          elements[str_detect(elements, "’s place is located in")]
      }

      if (length(elements) == 0) {
        message("Connection problem 2; scraping aborted.")
        remDr$close()
        if (!docker) rD$server$stop()
        return(geography)
      }

      # Try to filter out spurious uses of the "place is located in" phrase
      if (length(elements) > 1 & sum(str_detect(elements, "span")) > 0) {
        elements <-
          elements[str_detect(elements, "_abw475")]
      }

      # Logic for cases with standard place name separation
      if (str_detect(elements, "span")) {
        elements_extracted <-
          elements %>%
          str_extract_all(('(?<="><span>).*?(?=</span>)')) %>%
          unlist() %>%
          map_chr(str_replace, ",$", "")

        # Logic for cases with missing places in commas
      } else if (str_detect(elements, "\n, \n\n\n,")) {
        elements_extracted <-
          elements %>%
          str_extract("(?<=, ).*(?=.<)")

        # Logic for cases with one missing place in commas and exit early
      } else if (str_detect(elements, " \n\n,")) {
        elements_extracted <-
          elements %>%
          str_extract("(?<=, ).*(?=.<)") %>%
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
            str_extract("(?<=located in ).*(?=.<)") %>%
            str_split(", ") %>%
            unlist()

          elements_extracted[length(elements_extracted) - 1] <-
            paste(elements_extracted[length(elements_extracted) - 1],
              elements_extracted[length(elements_extracted)],
              sep = ", "
            )

          elements_extracted <- elements_extracted[-length(elements_extracted)]

          # Deal with other cases of a single plain-text string
        } else {
          elements_extracted <-
            elements %>%
            str_extract("(?<=located in ).*(?=.<)") %>%
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
    } else {
      stop()
    }

    geography[i, 5] <- elements

    .temp_scraping_table <<- geography

    message("Listing ", i, " successfully scraped.")
  }

  ### Close connection and return output

  remDr$close()
  if (!docker) rD$server$stop()

  rm(.temp_scraping_table, envir = .GlobalEnv)
  return(geography)
}
