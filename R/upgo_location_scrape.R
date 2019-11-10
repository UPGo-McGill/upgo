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

  geography <- tibble(property_ID = character(0),
                      city = character(0),
                      region = character(0),
                      country = character(0),
                      raw = character(0))


  ### Start browser

  if (!docker) {

    eCaps <- list(chromeOptions = list(
      args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
      w3c = FALSE
    ))

    rD <- rsDriver(port = as.integer(port), browser = "chrome",
                   chromever = "78.0.3904.70", extraCapabilities = eCaps)
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

      geography[i, 5] <- "NO LISTING"

      .temp_scraping_table <<- geography

      message("Listing ", i, " no longer exists.")

      next
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
      message("Connection problem 1; scraping aborted.")
      remDr$close()
      if (!docker) rD$server$stop()
      return(geography)


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

        message("Connection problem 2; scraping aborted.")

        remDr$close()

        if (!docker) rD$server$stop()

        return(geography)
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
    } else stop("Length conditions violated.")

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
