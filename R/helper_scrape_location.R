#' Helper function to scrape location information from an Airbnb listing
#'
#' \code{helper_scrape_location} scrapes the location (city, region and
#' country) from a single Airbnb listing and forwards to the calling function
#' for further processing.
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

helper_scrape_location <- function(PID) {

  ### Initialize objects #######################################################

  scrape_result <- tibble(property_ID = character(), raw = list())

  scrape_result[1, 1] <- paste0("ab-", PID)

  elements <- list()


  ### Navigate to listing ######################################################

  remDr$navigate(paste0("https://www.airbnb.ca/rooms/", PID))

  remDr$setImplicitWaitTimeout(5000)


  ### Test title for missing listing and exit early if so ######################

  if (remDr$getTitle()[[1]] == "Anywhere \u00b7 Stays \u00b7 Airbnb") {

    scrape_result[1, 2] <- list("NO LISTING")

    return(scrape_result)
  }


  ### Try regular listing type #################################################

  elements <- remDr$findElements("class", "_abw475")


  ### Test title for missing listing again #####################################

  if (remDr$getTitle()[[1]] == "Anywhere \u00b7 Stays \u00b7 Airbnb") {

    scrape_result[1, 2] <- list("NO LISTING")

    return(scrape_result)
  }


  ### If regular doesn't work, try _czm8crp then regular again #################

  if (!length(elements) %in% 1:3) {
    elements <- remDr$findElements("class", "_czm8crp")

    elements2 <- remDr$findElements("class", "_abw475")

    if (length(elements2 %in% 1:3)) elements <- elements2
  }


  ### Deal with Luxe and Plus listings #########################################

  if (str_detect(remDr$getCurrentUrl(), "luxury")) {
    elements <- remDr$findElements("class", "_4mq26")
  }

  if (str_detect(remDr$getCurrentUrl(), "plus")) {
    elements <- remDr$findElements("class", "_ylytgbo")
  }


  ### Convert elements to vector ###############################################

  if (length(elements) > 0) {
    elements <-
      map_chr(elements, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })
  }


  ### Allocate output and return ###############################################

  if (length(elements) > 0) {
    scrape_result[1,2] <- list(list(elements))
  }

  return(scrape_result)

}





#' Helper function to scrape location information from an Airbnb listing
#'
#' \code{helper_scrape_location_parse} scrapes the location (city, region and
#' country) from a single Airbnb listing and forwards to the calling function
#' for further processing.
#'
#' TKTK
#'
#' @param results The interim results helper_scrape_location.
#' @import RSelenium
#' @importFrom dplyr case_when last select
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_replace str_split
#' @importFrom purrr map map_chr map_int map_lgl

helper_scrape_location_parse <- function(results) {

  ### Divide results into separate tables for processing #######################

  missing <-
    results %>%
    filter(lengths(.data$raw) == 1) %>%
    filter(map_lgl(.data$raw, ~.x == "NO LISTING")) %>%
    mutate(city = NA_character_,
           region = NA_character_,
           country = NA_character_,
           date = Sys.Date()) %>%
    select(.data$property_ID, .data$city:.data$country, .data$raw, .data$date)

  luxe <-
    results %>%
    filter(map_int(.data$raw, ~sum(str_detect(.x, "_4mq26"))) > 0)

  plus <-
    results %>%
    filter(map_int(.data$raw, ~sum(str_detect(.x, "_ylytgbo"))) > 0)

  results <-
    results %>%
    filter(!.data$property_ID %in% missing$property_ID,
           !.data$property_ID %in% luxe$property_ID,
           !.data$property_ID %in% plus$property_ID)

  three_length <-
    results %>%
    filter(lengths(.data$raw) == 3)

  two_length <-
    results %>%
    filter(lengths(.data$raw) == 2)

  one_length <-
    results %>%
    filter(lengths(.data$raw) == 1)

  high_length <-
    results %>%
    filter(lengths(.data$raw) > 3)


  ### Process standard results #################################################

  three_length <-
    three_length %>%
    mutate(
      city = map_chr(.data$raw, ~{
        .x[1] %>%
          str_extract("(?<=><span>).*(?=</span>)") %>%
          str_replace(",", "")
        }),
      region = map_chr(.data$raw, ~{
        .x[2] %>%
          str_extract("(?<=><span>).*(?=</span>)") %>%
          str_replace(",", "")
      }),
      country = map_chr(.data$raw, ~{
        .x[3] %>%
          str_extract("(?<=><span>).*(?=</span>)") %>%
          str_replace(",", "")
      }),
      date = Sys.Date()
      ) %>%
    select(.data$property_ID, .data$city:.data$country, .data$raw, .data$date)

  two_length <-
    two_length %>%
    mutate(
      city = map_chr(.data$raw, ~{
        .x[1] %>%
          str_extract("(?<=><span>).*(?=</span>)") %>%
          str_replace(",", "")
      }),
      region = NA_character_,
      country = map_chr(.data$raw, ~{
        .x[2] %>%
          str_extract("(?<=><span>).*(?=</span>)") %>%
          str_replace(",", "")
      }),
      date = Sys.Date()
    ) %>%
    select(.data$property_ID, .data$city:.data$country, .data$raw, .data$date)

  one_length <-
    one_length %>%
    mutate(city = "ONE NEED TO CHECK",
           region = "ONE NEED TO CHECK",
           country = "ONE NEED TO CHECK",
           date = Sys.Date()) %>%
    select(.data$property_ID, .data$city:.data$country, .data$raw, .data$date)


  high_length <-
    high_length %>%
    mutate(
      location = map(.data$raw, ~{
        .x[str_detect(.x, "place is located in")] %>%
          str_extract("(?<=located in ).*(?=.</div)") %>%
          str_split(", ") %>%
          unlist()
      }),
      city = map_chr(.data$location, ~{
        if (length(.x) >= 2) .x[1] else NA_character_
        }),
      region = map_chr(.data$location, ~{
        if (length(.x) == 3) .x[2] else NA_character_
        }),
      country = map_chr(.data$location, ~{
        if (length(.x) >= 1) last(.x) else "PARSE ERROR TO CHECK"
        }),
      date = Sys.Date()) %>%
    select(.data$property_ID, .data$city:.data$country, .data$raw, .data$date)

  ### Process Luxe and Plus results ############################################

  US_states <-
    c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
      "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
      "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
      "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
      "WI", "WY")

  luxe <-
    luxe %>%
    mutate(
      location = map(.data$raw, ~{
        .x[str_detect(.x, ",")] %>%
          str_extract('(?=>).+(?=<)') %>%
          str_replace(">", "") %>%
          str_split(", ") %>%
          unlist()
      }),
      city = map_chr(.data$location, ~.x[1]),
      region = map_chr(.data$location, ~{
        if (.x[2] %in% US_states) {
          case_when(
            .x[2] == "AL" ~ "Alabama",  .x[2] == "AK" ~ "Alaska",
            .x[2] == "AZ" ~ "Arizona", .x[2] == "AR" ~ "Arkansas",
            .x[2] == "CA" ~ "California", .x[2] == "CO" ~ "Colorado",
            .x[2] == "CT" ~ "Connecticut", .x[2] == "DE" ~ "Delaware",
            .x[2] == "FL" ~ "Florida", .x[2] == "GA" ~ "Georgia",
            .x[2] == "HI" ~ "Hawaii", .x[2] == "ID" ~ "Idaho",
            .x[2] == "IL" ~ "Illinois", .x[2] == "IN" ~ "Indiana",
            .x[2] == "IA" ~ "Iowa", .x[2] == "KS" ~ "Kansas",
            .x[2] == "KY" ~ "Kentucky", .x[2] == "LA" ~ "Louisiana",
            .x[2] == "ME" ~ "Maine", .x[2] == "MD" ~ "Maryland",
            .x[2] == "MA" ~ "Massachusetts", .x[2] == "MI" ~ "Michigan",
            .x[2] == "MN" ~ "Minnesota", .x[2] == "MS" ~ "Mississippi",
            .x[2] == "MO" ~ "Missouri", .x[2] == "MT" ~ "Montana",
            .x[2] == "NE" ~ "Nebraska", .x[2] == "NV" ~ "Nevada",
            .x[2] == "NH" ~ "New Hampshire", .x[2] == "NJ" ~ "New Jersey",
            .x[2] == "NM" ~ "New Mexico", .x[2] == "NY" ~ "New York",
            .x[2] == "NC" ~ "North Carolina", .x[2] == "ND" ~ "North Dakota",
            .x[2] == "OH" ~ "Ohio", .x[2] == "OK" ~ "Oklahoma",
            .x[2] == "OR" ~ "Oregon", .x[2] == "PA" ~ "Pennsylvania",
            .x[2] == "RI" ~ "Rhode Island", .x[2] == "SC" ~ "South Carolina",
            .x[2] == "SD" ~ "South Dakota", .x[2] == "TN" ~ "Tennessee",
            .x[2] == "TX" ~ "Texas", .x[2] == "UT" ~ "Utah",
            .x[2] == "VT" ~ "Vermont", .x[2] == "VA" ~ "Virginia",
            .x[2] == "WA" ~ "Washington", .x[2] == "WV" ~ "West Virginia",
            .x[2] == "WI" ~ "Wisconsin", .x[2] == "WY" ~ "Wyoming"
          )
        } else NA_character_
      }),
      country = map_chr(.data$location, ~{
        if (.x[2] %in% US_states) "United States" else .x[2]
      }),
      date = Sys.Date()) %>%
    select(.data$property_ID, .data$city:.data$country, .data$raw, .data$date)

  plus <-
    plus %>%
    mutate(location = map(.data$raw, ~{
      .x %>%
        str_extract('(?=>).+(?=<)') %>%
        str_replace(">", "") %>%
        str_split(", ") %>%
        unlist()
    })) %>%
    mutate(city = map_chr(.data$location, ~.x[1]),
           region = map_chr(.data$location, ~{
             if (length(.x) == 2) NA_character_ else .x[2]
             }),
           country = map_chr(.data$location, ~{
             if (length(.x) == 2) .x[2] else .x[3]
             }),
           date = Sys.Date()) %>%
    select(.data$property_ID, .data$city:.data$country, .data$raw, .data$date)


  ### Reassemble output

  results <-
    bind_rows(three_length, two_length, one_length, high_length, luxe, plus,
              missing)

  return(results)
}
