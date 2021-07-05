#### AB SCRAPE HELPERS #########################################################

#' Helper function to scrape location information from an Airbnb listing
#'
#' \code{helper_scrape_ab} scrapes the location (city, region and
#' country) from a single Airbnb listing and forwards to the calling function
#' for further processing.
#'
#' @param PID An Airbnb property ID to be scraped.
#' @importFrom dplyr case_when last tibble
#' @importFrom stringr str_detect str_extract str_extract_all str_replace
#' @importFrom stringr str_split

helper_scrape_ab <- function(PID) {

  ### Initialize objects #######################################################

  scrape_result <-
    tibble(property_ID = character(), raw = list(), note = character())
  scrape_result[1, 1] <- paste0("ab-", PID)


  ### Navigate to listing ######################################################

  remDr$navigate(paste0("https://www.airbnb.ca/rooms/", PID))

  # Temporary workaround until proper loading trigger is found
  Sys.sleep(0.5)


  ### Verify listing is loaded and not missing #################################

  # load_check <- FALSE
  # iters <- 0
  #
  # load_check <- remDr$findElement(using = "xpath", value = '/html')
  # test_1 <- load_check$getElementAttribute('class')
  # elem <- remDr$findElement(using = "xpath", value = '//*[@id="data-state"]')
  # test_2 <- elem$getElementAttribute('data-state')
  #
  # while (!load_check && iters <= 5) {
  #
  #   iters <- iters + 1
  #
  #   # One of these elements is only true once the page is loaded
  #   load_check <- remDr$findElement(using = "xpath", value = '/html')
  #   load_check_1 <- load_check$getElementAttribute("data-triggered")
  #   load_check_2 <- load_check$getElementAttribute("data-is-hyperloop")
  #   load_check <- c(load_check_1, load_check_2)
  #
  #   if (length(load_check) == 0 || load_check[[1]] != "true") {

      # Exit early if listing is missing
      if (remDr$getCurrentUrl()[[1]] == "https://www.airbnb.ca/s/homes") {
        scrape_result[1,]$raw <- list("NO LISTING")
        scrape_result[1,]$note <- "no_listing"
        return(scrape_result)
      }

  #     # If the key element isn't loaded, wait 0.5 seconds then try again
  #     Sys.sleep(0.5)
  #     load_check <- FALSE
  #   } else load_check <- TRUE
  #
  # }


  ### Return NULL if the listing hasn't loaded in 5 seconds ####################

  # if (!load_check) return(NULL)


  ### Try regular listing type #################################################

  # One of these classes is sometimes present at the top of listings
  element_top <- remDr$findElements("class", "_1iissmv4")

  if (length(element_top) > 0) {

    element_top <-
      purrr::map_chr(element_top, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    element_top_processed <-
      element_top %>%
      stringr::str_replace('<span class=\"_1iissmv4\">', "") %>%
      stringr::str_replace("</span>", "") %>%
      stringr::str_split(", ") %>%
      unlist()

    if (length(element_top_processed) > 0) {
      scrape_result[1,]$raw <- list(element_top)
      scrape_result[1,]$note <- "top_1"

      return(scrape_result)
    }
  }

  # element_top <- remDr$findElements("class", "_5twioja")
  #
  # if (length(element_top) > 0) {
  #
  #   element_top <-
  #     purrr::map_chr(element_top, ~{
  #       .x$getElementAttribute("outerHTML")[[1]]
  #     })
  #
  #   element_top_processed <-
  #     element_top %>%
  #     stringr::str_extract('(?<=">).*(?=</a>)') %>%
  #     stringr::str_replace("<span>", "") %>%
  #     stringr::str_replace("</span>", "") %>%
  #     stringr::str_split(", ") %>%
  #     unlist()
  #
  #   if (length(element_top_processed) > 0) {
  #     scrape_result[1,]$raw <- list(element_top)
  #     scrape_result[1,]$note <- "top_2"
  #
  #     return(scrape_result)
  #   }
  # }
  #
  #
  ### Deal with Luxe and Plus listings #########################################

  if (stringr::str_detect(remDr$getCurrentUrl(), "luxury")) {
    element_bottom <-
      remDr$findElements("class", "_4mq26") %>%
      purrr::map_chr(~.x$getElementAttribute("outerHTML")[[1]])

    scrape_result[1,]$raw <- list(element_bottom)
    scrape_result[1,]$note <- "luxe"

    return(scrape_result)

  }

  if (str_detect(remDr$getCurrentUrl(), "plus")) {
    element_bottom <-
      remDr$findElements("class", "_ylytgbo") %>%
      purrr::map_chr(~.x$getElementAttribute("outerHTML")[[1]])

    scrape_result[1,]$raw <- list(element_bottom)
    scrape_result[1,]$note <- "plus"

    return(scrape_result)

  }
  #
  #
  # ### Try _abw475 ##############################################################
  #
  # element_bottom <- remDr$findElements("class", "_abw475")
  #
  # if (length(element_bottom) %in% 1:3) {
  #
  #   element_bottom <-
  #     purrr::map_chr(element_bottom, ~{
  #       .x$getElementAttribute("outerHTML")[[1]]
  #     })
  #
  #   scrape_result[1,]$raw <- list(element_bottom)
  #   scrape_result[1,]$note <- "_abw475"
  #
  #   return(scrape_result)
  #
  # }
  #
  #
  # ### Try _s1tlw0m #############################################################
  #
  # element_bottom <- remDr$findElements("class", "_s1tlw0m")
  #
  # if (length(element_bottom) > 0) {
  #
  #   element_bottom <-
  #     purrr::map_chr(element_bottom, ~{
  #       .x$getElementAttribute("outerHTML")[[1]]
  #     })
  #
  #   scrape_result[1,]$raw <- list(element_bottom)
  #   scrape_result[1,]$note <- "_s1tlw0m"
  #
  #   return(scrape_result)
  #
  # }
  #
  #
  # ### Try _czm8crp #############################################################
  #
  # element_bottom <- remDr$findElements("class", "_czm8crp")
  #
  # if (length(element_bottom) < 0) {
  #
  #   element_bottom <-
  #     purrr::map_chr(element_bottom, ~{
  #       .x$getElementAttribute("outerHTML")[[1]]
  #     })
  #
  #   scrape_result[1,]$raw <- list(element_bottom)
  #   scrape_result[1,]$note <- "_czm8crp"
  #
  #
  # }

  ### Try element_top one more time ############################################

  element_top <- remDr$findElements("class", "_1iissmv4")

  if (length(element_top) > 0) {

    element_top <-
      purrr::map_chr(element_top, ~{
        .x$getElementAttribute("outerHTML")[[1]]
      })

    element_top_processed <-
      element_top %>%
      stringr::str_replace('<span class=\"_1iissmv4\">', "") %>%
      stringr::str_replace("</span>", "") %>%
      stringr::str_split(", ") %>%
      unlist()

    if (length(element_top_processed) > 0) {
      scrape_result[1,]$raw <- list(element_top)
      scrape_result[1,]$note <- "top_1"

      return(scrape_result)
    }
  }


  ### If nothing else works, return NULL #######################################

  return(NULL)

}


#' Helper function to parse location information from an Airbnb listing
#'
#' \code{helper_parse_ab} parses the location (city, region and from a single
#' scraped Airbnb listing.
#'
#' @param scrape_result A one-row data frame produced as an interim result from
#' \code{helper_scrape_ab}.
#' @importFrom dplyr bind_rows case_when filter last mutate select tibble
#' @importFrom stringr str_detect str_extract str_replace str_split
#' @importFrom purrr map map_chr map_int map_lgl

helper_parse_ab <- function(scrape_result) {

  ### Exit early on NULL #######################################################

  if (is.null(scrape_result)) return(NULL)


  ### Process missing ##########################################################

  if (scrape_result$note == "no_listing") {

    return(
      scrape_result %>%
        mutate(city = NA_character_,
               region = NA_character_,
               country = NA_character_,
               date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process Luxe #############################################################

  if (scrape_result$note == "luxe") {

    return(

      scrape_result %>%
        mutate(
          location = purrr::map(.data$raw, ~{
            .x[stringr::str_detect(.x, ",")] %>%
              stringr::str_extract('(?=>).+(?=<)') %>%
              stringr::str_replace(">", "") %>%
              stringr::str_split(", ") %>%
              unlist()
          }),
          city = purrr::map_chr(.data$location, ~{
            if (is.null(.x)) NA_character_ else .x[1]}),
          region = purrr::map_chr(.data$location, ~{
            if (is.null(.x)) NA_character_ else if (.x[2] %in% US_states) {
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
                .x[2] == "NC" ~ "North Carolina", .x[2] == "ND" ~
                  "North Dakota",
                .x[2] == "OH" ~ "Ohio", .x[2] == "OK" ~ "Oklahoma",
                .x[2] == "OR" ~ "Oregon", .x[2] == "PA" ~ "Pennsylvania",
                .x[2] == "RI" ~ "Rhode Island", .x[2] == "SC" ~
                  "South Carolina",
                .x[2] == "SD" ~ "South Dakota", .x[2] == "TN" ~ "Tennessee",
                .x[2] == "TX" ~ "Texas", .x[2] == "UT" ~ "Utah",
                .x[2] == "VT" ~ "Vermont", .x[2] == "VA" ~ "Virginia",
                .x[2] == "WA" ~ "Washington", .x[2] == "WV" ~ "West Virginia",
                .x[2] == "WI" ~ "Wisconsin", .x[2] == "WY" ~ "Wyoming"
              )
            } else NA_character_
          }),
          country = purrr::map_chr(.data$location, ~{
            if (is.null(.x)) NA_character_ else
              if (.x[2] %in% US_states) "United States" else .x[2]
          }),
          date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process Plus #############################################################

  if (scrape_result$note == "plus") {

    return(
      scrape_result %>%
        mutate(location = purrr::map(.data$raw, ~{
          .x %>%
            stringr::str_extract('(?=>).+(?=<)') %>%
            stringr::str_replace(">", "") %>%
            stringr::str_split(", ") %>%
            unlist()
        })) %>%
        mutate(city = purrr::map_chr(.data$location, ~{
          if (is.null(.x)) NA_character_ else .x[1]}),
               region = purrr::map_chr(.data$location, ~{
                 if (is.null(.x)) NA_character_ else
                   if (length(.x) == 2) NA_character_ else .x[2]
               }),
               country = purrr::map_chr(.data$location, ~{
                 if (is.null(.x)) NA_character_ else
                   if (length(.x) == 2) .x[2] else .x[3]
               }),
               date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process element_top results ##############################################

  if (scrape_result$note == "top_1") {

    content <-
      scrape_result$raw[[1]] |>
      str_subset("_1iissmv4") |>
      stringr::str_replace('^.*<span class=\"_1iissmv4\">', "") |>
      stringr::str_replace("</span>.*$", "") |>
      stringr::str_split(", ") |>
      unlist()

    # Deal with Bonaire comma
    if (content[[length(content)]] == "Sint Eustatius and Saba") {
      content[[length(content) - 1]] <- "Bonaire, Sint Eustatius and Saba"
      content <- content[1:(length(content) - 1)]
    }

    if (length(content) == 1) {

      if (content %in% country_list) {
        return(
          scrape_result %>%
            mutate(
              city = NA_character_,
              region = NA_character_,
              country = content,
              date = Sys.Date()
            ) %>%
            select(.data$property_ID, .data$city:.data$country, .data$raw,
                   .data$date)
        )
      } else return(NULL)

    }

    return(
      scrape_result %>%
        mutate(
          city = content[[1]],
          region = if_else(length(content) == 3, content[[2]], NA_character_),
          country = content[[length(content)]],
          date = Sys.Date()
        ) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }

#
#   if (scrape_result$note == "top_2") {
#
#     content <-
#       scrape_result$raw[[1]] %>%
#       stringr::str_extract('(?<=">).*(?=</a>)') %>%
#       stringr::str_replace("<span>", "") %>%
#       stringr::str_replace("</span>", "") %>%
#       stringr::str_split(", ") %>%
#       unlist()
#
#     # Deal with Bonaire comma
#     if (content[[length(content)]] == "Sint Eustatius and Saba") {
#       content[[length(content) - 1]] <- "Bonaire, Sint Eustatius and Saba"
#       content <- content[1:(length(content) - 1)]
#     }
#
#     if (length(content) == 1) {
#
#       if (content %in% country_list) {
#         return(
#           scrape_result %>%
#             mutate(
#               city = NA_character_,
#               region = NA_character_,
#               country = content,
#               date = Sys.Date()
#             ) %>%
#             select(.data$property_ID, .data$city:.data$country, .data$raw,
#                    .data$date)
#         )
#       } else return(NULL)
#
#     }
#
#     return(
#       scrape_result %>%
#         mutate(
#           city = content[[1]],
#           region = if_else(length(content) == 3, content[[2]], NA_character_),
#           country = content[[length(content)]],
#           date = Sys.Date()
#         ) %>%
#         select(.data$property_ID, .data$city:.data$country, .data$raw,
#                .data$date)
#     )
#   }
#
#
#   ### Process _s1tlw0m results #################################################
#
#   if (scrape_result$note == "_s1tlw0m") {
#
#     content <-
#       # The valid element seems to be the only one with a comma
#       scrape_result$raw[[1]][stringr::str_detect(scrape_result$raw[[1]],
#                                                  ",")] %>%
#       stringr::str_extract('(?<=_s1tlw0m..).*(?=<)') %>%
#       stringr::str_split(", ") %>%
#       unlist()
#
#     # Deal with Bonaire comma
#     if (content[[length(content)]] == "Sint Eustatius and Saba") {
#       content[[length(content) - 1]] <- "Bonaire, Sint Eustatius and Saba"
#       content <- content[1:(length(content) - 1)]
#     }
#
#     return(
#       scrape_result %>%
#         mutate(
#           city = content[[1]],
#           region = if_else(length(content) == 3, content[[2]], NA_character_),
#           country = content[[length(content)]],
#           date = Sys.Date()
#         ) %>%
#         select(.data$property_ID, .data$city:.data$country, .data$raw,
#                .data$date)
#     )
#   }
#
#
#   ### Process _czm8crp results #################################################
#
#   if (scrape_result$note == "_czm8crp") {
#
#     return(
#       scrape_result %>%
#         mutate(
#           location = purrr::map(.data$raw, ~{
#             .x[stringr::str_detect(.x, "place is located in")] %>%
#               stringr::str_extract("(?<=located in ).*(?=.</div)") %>%
#               stringr::str_split(", ") %>%
#               unlist()
#           }),
#           city = purrr::map_chr(.data$location, ~{
#             if (length(.x) >= 2) .x[1] else NA_character_
#           }),
#           region = purrr::map_chr(.data$location, ~{
#             if (length(.x) == 3) .x[2] else NA_character_
#           }),
#           country = purrr::map_chr(.data$location, ~{
#             if (length(.x) >= 1) last(.x) else "PARSE ERROR TO CHECK"
#           }),
#           date = Sys.Date()) %>%
#         select(.data$property_ID, .data$city:.data$country, .data$raw,
#                .data$date)
#     )
#   }
#
  ### Process 3-length results #################################################

  if (length(scrape_result$raw[[1]]) == 3) {

    return(
      scrape_result %>%
        mutate(
          city = purrr::map_chr(.data$raw, ~{
            .x[1] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          region = purrr::map_chr(.data$raw, ~{
            .x[2] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          country = purrr::map_chr(.data$raw, ~{
            .x[3] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          date = Sys.Date()
        ) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process 2-length results #################################################

  if (length(scrape_result$raw[[1]]) == 2) {

    return(
      scrape_result %>%
        mutate(
          city = purrr::map_chr(.data$raw, ~{
            .x[1] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          region = NA_character_,
          country = purrr::map_chr(.data$raw, ~{
            .x[2] %>%
              stringr::str_extract("(?<=><span>).*(?=</span>)") %>%
              stringr::str_replace(",", "")
          }),
          date = Sys.Date()
        ) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process long-length results ##############################################

  if (length(scrape_result$raw[[1]]) > 3) {

    return(
      scrape_result %>%
        mutate(
          location = purrr::map(.data$raw, ~{
            .x[stringr::str_detect(.x, "place is located in")] %>%
              stringr::str_extract("(?<=located in ).*(?=.</div)") %>%
              stringr::str_split(", ") %>%
              unlist()
          }),
          city = purrr::map_chr(.data$location, ~{
            if (length(.x) >= 2) .x[1] else NA_character_
          }),
          region = purrr::map_chr(.data$location, ~{
            if (length(.x) == 3) .x[2] else NA_character_
          }),
          country = purrr::map_chr(.data$location, ~{
            if (length(.x) >= 1) last(.x) else "PARSE ERROR TO CHECK"
          }),
          date = Sys.Date()) %>%
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }
}


#' Helper function to scrape registration information from an Airbnb listing
#'
#' \code{helper_scrape_ab_registration} scrapes the registration number from a
#' single Airbnb listing and forwards to the calling function for further
#' processing.
#'
#' @param PID An Airbnb property ID to be scraped.
#' @param timeout TKTK

helper_scrape_ab_registration <- function(PID, timeout) {

  ### Initialize objects #######################################################

  scrape_result <-
    dplyr::tibble(property_ID = paste0("ab-", PID),
                  date = Sys.Date(),
                  registration = NA_character_)


  ### Navigate to listing and verify it is loaded ##############################

  remDr$setImplicitWaitTimeout(0)
  remDr$navigate(paste0("https://www.airbnb.ca/rooms/", PID,
                        "?modal=DESCRIPTION"))

  # Temporary workaround until proper loading trigger is found
  Sys.sleep(timeout)

  # Exit early if listing is missing
  if (remDr$getCurrentUrl()[[1]] == "https://www.airbnb.ca/s/homes") {
    scrape_result[1,]$registration <- "NO LISTING"
    return(scrape_result)
  }

  # Check for "Access Denied"
  denied <- tryCatch({
    denied <- suppressMessages(remDr$findElement(
      using = "xpath", value = '/html/body/h1'))
    denied <- denied$getElementText()[[1]]
    }, error = function(e) "OK")

  if (denied == "Access Denied") {
    scrape_result[1,]$registration <- "DENIED"
    return(scrape_result)
  }



  ### Get field ################################################################

  reg <- remDr$findElements(using = "class", value = "_15pb00k")
  reg <- map_chr(reg, ~.x$getElementText()[[1]])
  reg <- stringr::str_extract(reg, "(?<=Licence number\\n).*")
  reg <- reg[!is.na(reg)]
  if (length(reg) == 0) reg <- NA_character_
  scrape_result[1,]$registration <- reg

  return(scrape_result)

}
