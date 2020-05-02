#' Helper function to scrape location information from an Airbnb listing
#'
#' \code{helper_scrape_location_parse} scrapes the location (city, region and
#' country) from a single Airbnb listing and forwards to the calling function
#' for further processing.
#'
#' @param scrape_result A one-row data frame produced as an interim result from
#' \code{helper_scrape_location}.
#' @importFrom dplyr bind_rows case_when filter last mutate select
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_replace str_split
#' @importFrom purrr map map_chr map_int map_lgl

helper_scrape_location_parse_2 <- function(scrape_result) {

  ### Exit early on NULL #######################################################

  if (is.null(scrape_result)) return(NULL)


  ### Country list for checking ################################################

  country_list <-
    c("Afghanistan", "\u00c5land Islands", "Albania", "Algeria",
      "American Samoa", "Andorra", "Angola", "Anguilla", "Antigua and Barbuda",
      "Argentina", "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan",
      "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium",
      "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia",
      "Bonaire, Sint Eustatius and Saba", "Bosnia and Herzegovina", "Botswana",
      "Brazil", "British Virgin Islands", "Brunei", "Bulgaria", "Burkina Faso",
      "Burundi", "Cambodia", "Cameroon", "Canada", "Cape Verde",
      "Cayman Islands", "Central African Republic", "Chad", "Chile", "China",
      "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros",
      "Congo", "Cook Islands", "Costa Rica", "Croatia", "Cuba", "Cura\u00e7ao",
      "Cyprus", "Czech Republic", "Democratic Republic of the Congo", "Denmark",
      "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecuador",
      "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia",
      "Ethiopia", "Falkland Islands (Malvinas)", "Faroe Islands", "Fiji",
      "Finland", "France", "French Guiana", "French Polynesia", "Gabon",
      "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece",
      "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", "Guernsey",
      "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras", "Hong Kong",
      "Hungary", "Iceland", "India", "Indonesia", "Iraq", "Ireland",
      "Isle of Man", "Israel", "Italy", "Ivory Coast", "Jamaica", "Japan",
      "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kosovo", "Kuwait",
      "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya",
      "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Macedonia",
      "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta",
      "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte",
      "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro",
      "Montserrat", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru",
      "Nepal", "Netherlands", "New Caledonia", "New Zealand", "Nicaragua",
      "Niger", "Nigeria", "Niue", "Norfolk Island", "Northern Mariana Islands",
      "Norway", "Oman", "Pakistan", "Palau", "Palestinian Territories",
      "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines",
      "Pitcairn Islands", "Poland", "Portugal", "Puerto Rico", "Qatar",
      "R\u00e9union", "Romania", "Russia", "Rwanda", "Saint Barth\u00e9lemy",
      "Saint Helena", "Saint Kitts and Nevis", "Saint Lucia", "Saint Martin",
      "Saint Pierre and Miquelon", "Saint Vincent and the Grenadines", "Samoa",
      "San Marino", "S\u00e3o Tom\u00e9 and Pr\u00edncipe", "Saudi Arabia",
      "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore",
      "Sint Maarten", "Slovakia", "Slovenia", "Solomon Islands", "Somalia",
      "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka",
      "Sudan", "Suriname", "Svalbard and Jan Mayen", "Swaziland", "Sweden",
      "Switzerland", "Taiwan", "Tajikistan", "Tanzania", "Thailand", "Togo",
      "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan",
      "Turks and Caicos Islands", "Tuvalu", "U.S. Virgin Islands", "Uganda",
      "Ukraine", "United Arab Emirates", "United Kingdom", "United States",
      "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam",
      "Wallis and Futuna", "Yemen", "Zambia", "Zimbabwe"
    )


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

    US_states <-
      c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID",
        "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS",
        "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK",
        "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV",
        "WI", "WY")

    return(

      scrape_result %>%
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
          country = map_chr(.data$location, ~{
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
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process element_top results ##############################################

  if (scrape_result$note == "top") {

    content <-
      scrape_result$raw[[1]] %>%
      str_extract('(?<=">).*(?=</a>)') %>%
      str_replace("<span>", "") %>%
      str_replace("</span>", "") %>%
      str_split(", ") %>%
      unlist()

    # Deal with Bonaire comma
    if (content[[length(content)]] == "Sint Eustatius and Saba") {
      content[[length(content) - 1]] <- "Bonaire, Sint Eustatius and Saba"
      content <- content[1:(length(content) - 1)]
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


  ### Process _s1tlw0m results #################################################

  if (scrape_result$note == "_s1tlw0m") {

    content <-
      # The valid element seems to be the only one with a comma
      scrape_result$raw[[1]][str_detect(scrape_result$raw[[1]], ",")] %>%
      str_extract('(?<=_s1tlw0m..).*(?=<)') %>%
      str_split(", ") %>%
      unlist()

    # Deal with Bonaire comma
    if (content[[length(content)]] == "Sint Eustatius and Saba") {
      content[[length(content) - 1]] <- "Bonaire, Sint Eustatius and Saba"
      content <- content[1:(length(content) - 1)]
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


  ### Process _czm8crp results #################################################

  if (scrape_result$note == "_czm8crp") {

    return(
      scrape_result %>%
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
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }

  ### Process 3-length results #################################################

  if (length(scrape_result$raw[[1]]) == 3) {

    return(
      scrape_result %>%
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
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process 2-length results #################################################

  if (length(scrape_result$raw[[1]]) == 2) {

    return(
      scrape_result %>%
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
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }


  ### Process long-length results ##############################################

  if (length(scrape_result$raw[[1]]) > 3) {

    return(
      scrape_result %>%
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
        select(.data$property_ID, .data$city:.data$country, .data$raw,
               .data$date)
    )
  }
}
