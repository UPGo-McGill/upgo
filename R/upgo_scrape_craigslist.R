#' Function to scrape Craigslist listings
#'
#' \code{upgo_scrape_craigslist} scrapes all data from Craigslist rental
#' listings in a given city.
#'
#' @param city A character scalar indicating the city to scrape. It must
#' correspond to the domain prefix used for a city's Craigslist site (e.g.
#' "sfbay" for the San Francisco Bay area).
#' @param old_results A data frame. If the output of a previous run of
#' \code{upgo_scrape_craigslist} is supplied, listings previously scraped will
#' be incorporated into the new results.
#' @param recovery A logical vector. Should the function attempt to recover
#' results from a previous, unsuccessful function call?
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom crayon cyan italic silver
#' @importFrom dplyr %>%
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom stringr str_extract
#' @importFrom xml2 read_html
#' @export

upgo_scrape_craigslist <- function(city, old_results = NULL, recovery = FALSE,
                                   quiet = FALSE) {

  ### Setup ####################################################################

  .temp_url_list <- .temp_listings <- .temp_results <- .temp_finished_flag <-
    NULL


  ### Validate city argument ###################################################

  possible_cities <-
    c("abilene", "akroncanton", "albanyga", "albany", "albuquerque", "altoona",
      "amarillo", "ames", "anchorage", "annapolis", "annarbor", "appleton",
      "asheville", "ashtabula", "athensga", "athensohio", "atlanta", "auburn",
      "augusta", "austin", "bakersfield", "baltimore", "batonrouge",
      "battlecreek", "beaumont", "bellingham", "bemidji", "bend", "billings",
      "binghamton", "bham", "bismarck", "bloomington", "bn", "boise", "boone",
      "boston", "boulder", "bgky", "bozeman", "brainerd", "brownsville",
      "brunswick", "buffalo", "butte", "capecod", "catskills", "cedarrapids",
      "cenla", "centralmich", "cnj", "chambana", "charleston", "charlestonwv",
      "charlotte", "charlottesville, chattanooga", "chautauqua", "chicago",
      "chico", "chillicothe", "cincinnati", "clarksville", "cleveland",
      "clovis", "collegestation", "cosprings", "columbiamo", "columbia",
      "columbusga", "columbus", "cookeville", "corpuschristi", "corvallis",
      "chambersburg", "dallas", "danville", "daytona", "dayton", "decatur",
      "nacogdoches", "delaware", "delrio", "denver", "desmoines", "detroit",
      "dothan", "dubuque", "duluth", "eastco", "newlondon", "eastky", "montana",
      "eastnc", "martinsburg", "easternshore", "eastidaho", "eastoregon",
      "eauclaire", "elko", "elmira", "elpaso", "erie", "eugene", "evansville",
      "fairbanks", "fargo", "farmington", "fayar", "fayetteville",
      "fingerlakes", "flagstaff", "flint", "shoals", "florencesc", "keys",
      "fortcollins", "fortdodge", "fortsmith", "fortwayne", "frederick",
      "fredericksburg", "fresno", "fortmyers", "gadsden", "gainesville",
      "galveston", "glensfalls", "goldcountry", "grandforks", "grandisland",
      "grandrapids", "greatfalls", "greenbay", "greensboro", "greenville",
      "gulfport", "hanford", "harrisburg", "harrisonburg", "hartford",
      "hattiesburg", "honolulu", "cfl", "helena", "hickory", "rockies",
      "hiltonhead", "holland", "houma", "houston", "hudsonvalley", "humboldt",
      "huntington", "huntsville", "imperial", "indianapolis", "inlandempire",
      "iowacity", "ithaca", "jxn", "jackson", "jacksontn", "jacksonville",
      "onslow", "janesville", "jerseyshore", "jonesboro", "joplin", "kalamazoo",
      "kalispell", "kansascity", "kenai", "kpr", "racine", "killeen",
      "kirksville", "klamath", "knoxville", "kokomo", "lacrosse", "lafayette",
      "tippecanoe", "lakecharles", "lakeland", "loz", "lancaster", "lansing",
      "laredo", "lasalle", "lascruces", "lasvegas", "lawrence", "lawton",
      "allentown", "lewiston", "lexington", "limaohio", "lincoln", "littlerock",
      "logan", "longisland", "losangeles", "louisville", "lubbock", "lynchburg",
      "macon", "madison", "maine", "ksu", "mankato", "mansfield", "masoncity",
      "mattoon", "mcallen", "meadville", "medford", "memphis", "mendocino",
      "merced", "meridian", "milwaukee", "minneapolis", "missoula", "mobile",
      "modesto", "mohave", "monroe", "monroemi", "monterey", "montgomery",
      "morgantown", "moseslake", "muncie", "muskegon", "myrtlebeach",
      "nashville", "nh", "newhaven", "neworleans", "blacksburg", "newyork",
      "norfolk", "lakecity", "nd", "nesd", "nmi", "wheeling", "northernwi",
      "newjersey", "northmiss", "northplatte", "nwct", "nwga", "nwks", "enid",
      "ocala", "odessa", "ogden", "okaloosa", "oklahomacity", "olympic",
      "omaha", "oneonta", "orangecounty", "oregoncoast", "orlando",
      "outerbanks", "owensboro", "palmsprings", "panamacity", "parkersburg",
      "pensacola", "peoria", "philadelphia", "phoenix", "csd", "pittsburgh",
      "plattsburgh", "poconos", "porthuron", "portland", "potsdam", "prescott",
      "provo", "pueblo", "pullman", "quadcities", "raleigh", "rapidcity",
      "reading", "redding", "reno", "providence", "richmondin", "richmond",
      "roanoke", "rmn", "rochester", "rockford", "roseburg", "roswell",
      "sacramento", "saginaw", "salem", "salina", "saltlakecity", "sanangelo",
      "sanantonio", "sandiego", "sandusky", "slo", "sanmarcos", "santabarbara",
      "santafe", "santamaria", "sarasota", "savannah", "scottsbluff",
      "scranton", "seattle", "sfbay", "sheboygan", "showlow", "shreveport",
      "sierravista", "siouxcity", "siouxfalls", "siskiyou", "skagit",
      "southbend", "southcoast", "sd", "juneau", "ottumwa", "seks", "semo",
      "carbondale", "smd", "swv", "miami", "southjersey", "swks", "swmi",
      "marshall", "natchez", "bigbend", "swva", "spacecoast", "spokane",
      "springfieldil", "springfield", "pennstate", "statesboro", "staugustine",
      "stcloud", "stgeorge", "stillwater", "stjoseph", "stlouis", "stockton",
      "susanville", "syracuse", "tallahassee", "tampa", "terrehaute",
      "texarkana", "texoma", "thumb", "toledo", "topeka", "treasure",
      "tricities", "tucson", "tulsa", "tuscaloosa", "tuscarawas", "twinfalls",
      "twintiers", "easttexas", "up", "utica", "valdosta", "ventura", "vermont",
      "victoriatx", "visalia", "waco", "washingtondc", "waterloo", "watertown",
      "wausau", "wenatchee", "quincy", "westky", "westmd", "westernmass",
      "westslope", "wv", "wichitafalls", "wichita", "williamsport",
      "wilmington", "winchester", "winstonsalem", "worcester", "wyoming",
      "yakima", "york", "youngstown", "yubasutter", "yuma", "zanesville"
      )

if (!all(city %in% possible_cities)) {

    invalid_cities <- city[!city %in% possible_cities]

    if (length(invalid_cities) == 1) {
      stop("Invalid input to `city` argument (", invalid_cities,
           "). Inputs must correspond to ***.craigslist.org subdomains.")
    }

    if (length(invalid_cities) > 1) {
      stop("Invalid inputs to `city` argument (", invalid_cities,
           "). Inputs must correspond to ***.craigslist.org subdomains.")
    }
  }


  ### Restore object if recovery == TRUE #######################################

  if (recovery) {

    finished_flag <- get(".temp_finished_flag", envir = .GlobalEnv)

    # Check if finished_flag vector is the right length
    if (length(finished_flag) != length(city)) {
      stop("The recovery data does not match the inputs. ",
           "Try re-running with recovery = FALSE.")
    }

    url_list <- get(".temp_url_list", envir = .GlobalEnv)
    on.exit(.temp_url_list <<- url_list, add = TRUE)

    listings <- get(".temp_listings", envir = .GlobalEnv)
    on.exit(.temp_listings <<- listings, add = TRUE)

    results <- get(".temp_results", envir = .GlobalEnv)
    on.exit(.temp_results <<- results, add = TRUE)

    on.exit(.temp_finished_flag <<- finished_flag, add = TRUE)


  ### Initialize objects if recovery == FALSE ##################################

  } else {

    url_list <- vector("list", length(city))
    on.exit(.temp_url_list <<- url_list, add = TRUE)

    listings <- vector("list", length(city))
    on.exit(.temp_listings <<- listings, add = TRUE)

    results <- vector("list", length(city))
    on.exit(.temp_results <<- results, add = TRUE)

    finished_flag <- map(seq_along(city), ~FALSE)
    on.exit(.temp_finished_flag <<- finished_flag, add = TRUE)

  }


  ### Main scraping loop #######################################################

  for (n in seq_along(city)) {

    ## Get city_name

    if (city[[n]] %in% c("montreal", "Montreal", "montr\u00e9al",
                         "Montr\u00e9al")) {
      city_name <- "Montreal"

    } else if (city[[n]] %in% c("toronto", "Toronto")) {
      city_name <- "Toronto"

    } else if (city[[n]] %in% c("vancouver", "Vancouver")) {
      city_name <- "Vancouver"
    }


    ## Skip city if it is already finished in recovery data

    if (finished_flag[[n]]) {
      if (!quiet) message(silver(bold(glue(
        "Recovery data for {city_name} detected; skipping scrape."))))
      next
    }

    if (!quiet) message(silver(bold(glue(
      "Scraping Kijiji rental listings in {city_name}."))))

  }










  ### Find number of listings ##################################################

  listings_page <-
    read_html(paste0(
      "https://",
      city,
      ".craigslist.org/d/apts-housing-for-rent/search/apa?lang=en&cc=us"))

  listings_to_scrape <-
    listings_page %>%
    html_node(".totalcount") %>%
    html_text()

  pages <- ceiling(as.numeric(listings_to_scrape) / 120)


  ### Scrape listing URLs ######################################################

  # Initialize url list
  url_list <- list()

  on.exit(.temp_url_list <<- url_list)

  for (i in seq_len(pages)) {

    listings <-
      read_html(paste0(
        "https://",
        city,
        ".craigslist.org/search/apa?s=",
        120 * (i - 1),
        "&lang=en&cc=us"
      ))

    url_list[[i]] <-
      suppressWarnings(
        listings %>%
          html_nodes(".result-row") %>%
          str_extract('(?<=href=").*(?=" c)')
      )

    if (!quiet) message("Page ", i, " of ", pages, " scraped.")

  }

  url_list <- unique(unlist(url_list))


  ### Remove duplicate listings if old_results is provided

  if (!missing(old_results)) {

    url_list <-
      url_list[!str_replace(url_list,
                            "\\?lang=en&amp;cc=us", "") %in% old_results$url]

  }


  ### Scrape individual pages ##################################################

  listings <- list()

  on.exit(.temp_listings <<- listings, add = TRUE)

  for (i in seq_along(url_list)) {

    listings[[i]] <-
      tryCatch({
        url_list[[i]] %>%
          read_html(options = "HUGE")
      }, error = function(e) NULL)

    if (!quiet) message("Listing ", i, " of ", length(url_list), " scraped.")

  }


  ### Clean up #################################################################

  listings <- listings[!map_lgl(listings, is.null)]


  ### Parse HTML ###############################################################

  results <-
    listings %>%
    map_dfr(~{
      tibble(
        id = tryCatch({
          .x %>%
            html_node(xpath = '/html/body/section[@class="page-container"]') %>%
            html_node(xpath = 'section[@class="body"]') %>%
            html_node(xpath = 'section[@class="userbody"]') %>%
            html_node(xpath = 'div[@class="postinginfos"]') %>%
            html_node("p") %>%
            html_text() %>%
            str_extract('(?<=id: ).*')
        }, error = function(e) NA_character_),
        url =
          .x %>%
          html_node(xpath = '/html/head/link[@rel="canonical"]/@href') %>%
          html_text(),
        title =
          .x %>%
          html_node(xpath = '/html/head/title') %>%
          html_text(),
        date =
          .x %>%
          html_node(xpath = '//*[@id="display-date"]/time/@datetime') %>%
          html_text(),
        price =
          tryCatch({
            .x %>%
            html_node(xpath = '/html/body') %>%
              html_node(xpath = 'section[@class = "page-container"]') %>%
              html_node(xpath = 'section[@class = "body"]') %>%
              html_node(xpath = 'h2[@class = "postingtitle"]') %>%
              html_node(xpath = 'span/span[@class = "price"]') %>%
              html_text() %>%
              readr::parse_number()
          }, error = function(e) NA_real_),
        location =
          .x %>%
          html_node(xpath = '/html/head') %>%
          html_node(xpath = 'meta[@name = "geo.position"]/@content') %>%
          html_text(),
        details =
          tryCatch({
            .x %>%
              html_node(xpath = '/html/body/section[@class="page-container"]') %>%
              html_node(xpath = 'section[@class="body"]') %>%
              html_node(xpath = 'section[@class="userbody"]') %>%
              html_node(xpath = 'div[@class = "mapAndAttrs"]') %>%
              html_nodes(xpath = 'p') %>%
              html_text() %>%
              str_replace_all("\n", "") %>%
              str_replace_all("  ", " ") %>%
              str_replace_all("  ", " ") %>%
              str_replace_all("  ", " ") %>%
              str_replace_all("  ", " ") %>%
              str_extract('(?<= ).*(?= )') %>%
              paste(collapse = "; ")
            }, error = function(e) NA_character_),
        text =
          tryCatch({
            .x %>%
              html_node(xpath = '/html/body/section[@class="page-container"]'
                        ) %>%
              html_node(xpath = 'section[@class= "body"]') %>%
              html_node(xpath = 'section[@class= "userbody"]') %>%
              html_node(xpath = 'section[@id = "postingbody"]') %>%
              html_text() %>%
              str_replace(
                "\n            QR Code Link to This Post\n            \n        \n", ""
                ) %>%
              str_replace_all("\u2022\t", "") %>%
              str_replace_all("\n", " ")
          }, error = function(e) NA_character_),
        photos =
          list(.x %>%
          html_nodes(xpath = '//*/img') %>%
          html_node(xpath = '@src') %>%
          html_text() %>%
          # `[`(2:length(.)) %>%
          str_replace("50x50c", "600x450") %>%
          unique())
        )
    })

  on.exit()

  results

}

