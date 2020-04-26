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
#' @param proxies Character vector of IPs to use for proxy connections.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom crayon cyan italic silver
#' @importFrom dplyr %>%
#' @importFrom future nbrOfWorkers
#' @importFrom progressr progressor with_progress
#' @importFrom purrr map_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom stringr str_extract
#' @importFrom xml2 read_html
#' @export

upgo_scrape_craigslist <- function(city, old_results = NULL, recovery = FALSE,
                                 proxies = NULL, quiet = FALSE) {

  ### SETUP ####################################################################

  ## Initialize variables ------------------------------------------------------

  # Quiet R CMD check
  .temp_url_list <- .temp_listings <- .temp_results <- .temp_finished_flag <-
    NULL

  # Prepare for parallel processing
  doFuture::registerDoFuture()

  # Put null progress bar in .upgo_env
  .upgo_env$pb <-progressor(0)


  ## Validate city argument ----------------------------------------------------

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
      "charlotte", "charlottesville", "chattanooga", "chautauqua", "chicago",
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
      "yakima", "york", "youngstown", "yubasutter", "yuma", "zanesville",
      "toronto", "montreal", "vancouver"
      )

  if (!all(city %in% possible_cities)) {

    invalid_cities <- city[!city %in% possible_cities]

    if (length(invalid_cities) == 1) {
      stop("Invalid input to `city` argument (", invalid_cities,
           "). Inputs must correspond to ***.craigslist.org subdomains.")
    }

    if (length(invalid_cities) > 1) {
      stop("Invalid inputs to `city` argument (",
           do.call(paste, as.list(c(invalid_cities, sep = ", "))),
           "). Inputs must correspond to ***.craigslist.org subdomains.")
    }
  }


  ## Restore object if recovery == TRUE ----------------------------------------

  if (recovery) {

    finished_flag <- get(".temp_finished_flag", envir = .GlobalEnv)

    # Check if finished_flag vector is the right length
    if (length(finished_flag) != length(city)) {
      stop("The recovery data does not match the inputs. ",
           "Try re-running with recovery = FALSE.")
    }

    url_list <- get(".temp_url_list", envir = .GlobalEnv)
    listings <- get(".temp_listings", envir = .GlobalEnv)
    results <- get(".temp_results", envir = .GlobalEnv)


  ## Initialize objects if recovery == FALSE -----------------------------------

  } else {

    url_list <- vector("list", length(city))
    listings <- vector("list", length(city))
    results <- vector("list", length(city))
    finished_flag <- map(seq_along(city), ~FALSE)

  }

  ## Set on.exit expression ----------------------------------------------------

  on.exit({
    .temp_url_list <<- url_list
    .temp_listings <<- listings
    .temp_finished_flag <<- finished_flag
    .temp_results <<- results
  })


  ## Initialize proxies --------------------------------------------------------

  if (!missing(proxies)) {

    # Put proxy list in .upgo_env so it can be accessed from child functions
    .upgo_env$proxy_list <- proxies

    on.exit(rlang::env_unbind(.upgo_env, "proxy_list"), add = TRUE)

  }


  ### MAIN SCRAPING LOOP #######################################################

  for (n in seq_along(city)) {

    ## Get city_name -----------------------------------------------------------

    city_name <- city[[n]]


    ## Skip city if already finished in recovery data --------------------------

    if (finished_flag[[n]]) {
      if (!quiet) message(silver(bold(glue(
        "Recovery data for {city_name} detected; skipping scrape."))))
      next
    }

    if (!quiet) message(silver(bold(glue(
      "Scraping Craigslist rental listings in '{city_name}' ",
      "with {helper_plan()}."))))


    ## Retrieve URLs -----------------------------------------------------------

    start_time <- Sys.time()

    handler_upgo("Scraping page")

    if (!quiet) {
      with_progress(url_list[[n]] <- helper_cl_urls(city_name))
    } else {
      url_list[[n]] <- helper_cl_urls(city_name)
    }

    # Clean up
    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(url_list[[n]]), "listing URLs scraped in "),
              cyan(time_final_1, time_final_2), silver("."))
    }


    ## Process duplicate listings if old_results is provided -------------------

    if (!missing(old_results)) {

      updated_results <-
        old_results %>%
        filter(city == city_name, url %in% url_list[[n]]) %>%
        mutate(scraped = Sys.Date())

      old_results <-
        old_results %>%
        dplyr::anti_join(updated_results, by = "id") %>%
        bind_rows(updated_results)

      url_list[[n]] <-
        url_list[[n]][!url_list[[n]] %in% updated_results$url]

      # Advance loop early if there are no new listings
      if (length(url_list[[n]]) == 0) {

        listings[[n]] <- NULL
        results[[n]] <- old_results

        if (!quiet) message(silver(glue(
          "{nrow(updated_results)} previously scraped listings still active. ",
          "No new results to scrape.")))

        next
      }

      if (!quiet) message(silver(glue(
        "{nrow(updated_results)} previously scraped listings still active.")))


    }


    ## Scrape individual pages -------------------------------------------------

    start_time <- Sys.time()

    handler_upgo("Scraping listing")

    if (!quiet) {
      with_progress(
        listings[[n]] <-
          paste0(url_list[[n]], "?lang=en&cc=us") %>%
          helper_download_listing()
      )

    } else {
      listings[[n]] <-
        paste0(url_list[[n]], "?lang=en&cc=us") %>%
        helper_download_listing()
    }

    # Clean up
    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(listings[[n]]), "listings scraped in "),
              cyan(time_final_1, time_final_2), silver("."))
    }


    ## Parse HTML --------------------------------------------------------------

    handler_upgo("Parsing result")

    start_time <- Sys.time()

    with_progress({
      .upgo_env$pb <- progressor(along = listings[[n]])

      results[[n]] <-
        furrr::future_map2_dfr(listings[[n]], url_list[[n]], ~{
          .upgo_env$pb()
          helper_parse_cl(.x, .y, city_name)
          }
        )
      })


    ## Clean up ----------------------------------------------------------------

    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(nrow(results[[n]]), "listings parsed in "),
              cyan(time_final_1, time_final_2), silver("."))
    }


    ## Rbind with old_results if present, then arrange -------------------------

    if (!missing(old_results)) {
      results[[n]] <-
        bind_rows(results[[n]], old_results)
    }

    results[[n]] <-
      results[[n]] %>%
      arrange(.data$id)


    ## Set finished_flag upon successfully completing a city -------------------

    finished_flag[[n]] <- TRUE

  }


  ### RBIND AND RETURN RESULTS #################################################

  results <- bind_rows(results)

  if (!missing(proxies)) {
    on.exit(rlang::env_unbind(.upgo_env, "proxy_list"))
  } else on.exit()

  return(results)

}

