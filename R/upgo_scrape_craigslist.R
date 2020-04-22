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
#' @param proxies Character vector of IPs to use for proxy connections. If
#' supplied, this must be at least as long as the number of cores.
#' @param cores A positive integer scalar. How many processing cores should be
#' used to scrape?
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
                                 proxies = NULL, cores = 1L, quiet = FALSE) {

  ### SETUP ####################################################################

  ## Initialize variables ------------------------------------------------------

  .temp_url_list <- .temp_listings <- .temp_results <- .temp_finished_flag <-
    i <- NULL

  url_end <- "&lang=en&cc=us"

  # Default to no progress bar
  opts <- list()

  if (!quiet) {
    pb_fun <- function(n) pb$tick()
    opts <- list(progress = pb_fun)
  }


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
      stop("Invalid inputs to `city` argument (", invalid_cities,
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
    on.exit(.temp_url_list <<- url_list, add = TRUE)

    listings <- get(".temp_listings", envir = .GlobalEnv)
    on.exit(.temp_listings <<- listings, add = TRUE)

    results <- get(".temp_results", envir = .GlobalEnv)
    on.exit(.temp_results <<- results, add = TRUE)

    on.exit(.temp_finished_flag <<- finished_flag, add = TRUE)


  ## Initialize objects if recovery == FALSE -----------------------------------

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


  ## Initialize multicore processing and proxies -------------------------------

  if (!quiet && cores > 1) message(silver(glue(
    "Initializing {cores} processing threads.")))

  `%dopar%` <- foreach::`%dopar%`

  (cl <- cores %>% makeCluster()) %>% registerDoSNOW()

  if (!missing(proxies)) {

    proxy_reps <- ceiling(cores/length(proxies))
    proxy_list <- rep(proxies, proxy_reps)[seq_len(cores)]

    clusterApply(cl, proxy_list, function(x) {
      set_config(use_proxy(str_extract(x, '(?<=server=).*')))
    })
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
      "Scraping Craigslist rental listings in {city_name}."))))


    ## Construct listing page URL ----------------------------------------------

    listings_url <-
      paste0("https://", city_name, ".craigslist.org/search/apa?s=0", url_end)


    ## Get URLs ----------------------------------------------------------------

    start_time <- Sys.time()

    # Find number of pages to scrape
    listings_to_scrape <-
      listings_url %>%
      read_html() %>%
      html_node(".totalcount") %>%
      html_text()

    pages <- ceiling(as.numeric(listings_to_scrape) / 120)

    # Prepare progress bar
    if (!quiet) {
      pb <- progress_bar$new(format = silver(italic(
        "Scraping listing page :current of :total [:bar] :percent, ETA: :eta")),
        total = pages, show_after = 0)

      pb$tick(0)
    }

    # Scrape pages
    url_list[[n]] <-
      foreach (i = seq_len(pages), .options.snow = opts) %dopar% {

        tryCatch({
          suppressWarnings(
            read_html(GET(paste0(
              "https://", city_name, ".craigslist.org/search/apa?s=",
              120 * (i - 1), url_end))) %>%
              html_nodes(".result-row") %>%
              str_extract('(?<=href=").*(?=" c)')
          )}, error = function(e) NULL)
        }

    url_list[[n]] <- unique(unlist(url_list[[n]])) %>% str_extract('.*(?=\\?)')

    # Clean up
    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(url_list[[n]]),
                     "listing URLs scraped in "),
              cyan(time_final_1, time_final_2), silver("."))
    }


    ## Process duplicate listings if old_results is provided -------------------

    if (!missing(old_results)) {

      updated_results <-
        old_results %>%
        filter(city == city_name,
               url %in% url_list[[n]]) %>%
        mutate(scraped = Sys.Date())

      if (!quiet) message(silver(glue(
        "{nrow(updated_results)} previously scraped listings still active.")))

      old_results <-
        old_results %>%
        filter(city == city_name,
               !url %in% url_list[[n]]) %>%
        bind_rows(updated_results)

      url_list[[n]] <-
        url_list[[n]][!url_list[[n]] %in%
                        old_results[old_results$city == city_name,]$url]

    }


    ## Scrape individual pages -------------------------------------------------

    start_time <- Sys.time()

    listings[[n]] <-
      url_list[[n]] %>%
      helper_download_listing("", "?lang=en&cc=us", quiet = quiet)

    # Clean up
    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(length(listings[[n]]), "listings scraped in "),
              cyan(time_final_1, time_final_2), silver("."))
    }


    ## Parse HTML --------------------------------------------------------------

    start_time <- Sys.time()

    if (!quiet) {
      pb <- progress_bar$new(format = silver(italic(
        "Parsing result :current of :total [:bar] :percent, ETA: :eta")),
        total = length(listings[[n]]), show_after = 0)

      pb$tick(0)
    }

    results[[n]] <-
      map2_dfr(listings[[n]], url_list[[n]], ~{
        if (!quiet) pb$tick()
        helper_parse_cl(.x, .y, city_name)
      })


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


    ## Clean up ----------------------------------------------------------------

    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(silver(nrow(results[[n]]), "listings parsed in "),
              cyan(time_final_1, time_final_2), silver("."))
    }
  }


  ### RBIND AND RETURN RESULTS #################################################

  results <- bind_rows(results)

  on.exit()

  results

}

