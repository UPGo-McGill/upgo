#' Function to scrape Kijiji listings
#'
#' \code{upgo_scrape_kj} scrapes all data from Kijiji rental listings in
#' Montreal, Toronto or Vancouver.
#'
#' @param city A character vector indicating the city or cities to scrape.
#' Currently accepts "montreal", "toronto" or "vancouver" as inputs.
#' @param old_results A data frame. If the output of a previous run of
#' \code{upgo_scrape_kj} is supplied, listings previously scraped will be
#' incorporated into the new results.
#' @param short_long A character scalar. Should short-term rentals ("short"),
#' long-term rentals ("long") or both ("both", the default) be scraped?
#' @param recovery A logical vector. Should the function attempt to recover
#' results from a previous, unsuccessful function call?
#' @param proxies Character vector of IPs to use for proxy connections. If
#' supplied, this must be at least as long as the number of cores.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom crayon bold cyan italic silver
#' @importFrom dplyr %>% arrange bind_rows if_else
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @export

upgo_scrape_kj <- function(city, old_results = NULL, short_long = "both",
                           recovery = FALSE, proxies = NULL, quiet = FALSE) {

  ### SETUP ####################################################################

  helper_require("rvest")


  ## Initialize variables ------------------------------------------------------

  # Quiet R CMD check
  .temp_url_list_short <- .temp_url_list_long <- .temp_url_list <-
    .temp_listings <- .temp_results <- .temp_finished_flag <- NULL

  # Prepare for parallel processing
  if (requireNamespace("doFuture", quietly = TRUE)) {
    doFuture::registerDoFuture()
  }

  # Set user agent
  httr::set_config(httr::user_agent(user_agent))


  ## Validate city argument ----------------------------------------------------

  if (!all(city %in% c("montreal", "Montreal", "montr\u00e9al", "Montr\u00e9al",
                       "toronto", "Toronto", "vancouver", "Vancouver"))) {

    stop("Invalid input to `city` argument. ",
         "Currently recognized cities are Montreal, Toronto and Vancouver.")
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
    finished_flag <- purrr::map(seq_along(city), ~FALSE)

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

    if (city[[n]] %in% c("montreal", "Montreal", "montr\u00e9al",
                         "Montr\u00e9al")) {
      city_name <- "Montreal"

    } else if (city[[n]] %in% c("toronto", "Toronto")) {
      city_name <- "Toronto"

    } else if (city[[n]] %in% c("vancouver", "Vancouver")) {
      city_name <- "Vancouver"
    }


    ## Skip city if it is already finished in recovery data --------------------

    if (finished_flag[[n]]) {
      if (!quiet) message(crayon::silver(crayon::bold(glue::glue(
        "({n}/{length(city)}) ",
        "Recovery data for {city_name} detected; skipping scrape."))))
      next
    }

    if (!quiet) message(crayon::silver(crayon::bold(glue::glue(
      "({n}/{length(city)}) ",
      "Scraping Kijiji rental listings in {city_name} ",
      "with {helper_plan()}."))))


    ## Retrieve URLs -----------------------------------------------------------

    # STR
    if (short_long %in% c("short", "both")) {

      start_time <- Sys.time()
      handler_upgo("Scraping STR page")
      with_progress({url_list_short <- helper_urls_kj(city_name, "short")})

      # Simplify URLs
      url_list_short <-
        paste0("https://www.kijiji.ca/v-short-term-rental/",
               stringr::str_extract(url_list_short, '(?<=/)[:digit:]{5,}'))

      # Clean up
      total_time <- Sys.time() - start_time
      time_final_1 <- substr(total_time, 1, 4)
      time_final_2 <- attr(total_time, 'units')

      if (!quiet) {
        message(crayon::silver(length(url_list_short),
                               "STR listing URLs scraped in "),
                crayon::cyan(time_final_1, time_final_2), crayon::silver("."))}}

    # LTR
    if (short_long %in% c("long", "both")) {

      start_time <- Sys.time()
      handler_upgo("Scraping LTR page")
      with_progress({url_list_long <- helper_urls_kj(city_name, "long")})

      # Simplify URLs
      url_list_long <-
        paste0("https://www.kijiji.ca/v-apartments-condos/",
               stringr::str_extract(url_list_long, '(?<=/)[:digit:]{5,}'))

      # Clean up
      total_time <- Sys.time() - start_time
      time_final_1 <- substr(total_time, 1, 4)
      time_final_2 <- attr(total_time, 'units')

      if (!quiet) {
        message(crayon::silver(length(url_list_long),
                               "LTR listing URLs scraped in "),
                crayon::cyan(time_final_1, time_final_2), crayon::silver("."))}}


    ## Combine URLs into single list -------------------------------------------

    if (short_long == "both") {
      url_list[[n]] <- unique(c(url_list_short, url_list_long))
    }

    if (short_long == "short") {
      url_list[[n]] <- url_list_short
    }

    if (short_long == "long") {
      url_list[[n]] <- url_list_long
    }

    url_list[[n]] <- unique(url_list[[n]])


    ## Augment with previous listings if old_results is provided ---------------

    if (!missing(old_results)) {

      active_urls <-
        old_results %>%
        # Find listings which were active in the last scrape
        dplyr::filter(.data$scraped == max(.data$scraped),
                      .data$city == city_name) %>%
        # But remove duplicates with newly scraped URLs
        dplyr::filter(!url %in% url_list[[n]]) %>%
        dplyr::pull(url)

      url_list[[n]] <-
        c(url_list[[n]], active_urls)

      if (!quiet) message(crayon::silver(glue::glue(
        "{length(active_urls)} previously scraped listings to be checked.")))

    }


    ## Scrape individual pages -------------------------------------------------

    start_time <- Sys.time()

    handler_upgo("Scraping listing")

    if (!quiet) {
      with_progress(
        listings[[n]] <-
          paste0(url_list[[n]], "?siteLocale=en_CA") %>%
          helper_download_listing()
      )

    } else {
      listings[[n]] <-
        paste0(url_list[[n]], "?siteLocale=en_CA") %>%
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

    if (requireNamespace("future", quietly = TRUE)) {

      if (!quiet) {
        with_progress({
          .upgo_env$pb <- progressor(along = listings[[n]])

          results[[n]] <-
            furrr::future_map2_dfr(listings[[n]], url_list[[n]], ~{
              .upgo_env$pb()
              helper_parse_kj(.x, .y, city_name)
            })})

      } else {
        results[[n]] <-
          furrr::future_map2_dfr(listings[[n]], url_list[[n]], helper_parse_kj,
                                 city_name)
      }
    } else {

      if (!quiet) {
        with_progress({
          .upgo_env$pb <- progressor(along = listings[[n]])

          results[[n]] <-
            purrr::map2_dfr(listings[[n]], url_list[[n]], ~{
              .upgo_env$pb()
              helper_parse_kj(.x, .y, city_name)
            })})

      } else {
        results[[n]] <-
          purrr::map2_dfr(listings[[n]], url_list[[n]], helper_parse_kj,
                                 city_name)
      }
    }


    ## Clean up ----------------------------------------------------------------

    total_time <- Sys.time() - start_time
    time_final_1 <- substr(total_time, 1, 4)
    time_final_2 <- attr(total_time, 'units')

    if (!quiet) {
      message(crayon::silver(nrow(results[[n]]), "listings parsed in "),
              crayon::cyan(time_final_1, time_final_2), crayon::silver("."))
    }


    ## Rbind with old_results if present, then arrange -------------------------

    if (!missing(old_results)) {
      results[[n]] <-
        old_results %>%
        dplyr::filter(.data$city == city_name) %>%
        dplyr::bind_rows(results[[n]])
    }

    results[[n]] <-
      results[[n]] %>%
      dplyr::arrange(.data$id)


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


