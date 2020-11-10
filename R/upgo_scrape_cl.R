#' Function to scrape Craigslist listings
#'
#' \code{upgo_scrape_cl} scrapes all data from Craigslist rental
#' listings in a given city.
#'
#' @param city A character scalar indicating the city to scrape. It must
#' correspond to the domain prefix used for a city's Craigslist site (e.g.
#' "sfbay" for the San Francisco Bay area).
#' @param old_results A data frame. If the output of a previous run of
#' \code{upgo_scrape_cl} is supplied, listings previously scraped will
#' be incorporated into the new results.
#' @param old_results_add A logical scalar. Should the function add old results
#' to the table, or skip them (default)?
#' @param recovery A logical vector. Should the function attempt to recover
#' results from a previous, unsuccessful function call?
#' @param proxies Character vector of IPs to use for proxy connections.
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom crayon cyan italic silver
#' @importFrom dplyr %>%
#' @importFrom progressr progressor with_progress
#' @importFrom purrr map_dfr
#' @importFrom stringr str_extract
#' @export

upgo_scrape_cl <- function(city, old_results = NULL, old_results_add = FALSE,
                           recovery = FALSE, proxies = NULL, quiet = FALSE) {

  ### SETUP ####################################################################

  ## Check for packages --------------------------------------------------------

  helper_require("rvest")
  helper_require("xml2")


  ## Initialize variables ------------------------------------------------------

  # Quiet R CMD check
  .temp_url_list <- .temp_listings <- .temp_results <- .temp_finished_flag <-
    NULL

  # Prepare for parallel processing
  if (requireNamespace("future", quietly = TRUE)) {
    doFuture::registerDoFuture()
  }


  ## Validate city argument ----------------------------------------------------

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
        "({n}/{length(city)}) ",
        "Recovery data for '{city_name}' detected; skipping scrape."))))
      next
    }

    if (!quiet) message(silver(bold(glue(
      "({n}/{length(city)}) ",
      "Scraping Craigslist rental listings in '{city_name}' ",
      "with {helper_plan()}."))))


    ## Retrieve URLs -----------------------------------------------------------

    start_time <- Sys.time()

    # Temporarily the same
    if (!quiet) {
      url_list[[n]] <- helper_urls_cl(city_name)
    } else {
      url_list[[n]] <- helper_urls_cl(city_name)
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

      if (old_results_add) {

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

      } else {

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
    }


    ## Scrape individual pages -------------------------------------------------

    start_time <- Sys.time()

    handler_upgo("Scraping listing")

    # Temporarily the same
    if (!quiet) {
      listings[[n]] <- helper_download_listing(paste0(url_list[[n]],
                                                      "?lang=en&cc=us"))

    } else {
      listings[[n]] <- helper_download_listing(paste0(url_list[[n]],
                                                      "?lang=en&cc=us"))
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
          pb <- progressor(along = listings[[n]])

          results[[n]] <-
            furrr::future_map2_dfr(listings[[n]], url_list[[n]], ~{
              pb()
              helper_parse_cl(.x, .y, city_name)
            })})

      } else {
        results[[n]] <-
          furrr::future_map2_dfr(listings[[n]], url_list[[n]], helper_parse_cl,
                                 city_name)
      }
    } else {

      if (!quiet) {
        with_progress({
          pb <- progressor(along = listings[[n]])

          results[[n]] <-
            purrr::map2_dfr(listings[[n]], url_list[[n]], ~{
              pb()
              helper_parse_cl(.x, .y, city_name)
            })})

      } else {
        results[[n]] <-
          purrr::map2_dfr(listings[[n]], url_list[[n]], helper_parse_cl,
                                 city_name)
      }
    }


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
        old_results %>%
        filter(.data$city == city_name) %>%
        bind_rows(results[[n]])
    }

    results[[n]] <-
      results[[n]] %>%
      arrange(.data$id)


    ## Set finished_flag upon successfully completing a city -------------------

    finished_flag[[n]] <- TRUE

  }


  ### RBIND AND RETURN RESULTS #################################################

  results <- bind_rows(results)

  return(results)

}

