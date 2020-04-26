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
#' @param cores A positive integer scalar. How many processing cores should be
#' used to scrape?
#' @param quiet A logical vector. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return A table with one row per listing scraped.
#' @importFrom crayon bold cyan italic silver
#' @importFrom doSNOW registerDoSNOW
#' @importFrom dplyr %>% arrange bind_rows if_else
#' @importFrom glue glue
#' @importFrom httr GET set_config use_proxy
#' @importFrom progress progress_bar
#' @importFrom purrr map map2_dfr
#' @importFrom rvest html_node html_nodes html_text
#' @importFrom stringr str_extract
#' @importFrom xml2 read_html write_html
#' @export

upgo_scrape_kj <- function(city, old_results = NULL, short_long = "both",
                               recovery = FALSE, proxies = NULL, cores = 1L,
                               quiet = FALSE) {

  ### SETUP ####################################################################

  ## Initialize variables ------------------------------------------------------

  # Quiet R CMD check
  .temp_url_list_short <- .temp_url_list_long <- .temp_url_list <-
    .temp_listings <- .temp_results <- .temp_finished_flag <- NULL

  # Prepare for parallel processing
  doFuture::registerDoFuture()

  # Put null progress bar in .upgo_env
  .upgo_env$pb <-progressor(0)


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
      if (!quiet) message(silver(bold(glue(
        "Recovery data for {city_name} detected; skipping scrape."))))
      next
    }

    if (!quiet) message(silver(bold(glue(
      "Scraping Kijiji rental listings in {city_name}."))))


    ## Retrieve URLs -----------------------------------------------------------

    # STR
    if (short_long %in% c("short", "both")) {

      start_time <- Sys.time()

      handler_upgo("Scraping STR page")

      if (!quiet) {
        with_progress(url_list_short <- helper_urls_kj(city_name, "short"))
      } else {
        url_list_short <- helper_urls_kj(city_name, "short")
      }

      # Clean up
      total_time <- Sys.time() - start_time
      time_final_1 <- substr(total_time, 1, 4)
      time_final_2 <- attr(total_time, 'units')

      if (!quiet) {
        message(silver(length(url_list[[n]]), "STR listing URLs scraped in "),
                cyan(time_final_1, time_final_2), silver("."))
      }

    }

    # LTR
    if (short_long %in% c("long", "both")) {

      start_time <- Sys.time()

      handler_upgo("Scraping LTR page")

      if (!quiet) {
        with_progress(url_list_long <- helper_urls_kj(city_name, "long"))
      } else {
        url_list_long <- helper_urls_kj(city_name, "long")
      }

      # Clean up
      total_time <- Sys.time() - start_time
      time_final_1 <- substr(total_time, 1, 4)
      time_final_2 <- attr(total_time, 'units')

      if (!quiet) {
        message(silver(length(url_list[[n]]), "LTR listing URLs scraped in "),
                cyan(time_final_1, time_final_2), silver("."))
      }

    }


    ## Combine URLs into single list -------------------------------------------

    url_list[[n]] <-
      dplyr::case_when(
        short_long == "both"  ~ unique(c(url_list_short, url_list_long)),
        short_long == "short" ~ url_list_short,
        short_long == "long"  ~ url_list_long
        )

    # Still needed?
    url_list[[n]] <- url_list[[n]][url_list[[n]] != "https://www.kijiji.caNA"]
    url_list[[n]] <- str_replace(url_list[[n]], " ", "")








    ## Combine URLs into single list -------------------------------------------

    if (short_long == "both") {
      url_list[[n]] <- unique(c(url_list_short[[n]], url_list_long[[n]]))
    } else if (short_long == "short") {
      url_list[[n]] <- url_list_short[[n]]
    } else if (short_long == "long") {
      url_list[[n]] <- url_list_long[[n]]
    }

    url_list[[n]] <- url_list[[n]][url_list[[n]] != "https://www.kijiji.caNA"]
    url_list[[n]] <- str_replace(url_list[[n]], " ", "")


    ## Process duplicate listings if old_results is provided -------------------

    if (!missing(old_results)) {

      updated_results <-
        old_results %>%
        filter(city == city_name, url %in% url_list[[n]]) %>%
        mutate(scraped = Sys.Date())

      if (!quiet) message(silver(glue(
        "{nrow(updated_results)} previously scraped listings still active.")))

      old_results <-
        old_results %>%
        filter(city != city_name |
                 (city == city_name & !url %in% url_list[[n]])) %>%
        bind_rows(updated_results)

      url_list[[n]] <-
        url_list[[n]][!url_list[[n]] %in%
                        old_results[old_results$city == city_name,]$url]

    }


    ## Scrape individual pages -------------------------------------------------

    start_time <- Sys.time()

    listings[[n]] <-
      paste0(url_list[[n]], "?siteLocale=en_CA") %>%
      helper_download_listing()

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
        helper_parse_kijiji(.x, .y, city_name)
        })


    ## Rbind with old_results if present, then arrange -------------------------

    if (!missing(old_results)) {
      results[[n]] <-
        old_results %>%
        filter(city == city_name) %>%
        bind_rows(results[[n]])
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
      message(silver(length(listings[[n]]), "listings parsed in "),
              cyan(time_final_1, time_final_2), silver("."))
    }
  }


  ### RBIND AND RETURN RESULTS #################################################

  results <- bind_rows(results)

  on.exit()

  results

}


