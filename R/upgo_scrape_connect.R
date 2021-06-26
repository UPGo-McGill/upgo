#' Function to open Selenium server for webscraping
#'
#' \code{upgo_scrape_connect} opens a Selenium server to allow webscraping.
#'
#' This function opens a Selenium server to prepare for webscraping using one of
#' the upgo_scrape_* functions. For compatibility with those functions, it
#' assigns an object `rD` to the .upgo_env private environment, and will
#' overwrite any existing object with the same name without warning.
#'
#' @param workers A positive integer scalar. How many parallel workers should be
#' used to scrape?
#' @param proxies Character vector of IPs to use for proxy connections. If
#' this is shorter than the number of workers, proxies will be recycled.
#' @param chrome A character string specifying the version of Chrome to be used
#' with Selenium.
#' @param port Either "auto" or a positive integer scalar. If `auto`, ports
#' beginning with 4444 will be checked until sufficient unused ports are found
#' for all workers. If an integer, the port to open the first worker's server
#' on. (Subsequent workers will be at `port + 1`, `port + 2`, etc.)
#' @param headless A logical scalar. Should remote browsers operate in
#' "headless" mode (default) or open visible windows?
#' @param ... Additional arguments passed to RSelenium::rsDriver.
#' @return The function returns a connection object, which it assigns to `rD` in
#' the global environment.
#' @export

upgo_scrape_connect <- function(workers = 1L, proxies = NULL,
                                chrome = "90.0.4430.24", port = "auto",
                                headless = TRUE, ...) {

  # Error checking and initialization
  helper_require("RSelenium")
  rD <- NULL
  workers <- as.integer(workers)
  stopifnot(port == "auto" || port > 0)
  stopifnot(is.character(chrome), is.integer(workers), is.logical(headless))

  # Start cluster
  assign("cl", parallel::makeCluster(workers, setup_strategy = "sequential"),
         envir = .upgo_env)

  # Set up proxies
  proxy_number <- if (missing(proxies)) "no" else min(length(proxies), workers)
  proxies <- rep(proxies, ceiling(workers / length(proxies)))[seq_len(workers)]
  if (!is.null(proxies)) proxies <- paste0("--proxy-server=", proxies)

  # Set up ports
  if (port == "auto") {
    ports <- vector("integer", workers)
    to_check <- 4444L
    to_fill <- 1
    # Iterate over ports from 4444 on to find available ones
    while (sum(ports == 0) > 0) {
      port_free <- sum(!is.na(pingr::ping_port("localhost", to_check))) == 0
      if (port_free) {
        ports[to_fill] <- to_check
        to_fill <- to_fill + 1
      }
      to_check <- to_check + 1L
    }
  } else {
    ports <- port:(port + workers)
  }

  # Set up headless
  headless <- if (headless) "--headless" else NULL

  # Assemble config object
  config <- vector("list", workers)
  for (i in seq_along(config)) {
    config[[i]]$chrome <- chrome
    config[[i]]$headless <- headless
    config[[i]]$proxy <- character()
    config[[i]]["proxy"] <- list(proxies[i])
    config[[i]]$port <- ports[i]
  }

  # Export config
  parallel::clusterApply(.upgo_env$cl, config, \(x) {
    attach(list(config = x), name = "worker-config")
  })

  # Report status
  plural <- if (workers == 1) "" else "s"
  proxy_plural <- if (proxy_number == 1) "proxy" else "proxies"
  port_range <- if (workers == 1) ports else
    glue::glue("{min(ports)}:{max(ports)}")
  message(crayon::silver(glue::glue(
    "Initializing {workers} Selenium server{plural} ",
    "on port{plural} {port_range} with {proxy_number} {proxy_plural}.")))

  # Start servers
  out <- parallel::clusterEvalQ(.upgo_env$cl, {
    eCaps <- list(chromeOptions = list(
      args = c(config$proxy, config$headless, '--disable-gpu',
               '--window-size=1280,800',
               '--disable-backgrounding-occluded-windows'),
      w3c = FALSE))

    rD <- RSelenium::rsDriver(port = as.integer(config$port),
                                     browser = "chrome",
                                     chromever = config$chrome,
                                     check = FALSE,
                                     verbose = FALSE,
                                     extraCapabilities = eCaps)
    })

}


#' Function to close Selenium server after completing webscraping
#'
#' \code{upgo_scrape_disconnect} closes a Selenium server and removes the
#' connection object from the global environment.
#'
#' This function closes any active Selenium server and any web driver currently
#' assigned to the object `remDr`, removes both objects, and performs garbage
#' collection on memory, to ensure #' that subsequent scraping connections will
#' be made successfully.
#'
#' @return The function returns no output, but closes an open Selenium server
#' and performs associated cleanup.
#' @export

upgo_scrape_disconnect <- function() {

  helper_require("RSelenium")

  if (exists("cl", envir = .upgo_env)) {
    try(parallel::clusterEvalQ(.upgo_env$cl, {rD$server$stop()}))
    rm("cl", envir = .upgo_env)
  }

  invisible(gc())

}
