#' Function to open Selenium server for webscraping
#'
#' \code{upgo_scrape_connect} opens a Selenium server to allow webscraping.
#'
#' This function opens a Selenium server to prepare for webscraping using one of
#' the upgo_scrape_* functions. For compatibility with those functions, it
#' assigns an object `rD` to the global environment, and will overwrite any
#' existing object with the same name without warning.
#'
#' @param chrome A character string specifying the version of Chrome to be used
#' with Selenium.
#' @param proxy A character string. The URL of a proxy server to connect
#' through.
#' @param port An integer scalar. The port to open the server on.
#' @param ... Additional arguments passed to RSelenium::rsDriver.
#' @return The function returns a connection object, which it assigns to `rD` in
#' the global environment.
#' @export

upgo_scrape_connect <- function(chrome = "90.0.4430.24", proxy = NULL,
                                port = 4444L, ...) {

  helper_require("RSelenium")

  rD <- NULL

  if (missing(proxy)) {

    eCaps <- list(chromeOptions = list(
      args = c('--headless', '--disable-gpu', '--window-size=1280,800',
               '--disable-backgrounding-occluded-windows'),
      w3c = FALSE))

  } else {

    eCaps <- list(chromeOptions = list(
      args = c(paste0("--proxy-server=", proxy), '--headless', '--disable-gpu',
               '--window-size=1280,800',
               '--disable-backgrounding-occluded-windows'),
      w3c = FALSE))

  }

  message(crayon::silver(glue::glue("Initializing Selenium server.")))

  assign("rD",
         RSelenium::rsDriver(port = port,
                             browser = "chrome",
                             chromever = chrome,
                             extraCapabilities = eCaps,
                             check = TRUE,
                             verbose = FALSE,
                             ...),
         envir = .upgo_env)
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

  if (exists("rD", envir = .upgo_env)) .upgo_env$rD$server$stop()

  if (exists("remDr")) rm("remDr", envir = .GlobalEnv)

  if (exists("rD", envir = .upgo_env)) rm("rD", envir = .upgo_env)

  gc()

}
