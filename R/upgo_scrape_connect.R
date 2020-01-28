#' Function to open Selenium server for webscraping
#'
#' \code{upgo_scrape_connect} opens a Selenium server to allow webscraping.
#'
#' This function opens a Selenium server to prepare for webscraping using one of
#' the upgo_scrape_* functions. For compatibility with those functions, it
#' assigns an object `rD` to the global environment, and will overwrite any
#' existing object with the same name without warning.
#'
#' @return The function returns a connection object, which it assigns to `rD` in
#' the global environment.
#' @import RSelenium
#' @export

upgo_scrape_connect <- function() {

  rD <- NULL

  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
    w3c = FALSE))

  rD <<- rsDriver(port = 4444L, browser = "chrome",
                  chromever = "79.0.3945.36", extraCapabilities = eCaps)
}


#' Function to close Selenium server after completing webscraping
#'
#' \code{upgo_scrape_disconnect} closes a Selenium server and removes the
#' connection object from the global environment.
#'
#' This function closes any active Selenium server currently assigned to the
#' object `rD`, any web driver currently assigned to the object `remDr`,
#' removes both objects, and performs garbage collection on memory, to ensure
#' that subsequent scraping connections will be made successfully.
#'
#' @param connection A symbol corresponding to the open connection object.
#' @return The function returns no output, but closes an open Selenium server
#' and performs associated cleanup.
#' @import RSelenium
#' @export

upgo_scrape_disconnect <- function(connection = rD) {

  rD$server$stop()

  if (exists("remDr")) rm(remDr, envir = .GlobalEnv)

  rm(rD, envir = .GlobalEnv)

  rD <- remDr <- NULL

  gc()

}
