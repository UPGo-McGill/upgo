#' Function to open Selenium server for webscraping
#'
#' \code{upgo_scrape_connect} opens a Selenium server to allow webscraping.
#'
#' TKTK
#'
#' @return The function returns a connection object, which should be assigned to
#' `rD`.
#' @import RSelenium
#' @export

upgo_scrape_connect <- function() {

  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
    w3c = FALSE))

  rsDriver(port = 4444L, browser = "chrome",
           chromever = "78.0.3904.70", extraCapabilities = eCaps)
}


#' Function to close Selenium server after completing webscraping
#'
#' \code{upgo_scrape_disconnect} closes a Selenium server and removes the
#' connection object from the global environment.
#'
#' TKTK
#'
#' @param connection A symbol corresponding to the open connection object.
#' @return The function returns no output, but closes an open Selenium server
#' and performs associated cleanup.
#' @import RSelenium
#' @export

upgo_scrape_disconnect <- function(connection = rD) {

  rD$server$stop()

  rm(rD, envir = .GlobalEnv)

  gc()

}
