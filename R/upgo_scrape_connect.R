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
#' @return The function returns a connection object, which it assigns to `rD` in
#' the global environment.
#' @export

upgo_scrape_connect <- function(chrome = "81.0.4044.69") {

  helper_require("RSelenium")

  rD <- NULL

  eCaps <- list(chromeOptions = list(
    args = c('--headless', '--disable-gpu', '--window-size=1280,800'),
    w3c = FALSE))

  message(crayon::silver(glue::glue("Initializing Selenium server.")))

  assign("rD",
         RSelenium::rsDriver(port = 4444L,
                             browser = "chrome",
                             chromever = chrome,
                             extraCapabilities = eCaps,
                             verbose = FALSE),
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

  rm("rD", envir = .upgo_env)

  gc()

}
