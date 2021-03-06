% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upgo_scrape_connect.R
\name{upgo_scrape_connect}
\alias{upgo_scrape_connect}
\title{Function to open Selenium server for webscraping}
\usage{
upgo_scrape_connect(
  workers = 1L,
  proxies = NULL,
  chrome = "90.0.4430.24",
  port = "auto",
  headless = TRUE,
  ...
)
}
\arguments{
\item{workers}{A positive integer scalar. How many parallel workers should be
used to scrape?}

\item{proxies}{Character vector of IPs to use for proxy connections. If
this is shorter than the number of workers, proxies will be recycled.}

\item{chrome}{A character string specifying the version of Chrome to be used
with Selenium.}

\item{port}{Either "auto" or a positive integer scalar. If `auto`, ports
beginning with 4444 will be checked until sufficient unused ports are found
for all workers. If an integer, the port to open the first worker's server
on. (Subsequent workers will be at `port + 1`, `port + 2`, etc.)}

\item{headless}{A logical scalar. Should remote browsers operate in
"headless" mode (default) or open visible windows?}

\item{...}{Additional arguments passed to RSelenium::rsDriver.}
}
\value{
The function returns a connection object, which it assigns to `rD` in
the global environment.
}
\description{
\code{upgo_scrape_connect} opens a Selenium server to allow webscraping.
}
\details{
This function opens a Selenium server to prepare for webscraping using one of
the upgo_scrape_* functions. For compatibility with those functions, it
assigns an object `rD` to the .upgo_env private environment, and will
overwrite any existing object with the same name without warning.
}
