#' Function to convert between two currencies
#'
#' \code{convert_currency} produces an exchange rate between two currencies for
#' a specified date range.
#'
#' A function for producing an exchange rate for two countries' currencies over
#' a specified period of time. The function uses the fixer.io currency
#' conversion service, and relies on the \code{fixerapi} package, which must be
#' separately installed. A fixer.io API key must also be supplied, either
#' through the \code{fixerapi::fixer_api_key} function or by adding the key as a
#' FIXER_API_KEY variable in .Renviron.
#'
#' @param currency_from,currency_to A character scalar indicating the currency
#' to be converted from or to. Valid country codes can be found using the
#' function \code{fixerapi::fixer_symbols}.
#' @param start_date,end_date A date or a character string which can be coerced
#' to a date. A date range will be constructed from the first day of the month
#' in which `start_date` is located and the last day of the month in which
#' `end_date` is located.
#' @return A table with two columns: `year_month` and `exchange_rate`.
#' @export

convert_currency <- function(currency_from = "USD", currency_to = "CAD",
                             start_date, end_date) {

  ## Setup ---------------------------------------------------------------------

  helper_require("fixerapi")
  helper_require("lubridate")

  `%m-%` <- lubridate::`%m-%`
  `%m+%` <- lubridate::`%m+%`
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)


  ## Calculate date range ------------------------------------------------------

  year_range <-
    as.numeric(substr(end_date, 1, 4)) - as.numeric(substr(start_date, 1, 4))

  month_range <-
    as.numeric(substr(end_date, 6, 7)) - as.numeric(substr(start_date, 6, 7))

  number_of_months <-
    year_range * 12 + month_range + 1


  ## Get vector of year_month entries ------------------------------------------

  year_month <-

    purrr::map_chr(seq_len(number_of_months), ~{

      date <- start_date %m+% months(.x - 1)
      substr(date, 1, 7)

      })


  ## Get vector of conversion rates --------------------------------------------

  result <- purrr::map_dbl(seq_len(number_of_months) - 1, ~{
    ex_table <- fixerapi::fixer_historical(
      date = (end_date %m-% months(.x)),
      symbols = c(currency_to, currency_from))

    ex_table[1,]$value / ex_table[2,]$value
    })


  ## Construct output ----------------------------------------------------------

  dplyr::tibble(
    year_month = year_month,
    exchange_rate = result
  )

}
