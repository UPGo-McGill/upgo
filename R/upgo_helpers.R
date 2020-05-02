#' UPGo package environment

.upgo_env <- rlang::env()


#' Helper function to characterize future plan
#'
#' \code{helper_plan} parses the current \code{future::plan} for display in
#' progress messages.
#' @return A character string reporting the current plan.
#' @importFrom future nbrOfWorkers plan

helper_plan <- function() {

  tryCatch({
    workers_number <-
      future::nbrOfWorkers()

    workers_noun <-
      if_else(workers_number == 1, "process", "processes")

    cluster_type <-
      if_else("remote" %in% class(future::plan()), "remote", "local")

    paste0(workers_number, " ", cluster_type, " ", workers_noun)
  },
  error = function(e) "1 local process"
  )
}


#' Helper function to set a progress reporting strategy
#'
#' \code{handler_upgo} sets a progress reporting strategy.
#'
#' @param message A character string describing the task being iterated.
#' @return No visible output.
#' @importFrom crayon italic silver
#' @importFrom progressr handlers handler_progress

handler_upgo <- function(message) {

  cray_opt <- .Options$crayon.enabled
  if (crayon::has_color()) options(crayon.enabled = TRUE)
  on.exit(.Options$crayon.enabled <- cray_opt)

  handlers(
    handler_progress(
      format = silver(italic(paste0(
        message,
        " :current of :total (:tick_rate/s) [:bar] :percent, ETA: :eta"))),
      show_after = 0
    ))
}


#' Helper function to check current IP address
#'
#' \code{check_ip} queries whatismyipaddress.com to determine the current IP
#' address.
#'
#' @return A character string with the current IP address.
#' @export

check_ip <- function() {

  xml2::read_html(httr::GET("https://whatismyipaddress.com")) %>%
    rvest::html_node(xpath = '//*[@id = "ipv4"]') %>%
    rvest::html_text()

}


#' Helper function to execute parallel network code with proxies
#'
#' The function executes network code in a parallel future
#' with proxies
#'
#' @param obj The sequence to iterate over
#' @param ex An expression to evaluate in parallel
#' @return A list of evaluated expressions.
#' @export

`%do_upgo%` <- function(obj, ex) {

  # If .upgo_env isn't found, behave like standard foreach %dopar%
  if (!exists(".upgo_env")) return(foreach::`%dopar%`(obj, ex))

  if (rlang::env_has(.upgo_env, "proxy_list")) {

    if (rlang::env_has(.upgo_env, "pb")) {

      second_part <-
        substitute({
          httr::set_config(httr::use_proxy(
            .upgo_env$proxy_list[[(i %% length(.upgo_env$proxy_list)) + 1]]))
          .upgo_env$pb()
          ex
        })

    } else {

      second_part <-
        substitute({
          httr::set_config(httr::use_proxy(
            .upgo_env$proxy_list[[(i %% length(.upgo_env$proxy_list)) + 1]]))
          ex
        })
    }

  } else {

    if (rlang::env_has(.upgo_env, "pb")) {

      second_part <-
        substitute({
          .upgo_env$pb()
          ex
        })

    } else {

      second_part <- ex

    }
  }

  foreach::`%dopar%`(
    obj,
    eval(second_part)
  )

}


