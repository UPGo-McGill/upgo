#' UPGo package environment

upgo_env <- rlang::env()


#' Aliases to foreach functions
#' @param obj The `foreach` statement.
#' @param ex The expression to be iterated.

upgo_do <- foreach::`%do%`
upgo_dopar <- foreach::`%dopar%`


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


#' Helper function to execute paralle network code with proxies
#'
#' \code{helper_do_parallel} executes network code in a parallel future with
#' proxies
#'
#' @param iterator The sequence to iterate over
#' @param statement An expression to evaluate in parallel
#' @importFrom rlang enexpr
#' @return A list of evaluated expressions.

helper_do_parallel <- function(iterator, statement) {

  iterator <- enexpr(iterator)
  statement <- enexpr(statement)

  first_part <- call("foreach", i = iterator)

  if (rlang::env_has(upgo_env, "proxy_list")) {

    second_part <-
      substitute({
        httr::set_config(httr::use_proxy(
          upgo_env$proxy_list[[(i %% length(upgo_env$proxy_list)) + 1]]))
        upgo_env$pb()
        statement
        })

  } else {

    second_part <-
      substitute({
        upgo_env$pb()
        statement
        })

  }

  call("upgo_dopar", first_part, second_part)

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

  handlers(
    handler_progress(
      format = silver(italic(paste0(
        message, " :current of :total [:bar] :percent, ETA: :eta"))),
      show_after = 0
    ))
}
