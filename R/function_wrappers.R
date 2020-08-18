#' Wrapper for progressr::with_progress
#'
#' @param expr An expression to be evaluated.
#' @return The result of evaluating the expression.

with_progress <- function(expr) {

  quiet <- get("quiet", envir = parent.frame(n = 1))

  if (!quiet && requireNamespace("progressr", quietly = TRUE)) {

    progressr::with_progress(expr)

  } else expr

}


# Wrapper for progressr::progressor

progressor <-

  if (requireNamespace("progressr", quietly = TRUE)) {

    progressr::progressor

  } else function(...) function(...) NULL
