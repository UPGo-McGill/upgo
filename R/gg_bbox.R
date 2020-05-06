#' Function to specify a bounding box for geom_sf
#'
#' \code{gg_bbox} takes an sf object and uses its bounding box for a ggplot.
#'
#' A function for manually specifying the bounding box of a ggplot. It is a
#' convenience wrapper around \code{ggplot2::coord_sf}.
#'
#' @param geom An sf table whose bounding box will be used for ggplot.
#' @param x1 The left boundary of the bounding box.
#' @param x2 The right boundary of the bounding box.
#' @param y1 The top boundary of the bounding box.
#' @param y2 The bottom boundary of the bounding box.
#' @param ... Additional arguments to be passed to coord_sf.
#' @return A coord_sf call which can be inserted into a ggplot stack.
#' @export


gg_bbox <- function(geom, x1 = 0, x2 = 1, y1 = 0, y2 = 1, ...) {

  helper_require("ggplot2")
  helper_require("sf")

  bbox <- sf::st_bbox(geom)

  matrix_x <- matrix(bbox[c(1,3)], nrow = 1) %*% matrix(
    c(1 - x1, x1, 1 - x2, x2), nrow = 2)

  matrix_y <- matrix(bbox[c(2,4)], nrow = 1) %*% matrix(
    c(1 - y1, y1, 1- y2, y2), nrow = 2)

  ggplot2::coord_sf(xlim = as.vector(matrix_x), ylim = as.vector(matrix_y), ...)
}
