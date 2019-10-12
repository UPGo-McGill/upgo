#' Function to specify a bounding box for geom_sf
#'
#' \code{gg_bbox} TKTK.
#'
#' A function for TKTK.
#'
#' @param geom TKTK
#' @param x1 TKTK
#' @param x2 TKTK
#' @param y1 TKTK
#' @param y2 TKTK
#' @param ... TKTK
#' @return TKTK.
#' @importFrom ggplot2 coord_sf
#' @importFrom sf st_bbox
#' @export


gg_bbox <- function(geom, x1 = 0, x2 = 1, y1 = 0, y2 = 1, ...) {

  bbox <- st_bbox(geom)

  matrix_x <- matrix(bbox[c(1,3)], nrow = 1) %*% matrix(
    c(1 - x1, x1, 1 - x2, x2), nrow = 2)

  matrix_y <- matrix(bbox[c(2,4)], nrow = 1) %*% matrix(
    c(1 - y1, y1, 1- y2, y2), nrow = 2)

  coord_sf(xlim = as.vector(matrix_x), ylim = as.vector(matrix_y), ...)
}
