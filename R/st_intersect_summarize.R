#' Function to perform areal interpolation of variables
#'
#' \code{st_intersect_summarize} performs areal interpolation of variables from
#' one geometry to another.
#'
#' A function for translating variables from one polygon geometry to another,
#' using constant areal interpolation. The function works with variables which
#' represent counts within a polygon (`count_vars`) and with variables which
#' represent averages across a polygon (`mean_vars`). Using an additional
#' variable representing the population (or other weighting factor) of each
#' polygon (`population`), the function reapportions the variables from the
#' origin geometry (in `data`) to the destination geometry (`destination`),
#' using an optional `group_vars` argument to control how the destination
#' geometry is assembled. It works by assuming that variables are evenly
#' distributed throughout the polygons.
#'
#' @param data TKTK
#' @param destination TKTK
#' @param group_vars TKTK
#' @param population TKTK
#' @param count_vars TKTK
#' @param mean_vars TKTK
#' @return The function returns a table with the variables of interest
#' reassembled in the `destination` geometry (potentially modified by
#' `group_vars`).
#' @importFrom dplyr %>% full_join group_by mutate summarize summarize_at
#' @importFrom purrr reduce
#' @importFrom rlang .data :=
#' @importFrom sf st_area st_drop_geometry st_intersection
#' @export

st_intersect_summarize <- function(data, destination, group_vars = NULL,
                                   population, count_vars, mean_vars) {

  data <- data %>%
    mutate(CT_area = st_area(.))

  intersects <- suppressWarnings(st_intersection(data, destination)) %>%
    mutate(int_area_pct = st_area(.data$geometry) / .data$CT_area,
           population_int = {{ population }} * .data$int_area_pct) %>%
    group_by(!!! group_vars)

  population <- intersects %>%
    summarize({{ population }} := sum(.data$population_int, na.rm = TRUE))

  sums <- intersects %>%
    summarize_at(count_vars, ~{sum(. * int_area_pct, na.rm = TRUE) /
        sum(.data$population_int, na.rm = TRUE)})

  means <- intersects %>%
    summarize_at(mean_vars, ~{
      sum(. * .data$population_int, na.rm = TRUE) /
        sum(.data$population_int, na.rm = TRUE)
    })

  suppressMessages(reduce(list(population,
                               st_drop_geometry(sums),
                               st_drop_geometry(means)),
                          full_join))

}
