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
#' The output will be projected in the same CRS as the `destination` table.
#'
#' @param data An sf data frame with polygon geometries, containing the fields
#' to be aggregated and reapportioned.
#' @param destination An sf data frame with polygon geometries which the data
#' should be reapportioned to.
#' @param population The name of a field in the `data` table which sets the
#' weighting for other variables.
#' @param group_vars The name of a field in the `destination` table to be used
#' to reassemble the data. This will generally be a unique identifier (e.g. a
#' GeoUID) of the polygons in the `destination` table. If NULL (default), TKTK
#' @param count_vars A list of field names which are present in the `data`
#' table and which represent per-polygon counts which can be meaningfully
#' aggregated through addition. (An example is the number of residents who
#' commute to work by car.) The variable list must be supplied inside of
#' `vars()`.
#' @param mean_vars A list of field names which are present in the `data`
#' table and which represent per-polygon averages which cannot be meaningfully
#' aggregated through addition. (An example is the percentage of residents who
#' commute to work by car.) The variable list must be supplied inside of
#' `vars()`.
#' @return The function returns a table with the variables of interest
#' reassembled in the `destination` geometry (potentially modified by
#' `group_vars`).
#' @importFrom dplyr %>% full_join group_by mutate summarize summarize_at
#' @importFrom purrr reduce
#' @importFrom rlang .data :=
#' @importFrom sf st_area st_crs st_drop_geometry st_intersection st_transform

st_intersect_summarize <- function(data, destination, population,
                                   group_vars = NULL, count_vars = NULL,
                                   mean_vars = NULL) {

  # Set both tables to the destination CRS
  data <-
    st_transform(st_crs(destination))

  data <-
    data %>%
    mutate(data_area = st_area(.))

  intersects <-
    suppressWarnings(st_intersection(data, destination)) %>%
    mutate(
      int_area_pct = st_area(.data$geometry) / .data$data_area,
      population_int = {{ population }} * .data$int_area_pct
      ) %>%
    group_by(!!! group_vars)

  population <-
    intersects %>%
    summarize({{ population }} := sum(.data$population_int, na.rm = TRUE))

  sums <-
    intersects %>%
    summarize_at(count_vars, ~{sum(. * int_area_pct, na.rm = TRUE) /
        sum(.data$population_int, na.rm = TRUE)})

  means <-
    intersects %>%
    summarize_at(mean_vars, ~{
      sum(. * .data$population_int, na.rm = TRUE) /
        sum(.data$population_int, na.rm = TRUE)
    })

  suppressMessages(reduce(list(population,
                               st_drop_geometry(sums),
                               st_drop_geometry(means)),
                          full_join))

}
