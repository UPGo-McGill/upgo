upgo_connect <- function(property = TRUE, daily = TRUE, reviews = FALSE) {
  con <<- RPostgres::dbConnect(
    RPostgres::Postgres(),
    host = "025wpgs.campus.mcgill.ca",
    dbname = "airdna")

  if (property) property_all <<- dplyr::tbl(con, "property")
  if (daily) daily_all <<- dplyr::tbl(con, "daily")
  if (reviews) reviews_all <<- dplyr::tbl(con, "reviews")
}

