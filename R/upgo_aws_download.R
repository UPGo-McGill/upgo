#' Function to download file from the UPGo AWS bucket
#'
#' \code{upgo_aws_download} downloads a specified file from the UPGo AWS bucket
#'
#' This is a convenience wrapper around \code{aws.s3::save_object} to download
#' a single object from the UPGo AWS bucket. It will only work with appropriate
#' values for `AWS_ACCESS_KEY_ID` and `AWS_SECRET_ACCESS_KEY` saved in
#' .Renviron.
#'
#' @param object_name A character string which identifies the file name to be
#' downloaded.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return The output will be a file saved to disk in the current working
#' directory.

#' @export

upgo_aws_download <- function(object_name, quiet = FALSE) {

  time_1 <- Sys.time()

  if (!quiet) message("Beginning file download. (",
                      substr(Sys.time(), 12, 19), ")")

  aws.s3::save_object(
    bucket = "airdna-data/McGill",
    region = "us-west-1",
    check_region = FALSE,
    object = object_name)

  total_time <- Sys.time() - time_1

  if (!quiet) {message("Analysis complete. (",
                       substr(Sys.time(), 12, 19), ")")}

  if (!quiet) {message("Total time: ",
                       substr(total_time, 1, 5), " ",
                       attr(total_time, "units"), ".")}
}
