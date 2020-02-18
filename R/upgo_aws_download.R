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
#' @param file_path A character string which optionally identifies a subfolder
#' of the working directory in which to save the output.
#' @param quiet A logical scalar. Should the function execute quietly, or should
#' it return status updates throughout the function (default)?
#' @return The output will be a file saved to disk in the current working
#' directory (possibly modified by the `file_path` argument).

#' @export

upgo_aws_download <- function(object_name, file_path = NULL, quiet = FALSE) {

  time_1 <- Sys.time()

  file_path_final <- object_name

  if (!missing(file_path)) {
    file_path_final <- paste0(file_path, "/", file_path_final)
  }

  if (!quiet) message("Beginning file download. (",
                      substr(Sys.time(), 12, 19), ")")

  upgo_bucket <- paws::s3()

  obj <- upgo_bucket$get_object(Bucket = "airdna-data",
                           Key = paste0("McGill/", object_name))

  writeBin(obj$Body, con = file_path_final)

  total_time <- Sys.time() - time_1

  if (!quiet) {message("Download complete. (",
                       substr(Sys.time(), 12, 19), ")")}

  if (!quiet) {message("Total time: ",
                       substr(total_time, 1, 5), " ",
                       attr(total_time, "units"), ".")}
}
