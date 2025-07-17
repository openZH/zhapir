#' @name create_dataset
#' @rdname create_dataset
S7::method(create, Dataset) <- function(object, api_key, use_dev = TRUE, verbosity = 0) {
  api_request_wrapper(object, "POST", "/api/v1/datasets", api_key, use_dev, verbosity = verbosity, object_label = "Dataset")
}

#' @name create_distribution
#' @rdname create_distribution
S7::method(create, Distribution) <- function(object, api_key, use_dev = TRUE, verbosity = 0) {
  api_request_wrapper(object, "POST", "/api/v1/distributions", api_key, use_dev, verbosity = verbosity, object_label = "Distribution")
}

#' @name create_file_upload
#' @rdname create_file
S7::method(create, FileUpload) <- function(object, api_key, use_dev = TRUE, verbosity = 0) {
  api_request_wrapper(object, "POST", "/api/v1/file-uploads", api_key, use_dev, verbosity = verbosity, object_label = "FileUpload")
}
