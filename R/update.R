#' @name update_dataset
#' @rdname update_dataset
#'
S7::method(update, Dataset) <- function(object, api_key, use_dev = TRUE, verbosity = 0) {
  api_request_wrapper(
    object,
    "PATCH",
    endpoint   = paste0("/api/v1/datasets/", object@id),
    api_key    = api_key,
    use_dev    = use_dev,
    verbosity  = verbosity,
    object_label = "Dataset"
  )
}

#' @name update_distribution
#' @rdname update_distribution
S7::method(update, Distribution) <- function(object, api_key, use_dev = TRUE, verbosity = 0) {
  api_request_wrapper(
    object,
    "PATCH",
    endpoint   = paste0("/api/v1/distributions/", object@id),
    api_key    = api_key,
    use_dev    = use_dev,
    verbosity  = verbosity,
    object_label = "Distribution"
  )
}
