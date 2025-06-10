#' @describeIn update a Dataset via API call
S7::method(update, Dataset) <- function(object, api_key, use_dev = TRUE) {
  api_request_wrapper(
    object,
    "PATCH",
    endpoint   = paste0("/api/v1/datasets/", object@id),
    api_key    = api_key,
    use_dev    = use_dev,
    object_label = "Dataset"
  )
}

#' @describeIn update a Distribution via API call
S7::method(update, Distribution) <- function(object, api_key, use_dev = TRUE) {
  api_request_wrapper(
    object,
    "PATCH",
    endpoint   = paste0("/api/v1/distributions/", object@id),
    api_key    = api_key,
    use_dev    = use_dev,
    object_label = "Distribution"
  )
}
