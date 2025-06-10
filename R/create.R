#' @describeIn create a new Dataset via API call
S7::method(create, Dataset) <- function(object, api_key, use_dev = TRUE) {
  api_request_wrapper(object, "POST", "/api/v1/datasets", api_key, use_dev, object_label = "Dataset")
}

#' @describeIn create a new Distribution via API call
S7::method(create, Distribution) <- function(object, api_key, use_dev = TRUE) {
  api_request_wrapper(object, "POST", "/api/v1/distributions", api_key, use_dev, object_label = "Distribution")
}
