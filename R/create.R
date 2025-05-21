#' @describeIn create Create a new Dataset via API call
S7::method(create, Dataset) <- function(object, api_key, use_dev = TRUE) {
  post_to_api(object, "/api/v1/datasets", api_key, use_dev, object_label = "Dataset")
}

#' @describeIn create Create a new Distribution via API call
S7::method(create, Distribution) <- function(object, api_key, use_dev = TRUE) {
  post_to_api(object, "/api/v1/distributions", api_key, use_dev, object_label = "Distribution")
}
