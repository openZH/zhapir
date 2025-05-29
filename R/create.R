#' @describeIn create a new Dataset via API call
S7::method(create, Dataset) <- function(object, api_key, use_dev = TRUE) {
  post_request(object, "/api/v1/datasets", api_key, use_dev, object_label = "Dataset")
}

#' @describeIn create a new Distribution via API call
S7::method(create, Distribution) <- function(object, api_key, use_dev = TRUE) {
  post_request(object, "/api/v1/distributions", api_key, use_dev, object_label = "Distribution")
}

# Helper to POST an object to the API and print a CLI message
post_request <- function(object, endpoint, api_key, use_dev = TRUE, object_label) {
  payload <- object_to_payload(object)
  result  <- api_request("POST", endpoint, payload, api_key, use_dev)


  if (identical(endpoint, "/api/v1/distributions")) {
    # For distributions, include parent dataset ID
    parent_id <- result$dataset$id
    cli::cli_alert_success(
      "{.strong {object_label}} {.val {result$title}} with ID {.val {result$id}} created inside Dataset with ID {.val {parent_id}}."
    )
  } else {
    # For datasets (and all other endpoints), simple creation message (can be extended in the future)
    cli::cli_alert_success(
      "{.strong {object_label}} {.val {result$title}} created with ID {.val {result$id}}."
    )
  }

  invisible(result)
}
