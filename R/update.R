#' @describeIn update a Dataset via API call
S7::method(update, Dataset) <- function(object, api_key, use_dev = TRUE) {
  update_request(
    object,
    endpoint   = paste0("/api/v1/datasets/", object@id),
    api_key    = api_key,
    use_dev    = use_dev,
    object_label = "Dataset"
  )
}

#' @describeIn update a Distribution via API call
S7::method(update, Distribution) <- function(object, api_key, use_dev = TRUE) {
  update_request(
    object,
    endpoint   = paste0("/api/v1/distributions/", object@id),
    api_key    = api_key,
    use_dev    = use_dev,
    object_label = "Distribution"
  )
}



# Helper for PATCHing an object and printing a CLI message
update_request <- function(object, endpoint, api_key, use_dev = TRUE, object_label) {
  payload <- object_to_payload(object)
  result  <- api_request("PATCH", endpoint, payload, api_key, use_dev)

  if (identical(object_label, "Distribution")) {
    # e.g. include both distribution and parent dataset info
    parent <- result$dataset$id
    cli::cli_alert_success(
      "{.strong {object_label}} {.val {result$title}} (ID {.val {result$id}}) updated inside Dataset {.val {parent}}."
    )
  } else {
    # default for datasets
    cli::cli_alert_success(
      "{.strong {object_label}} {.val {result$title}} (ID {.val {result$id}}) successfully updated."
    )
  }

  invisible(result)
}
