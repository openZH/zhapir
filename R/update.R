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



#' Perform an API PATCH request to update a metadata object.
#'
#' This function sends an update request to the API for the given object
#' (e.g., a Dataset or Distribution). It handles success and error feedback via CLI messages.
#'
#' @param object        An S7 metadata object to update.
#' @param endpoint      Character; the API endpoint for the object (e.g., "/api/v1/distributions/{id}").
#' @param api_key       Character; API key for authentication.
#' @param use_dev       Logical; whether to use the development base URL.
#' @param object_label  Character; short label used in CLI messages (e.g., "Distribution").
#'
#' @return The parsed API response (invisibly), or NULL if the update failed.
update_request <- function(object, endpoint, api_key, use_dev = TRUE, object_label) {
  payload <- object_to_payload(object)

  result <- tryCatch(
    {
      result <- api_request("PATCH", endpoint, payload, api_key, use_dev)

      # Success message
      if (identical(object_label, "Distribution")) {
        parent <- result$dataset$id
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {result$title}} (ID {.val {result$id}}) updated inside Dataset {.val {parent}}."
        )
      } else {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {result$title}} (ID {.val {result$id}}) successfully updated."
        )
      }

      result
    },
    error = function(e) {
      if (inherits(e, "httr2_http_error")) {
        code <- e$response$status_code

        cli::cli_alert_danger(
          "{.strong {object_label}} (ID {.val {object@id}}) update failed ({code}): {e$message}"
        )
      } else {
        cli::cli_alert_danger(
          "{.strong {object_label}} (ID {.val {object@id}}) update failed: {e$message}"
        )
      }

      NULL
    }
  )

  invisible(result)
}
