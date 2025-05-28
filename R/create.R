#' @describeIn create a new Dataset via API call
S7::method(create, Dataset) <- function(object, api_key, use_dev = TRUE) {
  post_request(object, "/api/v1/datasets", api_key, use_dev, object_label = "Dataset")
}

#' @describeIn create a new Distribution via API call
S7::method(create, Distribution) <- function(object, api_key, use_dev = TRUE) {
  post_request(object, "/api/v1/distributions", api_key, use_dev, object_label = "Distribution")
}

#' Perform an API POST request to create a metadata object.
#'
#' This function sends a creation request to the API for the given object
#' (e.g., a Dataset or Distribution). It handles both success and failure feedback via CLI.
#'
#' @param object        An S7 metadata object to create.
#' @param endpoint      Character; the API endpoint for the object (e.g., "/api/v1/distributions").
#' @param api_key       Character; API key for authentication.
#' @param use_dev       Logical; whether to use the development base URL.
#' @param object_label  Character; short label used in CLI messages (e.g., "Distribution").
#'
#' @return The parsed API response (invisibly), or NULL if the creation failed.
post_request <- function(object, endpoint, api_key, use_dev = TRUE, object_label) {
  payload <- object_to_payload(object)

  result <- tryCatch(
    {
      result <- api_request("POST", endpoint, payload, api_key, use_dev)

      # Success message
      if (identical(endpoint, "/api/v1/distributions")) {
        parent_id <- result$dataset$id
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {result$title}} with ID {.val {result$id}} created inside Dataset with ID {.val {parent_id}}."
        )
      } else {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {result$title}} created with ID {.val {result$id}}."
        )
      }

      result
    },
    error = function(e) {
      if (inherits(e, "httr2_http_error")) {
        code <- e$response$status_code

        cli::cli_alert_danger(
          "{.strong {object_label}} creation failed ({code}): {e$message}"
        )
      } else {
        cli::cli_alert_danger(
          "{.strong {object_label}} creation failed: {e$message}"
        )
      }

      NULL
    }
  )

  invisible(result)
}
