#' Create a new Dataset via the MDV API
#'
#' S7 method for creating Dataset objects. This function serializes the
#' Dataset to JSON, posts it to the API, and reports success via a CLI message.
#'
#' @param object  An S7 Dataset object (built by `create_dataset()`).
#' @param api_key MDV API key (character).
#' @param use_dev Logical; if TRUE, uses the development API endpoint.
#'
#' @return Invisibly returns the parsed API response (a named list) on success.
#' @name create.Dataset
#' @rdname create.Dataset
#' @keywords internal
S7::method(create, Dataset) <- function(object, api_key, use_dev = TRUE) {
  # Determine base URL
  base_url <- get_base_url(use_dev)

  # Convert object to JSON-ready list
  payload <- object_to_payload(object)

  # Perform API request
  resp <- httr2::request(paste0(base_url, "/api/v1/datasets")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      Accept         = "application/json, application/problem+json",
      `x-api-key`    = api_key
    ) |>
    httr2::req_body_json(payload, null = "null") |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status < 300) {
    result <- httr2::resp_body_json(resp)
    # Inform user
    cli::cli_alert_success("Dataset {.val {result$title}} created with ID {.val {result$id}}.")
    return(invisible(result))
  }

  # On failure, raise an error
  stop(
    sprintf(
      "Dataset creation failed [%s]: %s",
      status,
      httr2::resp_body_string(resp)
    ),
    call. = FALSE
  )
}

