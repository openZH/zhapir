#' Create a Dataset via the MDV API
#'
#' This S7 method takes an in-memory `Dataset` object, converts it to a JSON payload,
#' and issues a POST to `/api/v1/datasets`. It handles serialization of dates,
#' omission of empty fields, and error checking.
#'
#' @param object   An S7 `Dataset` object (usually built by `create_dataset()`).
#' @param api_key  A valid MDV API key, e.g. from `get_api_key()` or the `MDV_API_KEY` environment variable.
#'
#' @param use_dev  Logical; when `TRUE`, uses the development API endpoint.
#'
#' @keywords internal
S7::method(create, Dataset) <- function(object, api_key, use_dev = TRUE) {

  # Determine base URL
  base_url <- get_base_url(use_dev)

  payload  <- object_to_payload(object)

  resp <- httr2::request(paste0(base_url, "/api/v1/datasets")) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      Accept         = "application/json, application/problem+json",
      `x-api-key`    = api_key
    ) |>
    httr2::req_body_json(payload, null = "null") |>
    httr2::req_perform()

  if (httr2::resp_status(resp) < 300) {
    httr2::resp_body_json(resp)
  } else {
    stop("API request failed: ", httr2::resp_body_string(resp), call. = FALSE)
  }
}
