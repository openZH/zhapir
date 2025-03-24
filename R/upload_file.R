#' Upload a file to a distribution
#'
#' @description
#' Uploads a file to an existing distribution in the catalog.
#'
#' @param distribution_id ID of the distribution to upload the file to
#' @param file_path Path to the file to upload
#' @param auth_info Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment (default: TRUE)
#'
#' @return The response from the API after uploading the file
#' @keywords internal
upload_file <- function(distribution_id, file_path, auth_info, use_dev = TRUE) {
  base_url <- get_base_url(use_dev)

  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Make the API request
  response <- httr2::request(paste0(base_url, "/api/v1/distributions/", distribution_id, "/upload")) |>
    httr2::req_headers(
      "Accept" = "application/json, application/problem+json",
      "Cookie" = auth_info$cookie,
      "Language" = "de"  # Default language, could be made configurable
    ) |>
    httr2::req_body_multipart(
      file = curl::form_file(file_path)
    ) |>
    httr2::req_method("POST") |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300) {
    resp_data <- httr2::resp_body_json(response)
    return(resp_data)
  } else {
    stop("API request failed with status ", httr2::resp_status(response),
         ": ", httr2::resp_body_string(response))
  }
}
