#' Upload a file via API
#'
#' Creates a new `FileUpload` object and uploads the file via the API.
#'
#' @param file_path Local file path to upload (required).
#' @param api_key   Optional API key. Falls back to environment if NULL.
#' @param use_dev   Logical; whether to use development environment (default: TRUE).
#'
#' @return Invisibly returns the parsed API response (including `file_upload_id`).
#' @export
create_file <- function(
    file_path,
    api_key = NULL,
    use_dev = TRUE
) {
  # Ensure API key is present
  api_key <- get_api_key(api_key)

  # Validate input
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("`file_path` must point to an existing file.", call. = FALSE)
  }

  # Create FileUpload object
  file_obj <- FileUpload(file_path = file_path)

  # Send to API
  result <- create(file_obj, api_key, use_dev)

  invisible(result)
}
