#' Upload a local file via API
#'
#' @description
#' Uploads a file to the server by creating a `FileUpload` object and sending it to the API.
#'
#' @param file_path Path to a local file to be uploaded (must exist).
#' @param api_key   Optional API key. If not provided, the function attempts to retrieve it from the environment.
#' @param use_dev   Logical; whether to use the development API environment (default: `TRUE`).
#' @param verbosity Integer; verbosity level passed to httr2::req_perform() (default: 0).
#'
#' @return Invisibly returns the parsed API response as a list, including the `file_upload_id`.
#' @export
create_file <- function(
    file_path,
    api_key = NULL,
    use_dev = TRUE,
    verbosity = 0
) {
  # Retrieve API key
  api_key <- get_api_key(api_key)

  # Validate input
  if (is.null(file_path) || !file.exists(file_path)) {
    stop("`file_path` must point to an existing file.", call. = FALSE)
  }

  # Construct FileUpload object
  file_obj <- FileUpload(file_path = file_path)

  # Dispatch upload request to the API
  result <- create(file_obj, api_key, use_dev, verbosity = verbosity)

  invisible(result)
}
