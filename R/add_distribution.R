#' Add a distribution to an existing dataset
#'
#' @description
#' Adds a new distribution (file or access point) to an existing dataset.
#' Optionally uploads a file to the distribution.
#'
#' @param auth_info Authentication information from login_to_api()
#' @param dataset_id ID of the dataset to add the distribution to
#' @param title Title of the distribution
#' @param description Description of the distribution
#' @param file_path Optional path to a file to upload (if provided, the file will be uploaded)
#' @param access_url_id URL where the distribution can be accessed
#' @param download_url_id URL where the distribution can be downloaded
#' @param format_id ID of the format (e.g., CSV, XML)
#' @param media_type_id ID of the media type
#' @param use_dev Whether to use the development environment (default: TRUE)
#' @param ... Additional distribution properties
#'
#' @return The created distribution information as returned by the API
#' @export
#'
#' @examples
#' \dontrun{
#' # First login
#' auth_info <- login_to_api("admin@example.com", "admin")
#'
#' # Add a distribution to dataset with ID 123
#' add_distribution(
#'   auth_info,
#'   dataset_id = 123,
#'   title = "Air Quality CSV Data",
#'   description = "CSV file containing hourly air quality measurements"
#' )
#'
#' # Add a distribution with a file upload
#' add_distribution(
#'   auth_info,
#'   dataset_id = 123,
#'   title = "Air Quality CSV Data",
#'   description = "CSV file containing hourly air quality measurements",
#'   file_path = "path/to/your/data.csv",
#'   format_id = 1,  # Assuming 1 is the ID for CSV format
#'   media_type_id = 2  # Assuming 2 is the ID for text/csv media type
#' )
#' }
add_distribution <- function(auth_info,
                             dataset_id,
                             title,
                             description,
                             file_path = NULL,
                             access_url_id = NULL,
                             download_url_id = NULL,
                             format_id = NULL,
                             media_type_id = NULL,
                             use_dev = TRUE,
                             ...) {
  # Create the Distribution object
  distribution <- Distribution(
    title = title,
    description = description,
    dataset_id = as.integer(dataset_id),
    access_url_id = access_url_id,
    download_url_id = download_url_id,
    format_id = if(!is.null(format_id)) as.integer(format_id) else NULL,
    media_type_id = if(!is.null(media_type_id)) as.integer(media_type_id) else NULL,
    ...
  )

  # Create distribution in catalog
  result <- create(distribution, auth_info, use_dev)

  # If a file path was provided, upload the file
  if (!is.null(file_path) && file.exists(file_path)) {
    upload_result <- upload_file(
      distribution_id = result$id,
      file_path = file_path,
      auth_info = auth_info,
      use_dev = use_dev
    )

    # Update the result with the upload response
    result$upload_result <- upload_result
  }

  return(result)
}
