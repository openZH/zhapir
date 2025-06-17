#' Update an existing distribution via API
#'
#' @param id                ID of the distribution to update (required).
#' @param title             Title of the distribution (optional; max. 1000 characters).
#' @param dataset_id        ID of the dataset to which this distribution belongs (optional).
#' @param stat_server_flag  Logical; should the distribution be published on the statistical server? (optional).
#' @param zh_web_flag       Logical; should the distribution be shown on the Zurich web catalog? (optional).
#' @param ogd_flag          Logical; should the distribution be published on the OGD portal? (optional).
#' @param sort_order        Integer; optional custom sorting index.
#' @param description       Optional free-text description.
#' @param access_url        Optional URL to access the distribution (must start with http:// or https://).
#' @param right             Optional usage rights description.
#' @param byte_size         Optional file size in bytes (must be a positive number).
#' @param status_id         Optional status ID (applied via PATCH after update).
#' @param license_id        Optional license ID.
#' @param format_id         Optional file format ID.
#' @param media_type_id     Optional media type ID.
#' @param periodicity_id    Optional update frequency ID.
#' @param file_path         Optional local file path; if provided, the file will be uploaded and linked.
#' @param file_upload_id    Optional ID of a previously uploaded file (overridden if file_path is used).
#' @param api_key           Optional API key; if not provided, the default environment variable is used.
#' @param use_dev           Logical; whether to use the development API endpoint (default: TRUE).
#'
#' @details
#' If `file_path` is provided, the file will be uploaded via a separate API call prior to updating the distribution.
#' The resulting `file_upload_id`, `format_id`, and `media_type_id` will be automatically extracted and included in the update payload.
#'
#' If `status_id` is specified, it will be applied via a separate PATCH request after the distribution is updated.
#'
#' @return Invisibly returns the parsed API response as a list.
#' @export

update_distribution <- function(
    id,
    title             = NULL,
    description       = NULL,
    stat_server_flag  = NULL,
    zh_web_flag       = NULL,
    ogd_flag          = NULL,
    sort_order        = NULL,
    access_url        = NULL,
    right             = NULL,
    byte_size         = NULL,
    status_id         = NULL,
    license_id        = NULL,
    format_id         = NULL,
    dataset_id        = NULL,
    media_type_id     = NULL,
    periodicity_id    = NULL,
    file_path         = NULL,
    file_upload_id    = NULL,
    api_key           = NULL,
    use_dev           = TRUE
) {
  # Retrieve API key
  api_key <- get_api_key(api_key)

  # Validate require ID
  if (missing(id) || is.na(id)) {
    stop("`id` is required to update a distribution.", call. = FALSE)
  }

  # Extract arguments and prepare for Distribution object creation
  args <- as.list(match.call())
  args <- args[2:length(args)]
  args <- args[!grepl("api_key|use_dev", names(args))]

  # If a file_path is provided, upload the file and extract IDs
  if (!is.null(file_path)) {
    file_result <- create_file(args$file_path, api_key = api_key, use_dev = use_dev)

    # Add linked file information to distribution
    args$file_upload_id <- file_result$id
    args$format_id <- file_result$file_format$id
    args$media_type_id <- file_result$media_type$id

    # file_path is not part of the Distribution class
    args$file_path <- NULL
  }

  # Create the Distribution object from arguments
  dist <- do.call(Distribution, args)


  # Send POST request to create distribution
  update(dist, api_key = api_key, use_dev = use_dev)
}
