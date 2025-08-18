#' Create a new distribution via API
#'
#' @param title             Title of the distribution (required; max. 1000 characters).
#' @param dataset_id        ID of the dataset to which this distribution belongs (required).
#' @param stat_server_flag  Logical; should the distribution be published on the statistical server? (optional).
#' @param zh_web_flag       Logical; should the distribution be shown on the Zurich web catalog? (optional).
#' @param ogd_flag          Logical; should the distribution be published on the OGD portal? (optional).
#' @param description       Optional free-text description.
#' @param access_url        Optional URL to access the distribution (must start with http:// or https://).
#' @param byte_size         Optional file size in bytes (must be a positive number).
#' @param status_id         Optional character; status ID (will be set via follow-up PATCH).
#' @param license_id        Optional integer; license ID.
#' @param file_format_id    Optional file format ID.
#' @param periodicity_id    Optional character periodicity ID.
#' @param file_path         Optional local file path; if provided, the file will be uploaded and linked.
#' @param start_date        POSIXct or ISO datetime string; new start (optional)
#' @param end_date          POSIXct or ISO datetime string; new end (optional)
#' @param api_key           Optional API key; if not provided, the default environment variable is used.
#' @param verbosity         Integer; verbosity level passed to httr2::req_perform() (default: 0).
#' @param use_dev           Logical; whether to use the development API endpoint (default: TRUE).
#' @param preview           Defines if it is a test run. If TRUE, the
#'                          dataset-object is returned into the environment.
#'                          Default = FALSE
#'
#' @details
#' If `file_path` is provided, the file will be uploaded via a separate API call prior to creating the distribution.
#' The resulting `file_upload_id` and `file_format_id` will be automatically extracted and included in the distribution payload.
#'
#' If `status_id` is specified, it will be applied via a follow-up PATCH request after the distribution is created,
#' as the initial POST request does not accept a custom status.
#'
#' If `start_date` or `end_date` are provided, the dataset's overall timespan will be updated via `update_dataset()`.
#'
#' To assist in constructing valid queries, the following functions provide the
#' set of acceptable values:
#'
#' - `get_statuses()`
#' - `get_licenses()`
#' - `get_formats()`
#' - `get_periodicities()`
#'
#' Use these functions to inspect the available values before making a query.
#'
#' @return Invisibly returns the parsed API response as a list.
#' @export
create_distribution <- function(
    title,
    dataset_id,
    stat_server_flag = NULL,
    zh_web_flag      = NULL,
    ogd_flag         = NULL,
    description      = NULL,
    access_url       = NULL,
    byte_size        = NULL,
    status_id        = NULL,
    license_id       = NULL,
    file_format_id   = NULL,
    periodicity_id   = NULL,
    file_path        = NULL,
    start_date       = NULL,
    end_date         = NULL,
    api_key          = NULL,
    verbosity        = 0,
    use_dev          = TRUE,
    preview          = FALSE
) {
  # Retrieve API key
  api_key <- get_api_key(api_key)

  # Validate required title field
  if (is.null(title) || is.na(title) || nzchar(title) == FALSE) {
    stop("`title` ist erforderlich, um eine neue Distribution zu erstellen.", call. = FALSE)
  }

  # Validate required dataset ID
  if (is.null(dataset_id) || is.na(dataset_id)) {
    stop("`dataset_id` ist erforderlich, um eine neue Distribution zu erstellen.", call. = FALSE)
  }

  # If a file_path is provided, upload the file and extract IDs
  if (!is.null(file_path) && preview == FALSE) {
    f <- create_file(file_path, api_key = api_key, use_dev = use_dev)
    file_upload_id <- f$id
    file_format_id <- f$file_format$id
  } else {
    # file_upload_id not available --> set to NULL
    file_upload_id <- NULL
  }

  # Resolve all lookups up front
  status_id_api      <- if (is.character(status_id))      convert_statuses_to_id(status_id)            else status_id
  license_id_api     <- if (is.character(license_id))     convert_licenses_to_id(license_id)           else license_id
  file_format_id_api <- if (is.character(file_format_id)) convert_formats_to_id(file_format_id)        else file_format_id
  periodicity_id_api <- if (is.character(periodicity_id)) convert_periodicities_to_id(periodicity_id)  else periodicity_id

  # Build pure Distribution (no HTTP here)
  dist <- Distribution(
    title            = title,
    dataset_id       = dataset_id,
    stat_server_flag = stat_server_flag,
    zh_web_flag      = zh_web_flag,
    ogd_flag         = ogd_flag,
    description      = description,
    access_url       = access_url,
    byte_size        = byte_size,
    file_upload_id   = file_upload_id,
    license_id       = license_id_api,
    file_format_id   = file_format_id_api,
    periodicity_id   = periodicity_id_api
  )

  # return early in a test run (preview = TRUE)
  if (preview) {
    return(dist)
  }

  # Dispatch POST
  result <- create(dist, api_key, use_dev, verbosity = verbosity)
  # If a custom status was requested, apply it via follow-up PATCH
  if (!is.null(result) && !is.null(status_id_api)) {
    set_status(
      Distribution(id = result$id, status_id = status_id_api),
      api_key   = api_key,
      use_dev   = use_dev,
      verbosity = verbosity
    )
  }

  # If a new start / end date is supplied, apply it via follow-up PATCH to the Dataset
  if (!is.null(start_date) || !is.null(end_date)) {
    update_dataset(
      id         = dataset_id,
      start_date = start_date,
      end_date   = end_date,
      api_key    = api_key,
      use_dev    = use_dev,
      verbosity  = verbosity
    )
  }


  invisible(result)
}

