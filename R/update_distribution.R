#' Update an existing distribution via API
#'
#' @param id                ID of the distribution to update (required).
#' @param title             Title of the distribution (optional; max. 1000 characters).
#' @param dataset_id        ID of the dataset to which this distribution belongs (optional).
#' @param stat_server_flag  Logical; should the distribution be published on the statistical server? (optional).
#' @param zh_web_flag       Logical; should the distribution be shown on the Zurich web catalog? (optional).
#' @param ogd_flag          Logical; should the distribution be published on the OGD portal? (optional).
#' @param description       Optional free-text description.
#' @param access_url        Optional URL to access the distribution (must start with http:// or https://).
#' @param byte_size         Optional file size in bytes (must be a positive number).
#' @param status_id         Optional status ID (applied via PATCH after update).
#' @param license_id        Optional license ID.
#' Use `1` = "CC BY 4.0 (Attribution required)" or `2` = "CC0 (No attribution required)".
#' @param file_format_id    Optional file format ID.
#' @param periodicity_id    Optional update frequency ID.
#' @param file_path         Optional local file path; if provided, the file will be uploaded and linked.
#' @param start_date        POSIXct or ISO datetime string; new start of Dataset(optional)
#' @param end_date          POSIXct or ISO datetime string; new end of Dataset (optional)
#' @param modified_next     POSIXct or ISO datetime string; new next update of Dataset (optional)
#' @param api_key           Optional API key; if not provided, the default environment variable is used.
#' @param verbosity         Integer; verbosity level passed to httr2::req_perform() (default: 0).
#' @param use_dev           Logical; whether to use the development API endpoint (default: TRUE).
#' @param preview           Defines if it is a test run. If TRUE, the
#'                          dataset-object is returned into the environment.
#'                          Default = FALSE
#'
#' @details
#' If `file_path` is provided, the file will be uploaded via a separate API call prior to updating the distribution.
#' The resulting `file_upload_id` and `file_format_id` will be automatically extracted and included in the update payload.
#'
#' If `status_id` is specified, it will be applied via a separate PATCH request after the distribution is updated.
#'
#' If `start_date` or `end_date` are provided, the dataset's overall timespan will be updated via `update_dataset()`.
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
    access_url        = NULL,
    byte_size         = NULL,
    status_id         = NULL,
    license_id        = NULL,
    file_format_id    = NULL,
    dataset_id        = NULL,
    periodicity_id    = NULL,
    file_path         = NULL,
    start_date        = NULL,
    end_date          = NULL,
    modified_next     = NULL,
    api_key           = NULL,
    verbosity         = 0,
    use_dev           = TRUE,
    preview           = FALSE
) {
  # Retrieve API key
  api_key <- get_api_key(api_key)

  # Validate require ID
  if (missing(id) || is.na(id)) {
    stop("`id` is required to update a distribution.", call. = FALSE)
  }

  if (!is.null(file_path) && preview == FALSE) {
    f <- create_file(file_path, api_key = api_key, use_dev = use_dev)
    file_upload_id <- f$id
    file_format_id <- f$file_format$id
  }else {
    # file_upload_id not available --> set to NULL
    file_upload_id <- NULL
  }

  status_id_api      <- if (is.character(status_id))      convert_statuses_to_id(status_id)            else status_id
  license_id_api     <- if (is.character(license_id))     convert_licenses_to_id(license_id)           else license_id
  file_format_id_api <- if (is.character(file_format_id)) convert_formats_to_id(file_format_id)   else file_format_id
  periodicity_id_api <- if (is.character(periodicity_id)) convert_periodicities_to_id(periodicity_id)  else periodicity_id


  # Create the Distribution object from arguments
  dist <- Distribution(
    id               = id,
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

  # Send POST request to update distribution
  result <- update(dist, api_key = api_key, use_dev = use_dev, verbosity = verbosity)

  if (!is.null(result) && !is.null(status_id_api)) {
    set_status(
      Distribution(id = result$id, status_id = status_id_api),
      api_key   = api_key,
      use_dev   = use_dev,
      verbosity = verbosity
    )
  }
  # If a new start / end date / modified next is supplied, apply it via follow-up PATCH to the Dataset
  if (!is.null(start_date) || !is.null(end_date) || !is.null(modified_next)) {
    update_dataset(
      id         = result$dataset$id,
      start_date = start_date,
      end_date   = end_date,
      modified_next = modified_next,
      api_key    = api_key,
      use_dev    = use_dev,
      verbosity  = verbosity
    )
  }


  invisible(result)
}
