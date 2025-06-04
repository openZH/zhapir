#' Create a new distribution via API
#'
#' @param title             Title of the distribution (required, <= 1000 chars)
#' @param dataset_id        ID of the related dataset (required integer)
#' @param stat_server_flag  Logical; publish to statistical server (optional)
#' @param zh_web_flag       Logical; publish to zh web catalog (optional)
#' @param ogd_flag          Logical; publish to Open Government Data portal (optional)
#' @param sort_order        Numeric; optional sorting index
#' @param description       Optional description string
#' @param access_url        Optional access URL (must start with http:// or https://)
#' @param right             Optional rights statement
#' @param byte_size         Optional numeric byte size (positive integer)
#' @param status_id         Optional status ID (will be set via follow-up PATCH)
#' @param license_id        Optional license ID
#' @param format_id         Optional format ID
#' @param media_type_id     Optional media type ID
#' @param periodicity_id    Optional periodicity ID
#' @param file_upload_id    Optional file upload ID (string)
#' @param api_key           API key (optional; falls back to env var)
#' @param use_dev           Logical; use development base URL
#'
#' @details If `status_id` is provided, it will be applied via a follow-up API PATCH request after creation.
#'
#' @return Invisibly returns the parsed API response
#' @export
create_distribution <- function(
    title,
    dataset_id,
    stat_server_flag = NULL,
    zh_web_flag      = NULL,
    ogd_flag         = NULL,
    sort_order       = NULL,
    description      = NULL,
    access_url       = NULL,
    right            = NULL,
    byte_size        = NULL,
    status_id        = NULL,
    license_id       = NULL,
    format_id        = NULL,
    media_type_id    = NULL,
    periodicity_id   = NULL,
    file_upload_id   = NULL,
    api_key          = NULL,
    use_dev          = TRUE
) {
  # Extract or prompt for API key
  api_key <- get_api_key(api_key)

  # Title is required
  if (is.null(title) || is.na(title) || nzchar(title) == FALSE) {
    stop("`title` ist erforderlich, um eine neue Distribution zu erstellen.", call. = FALSE)
  }

  # Dataset is required
  if (is.null(dataset_id) || is.na(dataset_id)) {
    stop("`dataset_id` ist erforderlich für das Erstellen einer Distribution.", call. = FALSE)
  }

  # Capture arguments of function call and construct a Distribution-Object
  args <- as.list(match.call())
  args <- args[2:length(args)]
  args <- args[!grepl("api_key|use_dev", names(args))]

  dist <- do.call(Distribution, args)

  # Dispatch to API
  result <- create(dist, api_key, use_dev)

  # If a status_id is provided, update it via PATCH
  # (status defaults to status_id = 1 on creation and cannot be set via PATCH)
  if (!is.null(result) && !is.null(status_id)) {
    update(
      Distribution(id = result$id, status_id = status_id),
      api_key = api_key,
      use_dev = use_dev
    )
  }

  invisible(result)
}
