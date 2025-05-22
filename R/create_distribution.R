#' Create a new distribution via API
#'
#' @param title             Title of the distribution (required, <= 1000 chars)
#' @param dataset_id        ID of the related dataset (required integer)
#' @param stat_server_flag  Logical; publish to statistical server (optional)
#' @param zh_web_flag       Logical; publish to zh web catalog (optional)
#' @param ogd_flag          Logical; publish to Open Government Data portal (optional)
#' @param sort_order        Numeric; optional sorting index
#' @param description       Optional description string
#' @param modified          Optional ISO datetime string or POSIXct
#' @param access_url        Optional access URL (must start with http:// or https://)
#' @param identifier        Optional string identifier
#' @param right             Optional rights statement
#' @param issued            Optional ISO datetime string or POSIXct
#' @param byte_size         Optional numeric byte size (positive integer)
#' @param status_id         Optional status ID (default: 1)
#' @param license_id        Optional license ID
#' @param format_id         Optional format ID
#' @param media_type_id     Optional media type ID
#' @param periodicity_id    Optional periodicity ID
#' @param file_upload_id    Optional file upload ID (string)
#' @param api_key           API key (optional; falls back to env var)
#' @param use_dev           Logical; use development base URL
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
    modified         = NULL,
    access_url       = NULL,
    identifier       = NULL,
    right            = NULL,
    issued           = NULL,
    byte_size        = NULL,
    status_id        = 1,
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

  # Construct Distribution object
  dist <- Distribution(
    title             = title,
    dataset_id        = dataset_id,
    stat_server_flag  = if (is.null(stat_server_flag)) NA else stat_server_flag,
    zh_web_flag       = if (is.null(zh_web_flag)) NA else zh_web_flag,
    ogd_flag          = if (is.null(ogd_flag)) NA else ogd_flag,
    sort_order        = if (is.null(sort_order)) NA_real_ else sort_order,
    description       = if (is.null(description)) NA_character_ else description,
    modified          = if (!is.null(modified)) as.POSIXct(modified, tz = "UTC") else as.POSIXct(NA),
    access_url        = if (is.null(access_url)) NA_character_ else access_url,
    identifier        = if (is.null(identifier)) NA_character_ else identifier,
    right             = if (is.null(right)) NA_character_ else right,
    issued            = if (!is.null(issued)) as.POSIXct(issued, tz = "UTC") else as.POSIXct(NA),
    byte_size         = if (is.null(byte_size)) NA_real_ else byte_size,
    status_id         = if (is.null(status_id)) NA_real_ else status_id,
    license_id        = if (is.null(license_id)) NA_real_ else license_id,
    format_id         = if (is.null(format_id)) NA_real_ else format_id,
    media_type_id     = if (is.null(media_type_id)) NA_real_ else media_type_id,
    periodicity_id    = if (is.null(periodicity_id)) NA_real_ else periodicity_id,
    file_upload_id    = if (is.null(file_upload_id)) NA_character_ else file_upload_id
  )

  # Dispatch to API
  create(dist, api_key, use_dev)
}
