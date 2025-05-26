#' Update an existing distribution via API
#'
#' @description
#' Update fields of an existing Distribution in the MDV data catalog.
#' Only the `id` is required; all other fields are optional and will be included if non-NULL.
#'
#' @param id                numeric; the distribution ID to update (required)
#' @param title             character; new title (optional)
#' @param description       character; new description (optional)
#' @param stat_server_flag  logical; publish to statistical server (optional)
#' @param zh_web_flag       logical; publish to zh web catalog (optional)
#' @param ogd_flag          logical; publish to OGD portal (optional)
#' @param sort_order        numeric; optional sorting index
#' @param modified          POSIXct or ISO datetime string; last modified timestamp
#' @param access_url        character; access URL (optional)
#' @param right             character; rights statement (optional)
#' @param issued            POSIXct or ISO datetime string; publication timestamp
#' @param byte_size         numeric; size in bytes (optional)
#' @param license_id        numeric; license ID (optional)
#' @param format_id         numeric; file format ID (optional)
#' @param media_type_id     numeric; media type ID (optional)
#' @param periodicity_id    numeric; periodicity ID (optional)
#' @param file_upload_id    character; file upload UUID (optional)
#' @param api_key           API key (optional; falls back to env var)
#' @param use_dev           Logical; use development base URL
#'
#' @return Invisibly returns the parsed API response (named list) on success.
#' @export
update_distribution <- function(
    id,
    title             = NULL,
    description       = NULL,
    stat_server_flag  = NULL,
    zh_web_flag       = NULL,
    ogd_flag          = NULL,
    sort_order        = NULL,
    modified          = NULL,
    access_url        = NULL,
    right             = NULL,
    issued            = NULL,
    byte_size         = NULL,
    license_id        = NULL,
    format_id         = NULL,
    media_type_id     = NULL,
    periodicity_id    = NULL,
    file_upload_id    = NULL,
    api_key           = NULL,
    use_dev           = TRUE
) {
  # API key
  api_key <- get_api_key(api_key)

  # Require ID
  if (missing(id) || is.na(id)) {
    stop("`id` is required to update a distribution.", call. = FALSE)
  }

  # Create Distribution object
  dist <- Distribution(
    id               = id,
    title            = if (is.null(title)) NA_character_ else title,
    description      = if (is.null(description)) NA_character_ else description,
    stat_server_flag = if (is.null(stat_server_flag)) NA else stat_server_flag,
    zh_web_flag      = if (is.null(zh_web_flag)) NA else zh_web_flag,
    ogd_flag         = if (is.null(ogd_flag)) NA else ogd_flag,
    sort_order       = if (is.null(sort_order)) NA_real_ else sort_order,
    modified         = if (!is.null(modified)) as.POSIXct(modified, tz = "UTC") else as.POSIXct(NA),
    access_url       = if (is.null(access_url)) NA_character_ else access_url,
    right            = if (is.null(right)) NA_character_ else right,
    issued           = if (!is.null(issued)) as.POSIXct(issued, tz = "UTC") else as.POSIXct(NA),
    byte_size        = if (is.null(byte_size)) NA_real_ else byte_size,
    license_id       = if (is.null(license_id)) NA_real_ else license_id,
    format_id        = if (is.null(format_id)) NA_real_ else format_id,
    media_type_id    = if (is.null(media_type_id)) NA_real_ else media_type_id,
    periodicity_id   = if (is.null(periodicity_id)) NA_real_ else periodicity_id,
    file_upload_id   = if (is.null(file_upload_id)) NA_character_ else file_upload_id
  )

  # Dispatch
  update(dist, api_key = api_key, use_dev = use_dev)
}
