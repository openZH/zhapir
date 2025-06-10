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
#' @param access_url        character; access URL (optional)
#' @param right             character; rights statement (optional)
#' @param byte_size         numeric; size in bytes (optional)
#' @param status_id         numeric; status ID (optional)
#' @param license_id        numeric; license ID (optional)
#' @param dataset_id        numeric; dataset ID (optional)
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
    access_url        = NULL,
    right             = NULL,
    byte_size         = NULL,
    status_id         = NULL,
    license_id        = NULL,
    format_id         = NULL,
    dataset_id        = NULL,
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

  # Build S7 Dataset object preserving required fields
  args <- as.list(match.call())
  args <- args[2:length(args)]
  args <- args[!grepl("api_key|use_dev", names(args))]

  dist <- do.call(Distribution, args)


  # Dispatch
  update(dist, api_key = api_key, use_dev = use_dev)
}
