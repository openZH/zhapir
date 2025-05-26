#' Update an existing dataset via API
#'
#' @description
#' Update fields of an existing Dataset in the MDV data catalog. Only the `id`
#' is required; all other fields are optional and will be included if non-NULL.
#'
#' @param id                numeric; the dataset ID to update (required)
#' @param title             character; new title (optional)
#' @param organisation_id   numeric; new organisation ID (optional)
#' @param description       character; new description (optional)
#' @param contact_email     character; new contact email (optional)
#' @param landing_page      character; new landing page URL (optional)
#' @param issued            POSIXct or ISO datetime string; new publication date (optional)
#' @param start_date        POSIXct or ISO datetime string; new start of timeseries (optional)
#' @param end_date          POSIXct or ISO datetime string; new end of timeseries (optional)
#' @param modified_next     POSIXct or ISO datetime string; next modification timestamp (optional)
#' @param keyword_ids       integer vector; new keyword IDs (optional)
#' @param zh_web_catalog_ids integer vector; new web catalog IDs (optional)
#' @param relation_ids      integer vector; new relation IDs (optional)
#' @param theme_ids         integer vector; new theme IDs (optional)
#' @param periodicity_id    numeric; new periodicity ID (optional)
#' @param see_also_ids      integer vector; new see-also IDs (optional)
#' @param api_key           API key (optional; falls back to env var)
#' @param use_dev           Logical; use development base URL (default TRUE)
#'
#' @return Invisibly returns the parsed API response (named list) on success.
#' @export
update_dataset <- function(
    id,
    title             = NULL,
    organisation_id   = NULL,
    description       = NULL,
    contact_email     = NULL,
    landing_page      = NULL,
    issued            = NULL,
    start_date        = NULL,
    end_date          = NULL,
    modified_next     = NULL,
    keyword_ids       = NULL,
    zh_web_catalog_ids= NULL,
    relation_ids      = NULL,
    theme_ids         = NULL,
    periodicity_id    = NULL,
    see_also_ids      = NULL,
    api_key           = NULL,
    use_dev           = TRUE
) {

  # API key
  api_key <- get_api_key(api_key)

  # Ensure id is provided
  if (missing(id) || is.na(id)) {
    stop("`id` is required to update a dataset.", call. = FALSE)
  }

  # If organisation_id not supplied, fetch existing dataset to get it
  if (is.null(organisation_id)) {
    existing <- get_dataset(id, api_key = api_key, use_dev = use_dev)
    organisation_id <- existing$organisation$id
  }


  # Build S7 Dataset object preserving required fields
  ds <- Dataset(
    id                 = id,
    title              = if (is.null(title))    NA_character_ else title,
    organisation_id    = organisation_id,
    description        = if (is.null(description))      NA_character_ else description,
    contact_email      = if (is.null(contact_email))    NA_character_ else contact_email,
    landing_page       = if (is.null(landing_page))     NA_character_ else landing_page,
    issued             = if (!is.null(issued))          as.POSIXct(issued, tz = "UTC") else as.POSIXct(NA),
    start_date         = if (!is.null(start_date))      as.POSIXct(start_date, tz = "UTC") else as.POSIXct(NA),
    end_date           = if (!is.null(end_date))        as.POSIXct(end_date, tz = "UTC") else as.POSIXct(NA),
    modified_next      = if (!is.null(modified_next))   as.POSIXct(modified_next, tz = "UTC") else as.POSIXct(NA),
    keyword_ids        = if (is.null(keyword_ids))       list() else keyword_ids,
    zh_web_catalog_ids = if (is.null(zh_web_catalog_ids)) list() else zh_web_catalog_ids,
    relation_ids       = if (is.null(relation_ids))      list() else relation_ids,
    theme_ids          = if (is.null(theme_ids))         list() else theme_ids,
    periodicity_id     = if (is.null(periodicity_id))    NA_real_ else periodicity_id,
    see_also_ids       = if (is.null(see_also_ids))      list() else see_also_ids
  )

  # Dispatch the update method
  update(ds, api_key = api_key, use_dev = use_dev)
}
