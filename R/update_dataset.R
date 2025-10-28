#' Update an existing dataset via API
#'
#' @description
#' Update fields of an existing Dataset in the MDV data catalog. Only the `id`
#' is required; all other fields are optional and will be included if non-NULL.
#'
#' @param id                     numeric; the dataset ID to update (required)
#' @param title                  character; new title (optional)
#' @param organisation_id        numeric; new organisation ID (optional)
#' @param description            character; new description (optional)
#' @param contact_email          character; new contact email (optional)
#' @param landing_page           character; new landing page URL (optional)
#' @param start_date             POSIXct or ISO datetime string; new start (optional)
#' @param end_date               POSIXct or ISO datetime string; new end (optional)
#' @param modified_next          POSIXct or ISO datetime string; next mod timestamp (optional)
#' @param keyword_ids            integer vector; new keyword IDs (optional)
#' @param zh_web_datacatalog_ids integer vector; new web catalog IDs (optional)
#' @param relation_ids           integer vector; new relation IDs (optional)
#' @param theme_ids              integer vector; new theme IDs (optional)
#' @param periodicity_id         numeric; new periodicity ID (optional)
#' @param see_also_ids           integer vector; new see-also IDs (optional)
#' @param api_key                API key (optional; falls back to env var)
#' @param use_dev                Logical; use development base URL (default TRUE)
#' @param verbosity              Integer; verbosity level for httr2 (default: 0)
#' @param preview                If TRUE, returns the `Dataset` object instead of calling API
#'
#' @return Invisibly returns the parsed API response (named list) on success.
#' @export
update_dataset <- function(
    id,
    title                     = NULL,
    organisation_id           = NULL,
    description               = NULL,
    contact_email             = NULL,
    landing_page              = NULL,
    start_date                = NULL,
    end_date                  = NULL,
    modified_next             = NULL,
    keyword_ids               = NULL,
    zh_web_datacatalog_ids    = NULL,
    relation_ids              = NULL,
    theme_ids                 = NULL,
    periodicity_id            = NULL,
    see_also_ids              = NULL,
    api_key                   = NULL,
    use_dev                   = TRUE,
    verbosity                 = 0,
    preview                   = FALSE
) {
  # API key
  api_key <- get_api_key(api_key)

  # Ensure id is provided
  if (missing(id) || is.na(id)) {
    stop("`id` is required to update a dataset.", call. = FALSE)
  }

  # If org not supplied, fetch existing so we can preserve it
  if (is.null(organisation_id)) {
    existing <- get_dataset(id, api_key = api_key, use_dev = use_dev)
    organisation_id <- existing$organisation$id
  }
  # If org not supplied, fetch existing so we can preserve it
  if (is.null(title)) {
    existing <- get_dataset(id, api_key = api_key, use_dev = use_dev)
    title <- existing$title
  }

  # resolve all lookups up front
  keyword_ids_api         <- if (is.character(keyword_ids))            convert_keywords_to_id(keyword_ids)           else keyword_ids
  zh_web_catalog_ids_api  <- if (is.character(zh_web_datacatalog_ids)) convert_zh_web_catalog_to_id(zh_web_datacatalog_ids) else zh_web_datacatalog_ids
  theme_ids_api           <- if (is.character(theme_ids))              convert_themes_to_id(theme_ids)               else theme_ids
  periodicity_id_api      <- if (is.character(periodicity_id))         convert_periodicities_to_id(periodicity_id)   else periodicity_id
  see_also_ids_api        <- if (is.character(see_also_ids))           convert_datasets_to_id(see_also_ids)          else see_also_ids

  # Build pure Dataset (no API calls here) with numeric IDs
  ds <- Dataset(
    id                       = id,
    title                    = title,
    organisation_id          = organisation_id,
    description              = description,
    contact_email            = contact_email,
    landing_page             = landing_page,
    start_date               = start_date,
    end_date                 = end_date,
    modified_next            = modified_next,
    keyword_ids              = keyword_ids_api,
    zh_web_datacatalog_ids   = zh_web_catalog_ids_api,
    relation_ids             = relation_ids,
    theme_ids                = theme_ids_api,
    periodicity_id           = periodicity_id_api,
    see_also_ids             = see_also_ids_api
  )

  # Dispatch the update method
  if (!preview) {

    # collect response
    resp <- update(ds, api_key, use_dev, verbosity = verbosity)

    # extract ID from response
    id <- resp$id

    # ---- run post-create validation check ----
    dataset_is_valid_for_status(
      id,
      api_key = api_key,
      use_dev = use_dev,
      verbosity = verbosity,
      fail_on_invalid = FALSE
    )

  invisible(resp)


  } else {
    return(ds)
  }
}
