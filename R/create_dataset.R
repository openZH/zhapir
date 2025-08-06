#' Create a new dataset via API
#'
#' @param title               Title of the dataset (required, <= 1000 chars)
#' @param organisation_id     Organisation ID (required integer)
#' @param description         Optional description string
#' @param contact_email       Optional contact email
#' @param landing_page        Optional landing page URL
#' @param start_date          Optional ISO datetime string or POSIXct
#' @param end_date            Optional ISO datetime string or POSIXct
#' @param modified_next       Optional ISO datetime string or POSIXct
#' @param keyword_ids         Optional character vector
#' @param zh_web_datacatalog_ids Optional character vector ('Datenkollektionen' in the UI)
#' @param relation_ids        Optional integer vector
#' @param theme_ids           Optional character vector
#' @param periodicity_id      Optional character
#' @param see_also_ids        Optional integer vector
#' @param api_key             API key (optional; falls back to env var)
#' @param use_dev             Logical; use development base URL
#' @param verbosity           Integer; verbosity level passed to httr2::req_perform() (default: 0).
#' @param preview             Defines if it is a test run. If TRUE, the
#'                            dataset-object is returned into the environment.
#'                            Default = FALSE
#'
#' @details
#' To assist in constructing valid queries, the following functions provide the
#' set of acceptable values:
#'
#' - `get_organisations()`
#' - `get_keyword()`
#' - `get_zh_web_catalog()`
#' - `get_theme()`
#' - `get_periodicities()`
#' - `get_datasets()`: Valid values for the `see_also_ids` argument.
#'
#'
#' Use these functions to inspect the available values before making a query.
#'
#' @export
create_dataset <- function(
    title,
    organisation_id,
    description = NULL,
    contact_email = NULL,
    landing_page = NULL,
    start_date = NULL,
    end_date = NULL,
    modified_next = NULL,
    keyword_ids = NULL,
    zh_web_datacatalog_ids = NULL,
    relation_ids = NULL,
    theme_ids = NULL,
    periodicity_id = NULL,
    see_also_ids = NULL,
    api_key = NULL,
    use_dev = TRUE,
    verbosity = 0,
    preview = FALSE) {

    # Extract or prompt for API key
  api_key <- get_api_key(api_key)

  # Title is required for creation
  if (is.null(title) || is.na(title) || nzchar(title) == FALSE) {
    stop("`title` ist erforderlich, um ein neues Dataset zu erstellen.", call. = FALSE)
  }


  # resolve all lookups up front
  keyword_ids_api         <- if (is.character(keyword_ids))         convert_keywords_to_id(keyword_ids)           else keyword_ids
  zh_web_catalog_ids_api      <- if (is.character(zh_web_datacatalog_ids)) convert_zh_web_catalog_to_id(zh_web_datacatalog_ids) else zh_web_datacatalog_ids
  theme_ids_api           <- if (is.character(theme_ids))           convert_themes_to_id(theme_ids)               else theme_ids
  periodicity_id_api      <- if (is.character(periodicity_id))      convert_periodicities_to_id(periodicity_id)   else periodicity_id
  see_also_ids_api        <- if (is.character(see_also_ids))        convert_datasets_to_id(see_also_ids)          else see_also_ids



  # 3. Build pure Dataset (no API calls here) with numeric IDs
  ds <- Dataset(
    title = title,
    organisation_id = organisation_id,
    description = description,
    contact_email = contact_email,
    landing_page = landing_page,
    start_date = start_date,
    end_date = end_date,
    modified_next = modified_next,
    keyword_ids = keyword_ids_api,
    zh_web_datacatalog_ids = zh_web_catalog_ids_api,
    relation_ids = relation_ids,
    theme_ids = theme_ids_api,
    periodicity_id = periodicity_id_api,
    see_also_ids = see_also_ids_api
  )

  # Dispatch create method
  if (!preview) {
    create(ds, api_key, use_dev, verbosity = verbosity)
  } else {
    return(ds)
  }
}
