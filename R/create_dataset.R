#' Create a new dataset via API
#'
#' @param title             Title of the dataset (required, <= 1000 chars)
#' @param organisation_id   Organisation ID (required integer)
#' @param description       Optional description string
#' @param contact_email     Optional contact email
#' @param landing_page      Optional landing page URL
#' @param issued            Optional ISO datetime string or POSIXct
#' @param start_date        Optional ISO datetime string or POSIXct
#' @param end_date          Optional ISO datetime string or POSIXct
#' @param modified          Optional ISO datetime string or POSIXct
#' @param modified_next     Optional ISO datetime string or POSIXct
#' @param keyword_ids       Optional integer vector
#' @param zh_web_catalog_ids Optional integer vector
#' @param relation_ids      Optional integer vector
#' @param theme_ids         Optional integer vector
#' @param periodicity_id    Optional integer
#' @param see_also_ids      Optional integer vector
#' @param api_key           API key (optional; falls back to env var)
#' @param use_dev           Logical; use development base URL
#' @export
create_dataset <- function(
    title,
    organisation_id,
    description        = NULL,
    contact_email      = NULL,
    landing_page       = NULL,
    issued             = NULL,
    start_date         = NULL,
    end_date           = NULL,
    modified           = NULL,
    modified_next      = NULL,
    keyword_ids        = NULL,
    zh_web_catalog_ids = NULL,
    relation_ids       = NULL,
    theme_ids          = NULL,
    periodicity_id     = NULL,
    see_also_ids       = NULL,
    api_key            = NULL,
    use_dev            = TRUE
) {

  print("Here")
  # Extract or prompt for API key
  api_key <- get_api_key(api_key)

  # Title is required for creation
  if (is.null(title) || is.na(title) || nzchar(title) == FALSE) {
    stop("`title` ist erforderlich, um ein neues Dataset zu erstellen.", call. = FALSE)
  }


  # Prepare fields, converting NULL to appropriate NA or defaults
  ds <- Dataset(
    id = NA_real_,
    title = title,
    organisation_id = organisation_id,
    description = if (is.null(description)) NA_character_ else description,
    contact_email = if (is.null(contact_email)) NA_character_ else contact_email,
    landing_page = if (is.null(landing_page)) NA_character_ else landing_page,
    issued = if (!is.null(issued)) as.POSIXct(issued, tz = "UTC") else as.POSIXct(NA),
    start_date = if (!is.null(start_date)) as.POSIXct(start_date, tz = "UTC") else as.POSIXct(NA),
    end_date = if (!is.null(end_date)) as.POSIXct(end_date, tz = "UTC") else as.POSIXct(NA),
    modified = if (!is.null(modified)) as.POSIXct(modified, tz = "UTC") else as.POSIXct(NA),
    modified_next = if (!is.null(modified_next)) as.POSIXct(modified_next, tz = "UTC") else as.POSIXct(NA),
    keyword_ids = if (is.null(keyword_ids)) list() else keyword_ids,
    zh_web_catalog_ids = if (is.null(zh_web_catalog_ids)) list() else zh_web_catalog_ids,
    relation_ids = if (is.null(relation_ids)) list() else relation_ids,
    theme_ids = if (is.null(theme_ids)) list() else theme_ids,
    periodicity_id = if (is.null(periodicity_id)) NA_real_ else periodicity_id,
    see_also_ids = if (is.null(see_also_ids)) list() else see_also_ids
  )

  # Dispatch create method
  create(ds, api_key, use_dev)
}
