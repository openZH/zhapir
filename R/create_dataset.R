#' Create a new dataset via API
#'
#' @param title             Title of the dataset (required, <= 1000 chars)
#' @param organisation_id   Organisation ID (required integer)
#' @param description       Optional description string
#' @param contact_email     Optional contact email
#' @param landing_page      Optional landing page URL
#' @param start_date        Optional ISO datetime string or POSIXct
#' @param end_date          Optional ISO datetime string or POSIXct
#' @param modified_next     Optional ISO datetime string or POSIXct
#' @param keyword_ids       Optional character vector
#' @param zh_web_catalogs Optional character vector
#' @param relation_ids      Optional integer vector
#' @param theme_ids         Optional character vector
#' @param periodicity_id    Optional character
#' @param see_also_ids      Optional integer vector
#' @param api_key           API key (optional; falls back to env var)
#' @param use_dev           Logical; use development base URL
#' @param verbosity Integer; verbosity level passed to httr2::req_perform() (default: 0).
#' @param preview           Defines if it is a test run. If TRUE, the
#'                          dataset-object is returned into the environment.
#'                          Default = FALSE
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
    zh_web_catalogs = NULL,
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

  # Capture arguments of function call and construct a Dataset-Object
  args <- as.list(match.call())
  args <- args[2:length(args)]
  args <- args[!grepl("api_key|use_dev|preview|verbosity", names(args))]

  ds <- do.call(Dataset, args)


  # Dispatch create method
  if (!preview) {
    create(ds, api_key, use_dev, verbosity = verbosity)
  } else {
    return(ds)
  }
}
