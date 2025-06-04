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
#' @param test              Defines if it is a test run. If TRUE, the
#'                          dataset-object is returned into the environment.
#'                          Default = FALSE
#'
#' @export
create_dataset <- function(
    title,
    organisation_id,
    description = NULL,
    contact_email = NULL,
    landing_page = NULL,
    issued = NULL,
    start_date = NULL,
    end_date = NULL,
    modified = NULL,
    modified_next = NULL,
    keyword_ids = NULL,
    zh_web_catalog_ids = NULL,
    relation_ids = NULL,
    theme_ids = NULL,
    periodicity_id = NULL,
    see_also_ids = NULL,
    api_key = NULL,
    use_dev = TRUE,
    test = FALSE) {
  # Extract or prompt for API key
  api_key <- get_api_key(api_key)

  # Title is required for creation
  if (is.null(title) || is.na(title) || nzchar(title) == FALSE) {
    stop("`title` ist erforderlich, um ein neues Dataset zu erstellen.", call. = FALSE)
  }

  # Capture arguments of function call and construct a Dataset-Object
  args <- as.list(match.call())
  args <- args[2:length(args)]
  args <- args[!grepl("api_key|use_dev|test", names(args))]

  ds <- do.call(Dataset, args)

  # Dispatch create method
  if (!test) {
    create(ds, api_key, use_dev)
  } else {
    return(ds)
  }
}
