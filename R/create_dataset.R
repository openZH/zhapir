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
  #api_key <- get_api_key(api_key)

  # Title is required for creation
  if (is.null(title) || is.na(title) || nzchar(title) == FALSE) {
    stop("`title` ist erforderlich, um ein neues Dataset zu erstellen.", call. = FALSE)
  }



  args <-as.list(match.call())
  args <- args[2:length(args)]

  date_args <- args[grepl("start_date|end_date|modified", names(args))]

  args[grepl("start_date|end_date|modified", names(args))] <- NULL
  date_args <- lapply(date_args, as.POSIXct)

  ds <- do.call(Dataset, c(args, date_args))


  # Prepare fields, converting NULL to appropriate NA or defaults
  # FIXME: Where do we set defaults?
  # if the defaults are set in the properties, all this is not needed but rather
  # just pass the input parameters
  # ds <- Dataset(
  #   id = NA_real_,
  #   title = title,
  #   organisation_id = organisation_id,
  #   description =  description,
  #   contact_email = contact_email,
  #   landing_page = landing_page,
  #   issued = if (!is.null(issued)) as.POSIXct(issued, tz = "UTC") ,
  #   start_date = if (!is.null(start_date)) as.POSIXct(start_date, tz = "UTC"),
  #   end_date = if (!is.null(end_date)) as.POSIXct(end_date, tz = "UTC"),
  #   modified = if (!is.null(modified)) as.POSIXct(modified, tz = "UTC"),
  #   modified_next = if (!is.null(modified_next)) as.POSIXct(modified_next, tz = "UTC"),
  #   keyword_ids = keyword_ids,
  #   zh_web_catalog_ids = zh_web_catalog_ids,
  #   relation_ids = relation_ids,
  #   theme_ids = theme_ids,
  #   periodicity_id = periodicity_id,
  #   see_also_ids = see_also_ids
  # )


  return(ds)
  # Dispatch create method
  #create(ds, api_key, use_dev)
}
