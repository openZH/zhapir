#' @keywords internal
S7::method(create, Dataset) <- function(object, auth_info, use_dev = TRUE) {
  base_url <- get_base_url(use_dev)

  # Prepare temporal object if provided
  temporal <- NULL
  if (!is.null(object@temporal) && length(object@temporal) > 0) {
    temporal <- object@temporal
  } else if (!is.na(object@issued)) {
    # Create a minimal temporal object with just start_date
    temporal <- list(
      start_date = object@issued
    )
  }

  # Prepare status object if provided
  status <- NULL
  if (!is.null(object@status) && length(object@status) > 0) {
    status <- object@status
  } else if (!is.na(object@status_id)) {
    # Create a minimal status object
    status <- list(
      id = object@status_id,
      label = "Default"  # This might need to be fetched from the API
    )
  }

  # Construct the payload
  payload <- list(
    title = object@title,
    description = object@description,
    contact_id = object@contact_id,
    publisher_id = object@publisher_id,
    status_id = object@status_id,
    issued = object@issued,
    keyword_ids = if(length(object@keyword_ids) > 0) object@keyword_ids else NULL,
    theme_ids = if(length(object@theme_ids) > 0) object@theme_ids else NULL,
    landing_page = if(!is.na(object@landing_page)) object@landing_page else NULL,
    modified_next = if(!is.na(object@modified_next)) object@modified_next else NULL,
    relation_id = if(!is.na(object@relation_id)) object@relation_id else NULL,
    see_also_ids = if(length(object@see_also_ids) > 0) object@see_also_ids else NULL,
    status = status,
    temporal = temporal,
    zhweb_catalog_ids = if(length(object@zhweb_catalog_ids) > 0) object@zhweb_catalog_ids else NULL
  )

  # Remove NULL entries
  payload <- payload[!sapply(payload, is.null)]

  # Make the API request
  response <- httr2::request(paste0(base_url, "/api/v1/datasets")) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json, application/problem+json",
      "Cookie" = auth_info$cookie,
      "Language" = "de"  # Default language, could be made configurable
    ) |>
    httr2::req_body_json(payload, null = "null") |>
    httr2::req_method("POST") |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300) {
    resp_data <- httr2::resp_body_json(response)
    return(resp_data)
  } else {
    stop("API request failed with status ", httr2::resp_status(response),
         ": ", httr2::resp_body_string(response))
  }
}
