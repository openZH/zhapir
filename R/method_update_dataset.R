#' @keywords internal
S7::method(update, Dataset) <- function(object, id, auth_info, use_dev = TRUE) {
  base_url <- get_base_url(use_dev)

  # Fetch current dataset to ensure required fields are preserved
  current_dataset <- get_dataset(id, auth_info, use_dev)

  # Start with an empty payload
  payload <- list()

  # Only include fields that were explicitly set in the update object
  # Simple properties
  if (!is.na(object@title)) payload$title <- object@title
  if (!is.na(object@description)) payload$description <- object@description
  if (!is.na(object@contact_id)) payload$contact_id <- object@contact_id
  if (!is.na(object@publisher_id)) payload$publisher_id <- object@publisher_id
  if (!is.na(object@issued)) payload$issued <- object@issued
  if (!is.na(object@landing_page)) payload$landing_page <- object@landing_page
  if (!is.na(object@modified_next)) payload$modified_next <- object@modified_next
  if (!is.na(object@relation_id)) payload$relation_id <- object@relation_id
  if (!is.na(object@status_id)) payload$status_id <- object@status_id

  # List properties
  if (length(object@keyword_ids) > 0) payload$keyword_ids <- object@keyword_ids
  if (length(object@theme_ids) > 0) payload$theme_ids <- object@theme_ids
  if (length(object@see_also_ids) > 0) payload$see_also_ids <- object@see_also_ids
  if (length(object@zhweb_catalog_ids) > 0) payload$zhweb_catalog_ids <- object@zhweb_catalog_ids

  # Handle temporal specially - common case is updating end_time
  if (!is.null(object@temporal) && length(object@temporal) > 0) {
    # Start with current temporal to preserve required fields
    if (!is.null(current_dataset$temporal)) {
      payload$temporal <- current_dataset$temporal

      # Override with provided fields
      for (field in names(object@temporal)) {
        payload$temporal[[field]] <- object@temporal[[field]]
      }
    } else {
      # No existing temporal, use new one as is
      payload$temporal <- object@temporal
    }
  }

  # Handle status specially
  if (!is.null(object@status) && length(object@status) > 0) {
    payload$status <- object@status
  } else if (!is.na(object@status_id)) {
    if (!is.null(current_dataset$status)) {
      payload$status <- current_dataset$status
      payload$status$id <- object@status_id
    } else {
      payload$status <- list(
        id = object@status_id,
        label = "Default"
      )
    }
  }

  # Make the API request
  response <- httr2::request(paste0(base_url, "/api/v1/datasets/", id)) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json, application/problem+json",
      "Cookie" = auth_info$cookie,
      "Language" = "de"  # Default language, could be made configurable
    ) |>
    httr2::req_body_json(payload, null = "null") |>
    httr2::req_method("PATCH") |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300) {
    resp_data <- httr2::resp_body_json(response)
    return(resp_data)
  } else {
    stop("API request failed with status ", httr2::resp_status(response),
         ": ", httr2::resp_body_string(response))
  }
}
