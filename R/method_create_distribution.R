#' @keywords internal
S7::method(create, Distribution) <- function(object, auth_info, use_dev = TRUE) {
  base_url <- get_base_url(use_dev)

  # Construct the payload
  payload <- list(
    title = object@title,
    description = object@description,
    dataset_id = object@dataset_id,
    access_url_id = if(!is.na(object@access_url_id)) object@access_url_id else NULL,
    byte_size = if(!is.na(object@byte_size)) object@byte_size else -1,
    download_url_id = if(!is.na(object@download_url_id)) object@download_url_id else NULL,
    format_id = if(!is.na(object@format_id)) object@format_id else NULL,
    identifier = if(!is.na(object@identifier)) object@identifier else NULL,
    issued = if(!is.na(object@issued)) object@issued else NULL,
    media_type_id = if(!is.na(object@media_type_id)) object@media_type_id else NULL,
    ogd_flag = object@ogd_flag,
    right_id = if(!is.na(object@right_id)) object@right_id else NULL,
    sort_order = if(!is.na(object@sort_order)) object@sort_order else 0,
    stat_server_flag = object@stat_server_flag,
    status_id = if(!is.na(object@status_id)) object@status_id else NULL,
    zhweb_flag = object@zhweb_flag
  )

  # Remove NULL entries
  payload <- payload[!sapply(payload, is.null)]

  # Make the API request
  response <- httr2::request(paste0(base_url, "/api/v1/distributions")) |>
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
