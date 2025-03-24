#' Get the base URL based on environment setting
#'
#' @param use_dev Whether to use the development environment
#' @return The base URL string
#' @keywords internal
get_base_url <- function(use_dev = FALSE) {
  if (use_dev) {
    return("https://mdv-dev.nebula.statzh.ch")
  } else {
    return("https://mdv.nebula.statzh.ch")
  }
}



#' Get a list of datasets from the catalog
#'
#' @description
#' Retrieves all datasets available in the data catalog that the user has access to.
#'
#' @param auth_info Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment (default: TRUE)
#'
#' @return A list of datasets as returned by the API
#' @export
#'
#' @examples
#' \dontrun{
#' # First login
#' auth_info <- login_to_api("admin@example.com", "admin")
#'
#' # Get all datasets
#' datasets <- get_datasets(auth_info)
#' }
get_datasets <- function(auth_info, use_dev = TRUE) {
  base_url <- if(use_dev) "https://mdv-dev.nebula.statzh.ch" else "https://mdv.nebula.statzh.ch"

  response <- httr2::request(paste0(base_url, "/api/v1/datasets")) |>
    httr2::req_headers(
      "Accept" = "application/json, application/problem+json",
      "Cookie" = auth_info$cookie
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300) {
    resp_data <- httr2::resp_body_json(response)
    return(resp_data)
  } else {
    stop("API request failed with status ", httr2::resp_status(response),
         ": ", httr2::resp_body_string(response))
  }
}



#' Get a single dataset from the catalog
#'
#' @description
#' Retrieves a single dataset by its ID.
#'
#' @param dataset_id ID of the dataset to retrieve
#' @param auth_info Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment (default: TRUE)
#'
#' @return The dataset information as returned by the API
#' @export
#'
#' @examples
#' \dontrun{
#' # First login
#' auth_info <- login_to_api("admin@example.com", "admin")
#'
#' # Get a dataset
#' dataset <- get_dataset(123, auth_info)
#' }
get_dataset <- function(dataset_id, auth_info, use_dev = TRUE) {
  base_url <- get_base_url(use_dev)

  response <- httr2::request(paste0(base_url, "/api/v1/datasets/", dataset_id)) |>
    httr2::req_headers(
      "Accept" = "application/json, application/problem+json",
      "Cookie" = auth_info$cookie
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300) {
    resp_data <- httr2::resp_body_json(response)
    return(resp_data)
  } else {
    stop("API request failed with status ", httr2::resp_status(response),
         ": ", httr2::resp_body_string(response))
  }
}

#' Get available formats for distributions
#'
#' @description
#' Retrieves all available formats that can be used for distributions.
#'
#' @param auth_info Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment (default: TRUE)
#'
#' @return A list of available formats
#' @export
get_formats <- function(auth_info, use_dev = TRUE) {
  base_url <- get_base_url(use_dev)

  response <- httr2::request(paste0(base_url, "/api/v1/formats")) |>
    httr2::req_headers(
      "Accept" = "application/json, application/problem+json",
      "Cookie" = auth_info$cookie
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300) {
    resp_data <- httr2::resp_body_json(response)
    return(resp_data)
  } else {
    stop("API request failed with status ", httr2::resp_status(response),
         ": ", httr2::resp_body_string(response))
  }
}

#' Get available media types for distributions
#'
#' @description
#' Retrieves all available media types that can be used for distributions.
#'
#' @param auth_info Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment (default: TRUE)
#'
#' @return A list of available media types
#' @export
get_media_types <- function(auth_info, use_dev = TRUE) {
  base_url <- get_base_url(use_dev)

  response <- httr2::request(paste0(base_url, "/api/v1/media-types")) |>
    httr2::req_headers(
      "Accept" = "application/json, application/problem+json",
      "Cookie" = auth_info$cookie
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  if (httr2::resp_status(response) >= 200 && httr2::resp_status(response) < 300) {
    resp_data <- httr2::resp_body_json(response)
    return(resp_data)
  } else {
    stop("API request failed with status ", httr2::resp_status(response),
         ": ", httr2::resp_body_string(response))
  }
}


