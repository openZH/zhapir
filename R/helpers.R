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





#' Retrieve MDV API key
#'
#' Attempts to fetch the API key from (in order):
#' 1. Provided argument
#' 2. Environment variable `MDV_API_KEY`
#' 3. Interactive prompt (RStudio or askpass)
#'
#' @param key Optional plain API key.
#' @return API key string.
#' @export
get_api_key <- function(key = NULL) {
  # 1. Direct argument
  if (!is.null(key) && nzchar(key)) {
    return(key)
  }

  # 2. Environment variable
  env_key <- Sys.getenv("ZHAPIR_API_KEY")
  if (nzchar(env_key)) {
    return(env_key)
  }

  # 3. Interactive prompt (last resort)
  if (interactive()) {
      prompt_key <- askpass::askpass("Please enter your MDV API key")
    }
    if (nzchar(prompt_key)) {
      return(prompt_key)
    }

  stop(
    "No API key found. Supply via argument or set MDV_API_KEY environment variable.",
    call. = FALSE
  )
}



#' Convert an S7 object into a JSON-ready payload list
#'
#' This helper extracts all properties, formats dates, flattens simple lists,
#' and removes empty or missing values.
#'
#' @param object An S7 object (e.g., Dataset or Distribution)
#' @return A named list suitable for httr2::req_body_json()
#' @keywords internal
object_to_payload <- function(object) {
  # 1. Extract raw properties
  p <- S7::props(object)

  # Helper for ISO-8601
  fmt <- function(dt) format(dt, "%Y-%m-%dT%H:%M:%SZ")

  # 2. Transform properties:
  #   - POSIXct -> ISO strings
  #   - simple lists of scalars -> atomic vectors
  p <- purrr::map(p, function(x) {
    if (inherits(x, "POSIXct")) {
      if (!is.na(x)) return(fmt(x))
      return(NA_character_)
    }
    if (is.list(x) && length(x) > 0L &&
        all(purrr::map_lgl(x, ~ is.atomic(.) && length(.) == 1L))) {
      return(unlist(x, use.names = FALSE))
    }
    x
  })

  # 3. Remove empty or missing values:
  #    - NULL
  #    - length-1 NA
  #    - empty lists
  p <- purrr::keep(p, function(x) {
    !(is.null(x) ||
        (is.atomic(x) && length(x) == 1L && is.na(x)) ||
        (is.list(x) && length(x) == 0L))
  })

  p
}


#' Helper for API calls
#'
#' @param method string; what kind of request should be made
#' @param endpoint string; which endpoint should be reached
#' @param payload list containing the payload
#' @inheritParams get_dataset
#' @keywords internal
api_request <- function(
    method = c("GET", "POST", "PUT", "PATCH", "DELETE"),
    endpoint,
    payload = NULL,
    api_key,
    use_dev = TRUE
) {
  method <- match.arg(method)
  base_url <- get_base_url(use_dev)


  req <- httr2::request(paste0(base_url, endpoint)) |>
    httr2::req_method(method) |>
    httr2::req_headers(
      `Content-Type` = "application/json",
      Accept         = "application/json, application/problem+json",
      `x-api-key`    = api_key
    )

  if (!is.null(payload)) {
    req <- req |> httr2::req_body_json(payload, null = "null")
  }

  resp <- req |> httr2::req_perform()
  status <- httr2::resp_status(resp)

  if (status < 300) {
    return(httr2::resp_body_json(resp))
  }

  stop(
    sprintf("API %s failed [%s]: %s", method, status, httr2::resp_body_string(resp)),
    call. = FALSE
  )
}



#' Retrieve a dataset by ID from the MDV API
#'
#' @param id Numeric; the dataset ID to fetch.
#' @param api_key MDV API key (optional; falls back to env var).
#' @param use_dev Logical; if TRUE, uses the development API endpoint.
#' @return A named list parsed from the JSON response.
#' @export
get_dataset <- function(id, api_key = NULL, use_dev = TRUE) {

  if(is.null(api_key)){
    api_key <- get_api_key(api_key)
  }


  url <- paste0(get_base_url(use_dev), "/api/v1/datasets/", id)

  resp <- httr2::request(url) |>
    httr2::req_headers(
      Accept      = "application/json",
      `x-api-key` = api_key
    ) |>
    httr2::req_method("GET") |>
    httr2::req_perform()

  status <- httr2::resp_status(resp)
  if (status < 300) {
    httr2::resp_body_json(resp)
  } else {
    stop(
      sprintf("Failed to fetch dataset [%s]: %s", status,
              httr2::resp_body_string(resp)),
      call. = FALSE
    )
  }
}



to_POSIXct <- function(date_var){
  if (!inherits(date_var, "S7_missing")) {
    as.POSIXct(date_var, tz = "UTC")
  } else {
    S7::class_missing
  }
}

to_list <- function(vec_var){
  if (!inherits(vec_var, "S7_missing")) {
    as.list(vec_var)
  } else {
    S7::class_missing
  }
}


