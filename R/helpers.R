
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
      prompt_key <- askpass::askpass("Please enter your ZHAPIR_API_KEY key")
    }
    if (nzchar(prompt_key)) {
      return(prompt_key)
    }

  stop(
    "No API key found. Supply via argument or set ZHAPIR_API_KEY environment variable.",
    call. = FALSE
  )
}



#' Convert an S7 object into a JSON-ready payload list
#'
#' This helper extracts all properties, formats dates, and removes empty
#' or missing values (including single-element lists of NA), but does not
#' flatten length-1 lists so that they remain JSON arrays.
#'
#' @param object An S7 object (e.g., Dataset or Distribution)
#' @return A named list suitable for httr2::req_body_json()
#' @keywords internal
object_to_payload <- function(object) {

  # 1. Extract raw properties
  p <- S7::props(object)

  # Helper for date-only format (needed for S7 to JSON)
  fmt_date <- function(d) format(d, "%Y-%m-%d")

  # 2. Transform properties:
  #    - POSIXct/Date → YYYY-MM-DD
  p <- purrr::map(p, function(x) {
    if (inherits(x, c("POSIXct", "Date"))) {
      # if no date at all or only NA, emit a single NA_character_
      if (length(x) == 0L || all(is.na(x))) {
        return(NA_character_)
      }
      # otherwise we expect exactly one non-NA date:
      # format it and return
      return(fmt_date(x))
    }
    x
  })

  # 3. Remove empty or missing values:
  #    - NULL
  #    - length-1 atomic NA
  #    - empty lists
  #    - single-element lists whose only element is NA
  p <- purrr::keep(p, function(x) {
    !(
      is.null(x) ||
        (is.atomic(x) && length(x) == 1L && is.na(x)) ||
        (is.list(x)   && length(x) == 0L) ||
        (is.list(x)   &&
           length(x) == 1L &&
           is.atomic(x[[1]]) &&
           is.na(x[[1]]))
    )
  })

  p
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


to_date <- function(x) {
  if (!inherits(x, "S7_missing")) {
    as.Date(x)
  } else {
    S7::class_missing
  }
}

to_list <- function(vec_var) {
  if (!inherits(vec_var, "S7_missing")) {
    # c(42) → list(42); c(1,2,3) → list(1,2,3)
    as.list(vec_var)
  } else {
    S7::class_missing
  }
}

