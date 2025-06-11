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

  # FIXME: Why here and not already in constructor??
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


#' Get first or second argument
#'
#' Returns the first argument if it is not `NULL`, otherwise returns the second.
#'
#' @param x Object 1
#' @param y Object 2
#'
fallback_null <- function(x, y) if (!is.null(x)) x else y



#' API Request Wrapper with CLI Feedback
#'
#' Sends an API request based on the given S7 object. Automatically handles
#' JSON or multipart payloads and routes the request to the appropriate internal
#' function (`api_request_json()` or `api_request_multipart()`).
#'
#' On success, prints a CLI message. On failure, shows an error alert.
#'
#' @param object An S7 object (e.g., Dataset, Distribution, FileUpload).
#' @param method HTTP method as string (e.g., "POST", "PATCH").
#' @param endpoint API endpoint as string.
#' @param api_key API key string.
#' @param use_dev Logical; use development API.
#' @param object_label Label used in CLI output.
#'
#' @return Parsed API response (invisibly), or `NULL` on error.
#' @keywords internal
api_request_wrapper <- function(
    object,
    method = c("POST", "PATCH", "PUT", "DELETE", "GET"),
    endpoint,
    api_key,
    use_dev = TRUE,
    object_label
) {
  method <- match.arg(method)

  result <- tryCatch(
    {
      result <- api_request(method, endpoint, object, object_label, api_key, use_dev)

      # extract values for feedback
      title <- fallback_null(result$title, "unknown")
      id <- fallback_null(result$id, "unknown")
      parent_id <- fallback_null(result$dataset$id, "unknown")

      # CLI success feedback
      if (method == "POST" && object_label == "Dataset") {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {title}} (ID {.val {id}}) successfully created."
        )
      } else if (method == "POST" && object_label == "Distribution") {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {title}} (ID {.val {id}}) successfully created inside Dataset ID {.val {parent_id}}."
        )
      } else if (method == "POST" && object_label == "FileUpload") {
        file_path <- tryCatch(object@file_path, error = function(e) "unknown")
        file_upload_id <- fallback_null(result$id, "unknown")
        cli::cli_alert_success(
          "{.strong File} {.file {file_path}} uploaded successfully (Upload ID: {.val {file_upload_id}})."
        )
      } else if (method == "PATCH" && object_label %in% c("Dataset", "Distribution")) {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {title}} (ID {.val {id}}) successfully updated."
        )
      } else {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {title}} (ID {.val {id}}) {method}-request succeeded."
        )
      }

      invisible(result)
    },
    error = function(e) {
      code <- if (inherits(e, "httr2_http_error")) e$response$status_code else "unknown"
      id <- tryCatch(object@id, error = function(e) "n/a")
      msg <- as.character(e$message)

      # specific error handling
      cli::cli_alert_danger(
        "{.strong {object_label}} (ID {.val {id}}) {method}-Request failed ({code}): {msg}"
      )

      invisible(NULL)
    }
  )

  invisible(result)
}



#' Send API Request
#'
#' Accepts an S7 object, converts it to the appropriate payload (JSON or multipart),
#' and performs the request using `httr2`.
#'
#' @param method HTTP method (e.g., "POST", "PATCH", "GET").
#' @param endpoint Character; API endpoint (e.g., "/api/v1/datasets").
#' @param objectlabel An S7 object (e.g., Dataset, Distribution, or FileUpload).
#' @param api_key API key for authentication.
#' @param use_dev Logical; whether to use the development environment.
#'
#' @return Parsed response body as list.
#' @keywords internal
api_request <- function(
    method = c("GET", "POST", "PUT", "PATCH", "DELETE"),
    endpoint,
    object,
    object_label,
    api_key,
    use_dev = TRUE
) {
  method <- match.arg(method)
  url <- paste0(get_base_url(use_dev), endpoint)

  # Prepare request
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_headers(
      Accept = "application/json",
      `x-api-key` = api_key
    )

  # Convert object to appropriate payload
  if (object_label == "FileUpload") {
    payload <- list(file = curl::form_file(object@file_path))

    req <- req |> httr2::req_body_multipart(!!!payload)
  } else {
    payload <- object_to_payload(object)
    req <- req |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(payload, null = "null")
  }

  # Perform request
  resp <- req |> httr2::req_perform()


  # Return parsed JSON body
  httr2::resp_body_json(resp)
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


