
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
      if (!is.na(x)) {
        return(fmt_date(x))
      }
      return(NA_character_)
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


#' Send an API request and print CLI feedback
#'
#' @description
#' Sends an HTTP request to the API for a given S7 object and provides contextual
#' CLI feedback based on the object type and HTTP method.
#' The request body is automatically encoded as JSON or multipart/form-data
#'
#' On success, a confirmation message is shown via the CLI. On failure, a formatted error message is printed.
#'
#' @param object        An S7 object (e.g., `Dataset`, `Distribution`, `FileUpload`) to be sent as payload.
#' @param method        HTTP method as string; one of `"POST"`, `"PATCH"`, `"PUT"`, `"DELETE"`, `"GET"`.
#' @param endpoint      Character string; the target API endpoint.
#' @param api_key       API key string used for authentication.
#' @param use_dev       Logical; whether to use the development API base URL (default: `TRUE`).
#' @param verbosity         Integer; verbosity level passed to httr2::req_perform() (default: 0).
#' @param object_label  Human-readable label for the object (used in CLI messages).
#'
#' @return Invisibly returns the parsed API response as a list. Returns `NULL` if the request fails.
#'
#' @keywords internal
api_request_wrapper <- function(
    object,
    method = c("POST", "PATCH", "PUT", "DELETE", "GET"),
    endpoint,
    api_key,
    use_dev = TRUE,
    verbosity = 0,
    object_label
) {
  method <- match.arg(method)
  result <- tryCatch(
    {
      # Perform the actual API request (JSON or multipart is handled internally)
      result <- api_request(method, endpoint, object, object_label, api_key, verbosity = verbosity, use_dev)
      parsed_result <- httr2::resp_body_json(result)

      # Extract key info for CLI feedback
      title <- parsed_result$title %||% "unknown"
      id <- parsed_result$id %||%  "unknown"
      parent_id <- parsed_result$dataset$id %||%  "unknown"

      # Method- and object-specific success messages
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
        file_upload_id <- parsed_result$id %||% "unknown"
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
      # Extract HTTP status code and ID
      code <- if (inherits(e, "httr2_http_error")) e$response$status_code else "unknown"
      id <- tryCatch(object@id, error = function(e) "n/a")
      msg <- as.character(e$message)

      # CLI error message
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
#' @description
#' Internal function to perform an API request for a given S7 object.
#' Depending on the object type, the payload is serialized as JSON (for most objects)
#' or as multipart/form-data (for file uploads). The request is executed using the `httr2` package.
#'
#' @param method        HTTP method to use, e.g. `"POST"`, `"PATCH"`, `"GET"` (required).
#' @param endpoint      Character string; relative API endpoint (e.g. `"/api/v1/datasets"`).
#' @param object        An S7 object representing the payload (e.g., `Dataset`, `Distribution`, or `FileUpload`).
#' @param object_label  Character string indicating the object type (used to determine encoding strategy).
#' @param api_key       API key string used for authentication.
#' @param verbosity         Integer; verbosity level passed to httr2::req_perform() (default: 0).
#' @param use_dev       Logical; whether to use the development environment (default: `TRUE`).
#'
#' @return Parsed response content as a list.
#' @keywords internal
api_request <- function(
    method = c("GET", "POST", "PUT", "PATCH", "DELETE"),
    endpoint,
    object,
    object_label,
    api_key,
    verbosity = 0,
    use_dev = TRUE
) {
  method <- match.arg(method)
  url <- paste0(get_base_url(use_dev), endpoint)

    # Initialise request with method and headers
  req <- httr2::request(url) |>
    httr2::req_method(method) |>
    httr2::req_headers(
      Accept = "application/json",
      `x-api-key` = api_key
    )

  # If object is a file upload, use multipart/form-data
  if (object_label == "FileUpload") {
    payload <- list(file = curl::form_file(object@file_path))

    # Attach file using multipart body
    req <- req |> httr2::req_body_multipart(!!!payload)
  } else {
    # Otherwise, serialise object as JSON
    payload <- object_to_payload(object)
    req <- req |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(payload, null = "null")
  }
  # Perform request
  resp <- req |> httr2::req_perform(verbosity = verbosity)

  # Return parsed JSON body
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

to_list <- function(vec_var){
  if (!inherits(vec_var, "S7_missing")) {
    as.list(vec_var)
  } else {
    S7::class_missing
  }
}


