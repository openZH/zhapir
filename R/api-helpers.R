#' Send an API request and print CLI feedback
#'
#' @description
#' Sends an HTTP request to the API for a given S7 object and provides contextual
#' CLI feedback based on the object type and HTTP method.
#' The request body is automatically encoded as JSON or multipart/form-data.
#'
#' On success, a confirmation message is shown via the CLI. On failure, a formatted error message is printed.
#'
#' @param object        An S7 object (e.g., `Dataset`, `Distribution`, `FileUpload`) to be sent as payload.
#' @param method        HTTP method as string; one of `"POST"`, `"PATCH"`, `"PUT"`, `"DELETE"`, `"GET"`.
#' @param endpoint      Character string; the target API endpoint.
#' @param api_key       API key string used for authentication.
#' @param use_dev       Logical; whether to use the development API base URL (default: `TRUE`).
#' @param verbosity     Integer; verbosity level passed to httr2::req_perform() (default: 0).
#' @param object_label  Human-readable label for the object (used in CLI messages).
#'
#' @return Invisibly returns either the raw `httr2_response` (for HTTP errors) or the parsed response as a list.
#' @keywords internal
api_request_wrapper <- function(
    object,
    method = c("POST", "PATCH", "PUT", "DELETE", "GET"),
    endpoint = NULL,
    api_key = NULL,
    use_dev = TRUE,
    verbosity = 0,
    object_label = NULL
)
{

  method <- match.arg(method)

  result <- tryCatch(
    {
      # Perform the actual API request (JSON or multipart is handled internally)
      raw <- api_request(
        method, endpoint,
        object, object_label,
        api_key,
        verbosity = verbosity,
        use_dev = use_dev
      )

      # If api_request() already parsed the JSON (returns a list),
      # skip resp_body_json; otherwise parse the httr2_response.
      if (inherits(raw, "httr2_response")) {
        parsed <- httr2::resp_body_json(raw)
      } else {
        parsed <- raw
      }

      # Extract key info for CLI feedback
      title     <- parsed$title %||% "unknown"
      id        <- parsed$id    %||% "unknown"
      parent_id <- parsed$dataset$id %||% "unknown"

      # Success messages by method/object type
      if (method == "POST" && object_label == "Dataset") {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {title}} (ID {.val {id}}) successfully created."
        )
      } else if (method == "POST" && object_label == "Distribution") {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {title}} (ID {.val {id}}) successfully created inside Dataset ID {.val {parent_id}}."
        )
      } else if (method == "POST" && object_label == "FileUpload") {
        file_path     <- tryCatch(object@file_path, error = function(e) "unknown")
        file_upload_id<- parsed$id %||% "unknown"
        cli::cli_alert_success(
          "{.strong File} {.file {file_path}} uploaded successfully (Upload ID: {.val {file_upload_id}})."
        )
      } else if (method == "PATCH" && object_label %in% c("Dataset", "Distribution", "Distribution Status")) {

        if (object_label == "Distribution Status"){
          cli::cli_alert_success(
            "{.strong Distribution {.val {title}}} (ID {.val {id}}) successfully updated to Status: {parsed$status$label}."
          )
        } else {
          cli::cli_alert_success(
            "{.strong {object_label}} {.val {title}} (ID {.val {id}}) successfully updated."
          )
        }


      } else {
        cli::cli_alert_success(
          "{.strong {object_label}} {.val {title}} (ID {.val {id}}) {method}-request succeeded."
        )
      }

      invisible(raw)
    },
    error = function(e) {
      # HTTP status code (or "unknown")
      code <- if (inherits(e, "httr2_http")) {
        e$resp$status_code
      } else {
        "unknown"
      }
      # ID from the object (or "n/a")
      id <- tryCatch(object@id, error = function(e) "n/a")

      # Default to the raw message
      detailed <- as.character(e$message)

      # If it's an httr2 HTTP error, try to extract the server‐side "detail"
      if (inherits(e, "httr2_http")) {
        # parse the JSON body
        body <- tryCatch(httr2::resp_body_json(e$resp), error = function(e2) NULL)
        if (!is.null(body$errors)) {
          # concatenate all "detail" fields
          details <- vapply(body$errors, `[[`, "", "detail")
          detailed <- paste(details, collapse = "; ")
        }
      }

      cli::cli_alert_danger(
        "{.strong {object_label}} (ID {.val {id}}) {method}-Request failed ({code}): {detailed}"
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
      `x-api-key` = api_key,
      .redact = "x-api-key"
    )

  # Handle GET requests without body/payload
  if (method == "GET") {
    # do nothing
  }
  # If object is a file upload, use multipart/form-data
  else if (object_label == "FileUpload") {
    payload <- list(file = curl::form_file(object@file_path))

    # Attach file using multipart body
    req <- req |> httr2::req_body_multipart(!!!payload)
  }
  # if we want to change the status of a distribution/dataset
  else if (grepl("/set-status$", endpoint))
  {
    payload <- list(status_id = object@status_id)

    req <- req |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(payload, null = "null")
  }

  else {
    # Otherwise, serialise object as JSON
    payload <- object_to_payload(object)
    req <- req |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(payload, null = "null")
  }
  # Perform request
  resp <- req |> httr2::req_perform(verbosity = verbosity)

  # Return parsed JSON body
  httr2::resp_body_json(resp)
}
