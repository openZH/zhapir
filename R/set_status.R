#' @name set_status
#' @rdname set_status
S7::method(set_status, Distribution) <- function(object, api_key = NULL, use_dev = TRUE, verbosity = 0) {
  api_key <- get_api_key(api_key)
  api_request_wrapper(
    object = object,
    method = "PATCH",
    endpoint = paste0("/api/v1/distributions/", object@id, "/set-status"),
    api_key = api_key,
    use_dev = use_dev,
    verbosity = verbosity,
    object_label = "Distribution Status"
  )
}

#' @name set_status
#' @rdname set_status
S7::method(set_status, Dataset) <- function(object, api_key = NULL, use_dev = TRUE, verbosity = 0) {
  api_key <- get_api_key(api_key)
  api_request_wrapper(
    object = object,
    method = "PATCH",
    endpoint = paste0("/api/v1/datasets/", object@id, "/set-status"),
    api_key = api_key,
    use_dev = use_dev,
    verbosity = verbosity,
    object_label = "Dataset Status"
  )
}

