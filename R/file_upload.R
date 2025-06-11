#' FileUpload class
#'
#' @description
#' Defines a new `FileUpload` object representing a local file to be uploaded
#' via the API.
#'
#' @param file_path Path to the local file to upload (must exist).
#'
#' @return An S7 `FileUpload` object.
FileUpload <- S7::new_class(
  "FileUpload",
  package = "zhapir",
  properties = list(
    file_path = prop_string(validator = validate_text)
  ),
  constructor = function(file_path = S7::class_missing) {
    if (!file.exists(file_path)) stop("File doesn't exist: ", file_path)
    S7::new_object(S7::S7_object(), file_path = file_path)
  }
)
