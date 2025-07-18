#’ Validate that a value is a single Date, POSIXct, or ISO-8601 “YYYY-MM-DD”
#’
#’ @param value A vector to validate
#’ @return NULL if OK, or an error string
validate_date <- function(value) {
  if (inherits(value, "Date") && length(value) == 1L) {
    return(NULL)
  }
  if (inherits(value, "POSIXct") && length(value) == 1L) {
    return(NULL)
  }
  if (is.character(value) && length(value) == 1L &&
      grepl("^\\d{4}-\\d{2}-\\d{2}$", value)) {
    return(NULL)
  }
  "must be a single Date, POSIXct, or YYYY-MM-DD string"
}



# Helper function to validate ID fields
validate_id <- function(value, allow_na = TRUE) {
  if (length(value) != 1) {
    return("must have exactly one value")
  }
  if (is.na(value)) {
    if (!allow_na) {
      return("cannot be NA")
    }
  } else {
    if (value < 0) {
      return("must be a positive number")
    }
    if (value != floor(value)) {
      return("must be a whole number")
    }
  }
  return(NULL)
}

validate_bytesize <- function(value){
  if (length(value) != 1) {
    return("must have exactly one value")
  }
}


validate_natural_number_list <- function(value) {
  if (length(value) > 0) {
    # Check if all elements are numeric
    if (!all(sapply(value, is.numeric))) {
      return("all elements must be numeric")
    }

    # Check if all elements are natural numbers
    non_natural <- sapply(value, function(x) !is.na(x) && (x < 1 || x != floor(x)))
    if (any(non_natural)) {
      return("all elements must be positive integers")
    }
  }
}


validate_text <- function(value, max_length = 1000L) {
  # FIXME: is an empty string allowed?
  if (!is.na(value) && nzchar(value) && nchar(value) > max_length) {
    return(paste("can have a maximum of", max_length, "characters"))
  }
  return(NULL)
}


validate_url <- function(value) {
  if (!is.na(value) && nzchar(value)) {
    if (!grepl("^https?://[[:alnum:].-]+\\.[A-Za-z]{2,}(/[[:alnum:]._~%-]*)*$", value)) {
      return("must start with http:// or https:// and must have a valid domain")
    }
  }
  return(NULL)
}



validate_email <- function(value) {
  if (!is.na(value) && nzchar(value)) {
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", value)) {
      return("must be a valid address.")
    }
  }
}


validate_file_exists <- function(value) {
  if (!file.exists(value)) {
    return(paste0("File doesn't exist: ", value))
  }
}

