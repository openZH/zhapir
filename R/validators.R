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


validate_optional_text <- function(value, max = 1000, field = "Feld") {
  if (!is.na(value) && nzchar(value) && nchar(value) > max) {
    return(paste(field, "darf maximal", max, "Zeichen lang sein."))
  }
  return(NULL)
}


validate_url <- function(value, field = "URL") {
  if (!is.na(value) && nzchar(value)) {
    if (!grepl("^https?://[[:alnum:].-]+\\.[A-Za-z]{2,}(/[[:alnum:]._~%-]*)*$", value)) {
      return(paste(field, "muss mit http:// oder https:// beginnen und eine gültige Domain haben"))
    }
  }
  return(NULL)
}
