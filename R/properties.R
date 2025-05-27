prop_posixct <- function(default = NULL) {
  S7::new_property(
    class = S7::class_POSIXct,
    default = if (is.null(default)) {
      as.POSIXct(NA)
    } else {
      default
    }
  )
}


prop_string <- function(validator = NULL, ...) {
  S7::new_property(
    class = S7::class_character,
    default = NA_character_,
    validator = function(value) validator(value, ...)
  )
}


prop_numeric <- function(default = NULL, validator = NULL, ...) {
  S7::new_property(
    class = S7::class_numeric,
    default = if (is.null(default)) {
      NA_real_
    } else {
      default
    },
    validator = function(value) validator(value, ...)
  )
}



prop_list <- function(default = NULL, validator = NULL) {
  S7::new_property(
    class = S7::class_list,
    default = if (is.null(default)) {
      list()
    } else {
      default
    },
    validator = function(value) validator(value)
  )
}


prop_logical <- function(default = NULL) {
  S7::new_property(
    class = S7::class_logical,
    default = if (is.null(default)) {
      NA
    } else {
      default
    }
  )
}





validate_text <- function(value, max_length = 1000L, field = "Feld") {
  if (!is.na(value) && nzchar(value) && nchar(value) > max_length) {
    return(paste(field, "darf maximal", max_length, "Zeichen lang sein."))
  }
}


validate_email <- function(value) {
  if (!is.na(value) && nzchar(value)) {
    if (!grepl("^[^@]+@[^@]+\\.[^@]+$", value)) {
      return("contact_email muss eine gültige E-Mail-Adresse sein.")
    }
  }
}


validate_url <- function(value, field = "URL") {
  if (!is.na(value) && nzchar(value)) {
    if (!grepl(
      "^https?://[[:alnum:].-]+\\.[A-Za-z]{2,}(/[[:alnum:].-]*)*$",
      value
    )) {
      return(
        paste0(
          field,
          " muss mit http:// oder https:// beginnen und eine gültige Domain haben"
        )
      )
    }
  }
}
