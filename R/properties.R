#’ Date-only property (YYYY-MM-DD)
#’
#’ @param default  A Date or NULL
#’ @param validator A function like validate_date()
prop_date <- function(default = NULL, validator = NULL) {
  S7::new_property(
    class     = S7::class_Date,
    default   = if (is.null(default)) as.Date(NA) else default,
    validator = validator
  )
}

prop_string <- function(default = NULL, validator = NULL, ...) {
  S7::new_property(
    class = S7::class_character,
    default = if (is.null(default)) {
      NA_character_
    } else {
      default
    },
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





