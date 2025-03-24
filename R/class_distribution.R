#' Distribution class for DCAT API
#'
#' @description
#' An S7 class representing a DCAT distribution (file or access point)
#' associated with a dataset in the catalog.
#'
#' @details
#' This class implements the DCAT AP CH V2 standard for distribution metadata.
#'
#' @keywords internal
Distribution <- S7::new_class(
  "Distribution",
  package = "zhapir",
  properties = list(
    # Required fields
    title = S7::new_property(class = S7::class_character),
    description = S7::new_property(class = S7::class_character),
    dataset_id = S7::new_property(class = S7::class_integer),

    # Optional fields with defaults
    access_url_id = S7::new_property(class = S7::class_character, default = NA_character_),
    byte_size = S7::new_property(class = S7::class_integer, default = -1L),
    download_url_id = S7::new_property(class = S7::class_character, default = NA_character_),
    format_id = S7::new_property(class = S7::class_integer, default = NA_integer_),
    identifier = S7::new_property(class = S7::class_character, default = NA_character_),
    issued = S7::new_property(class = S7::class_character, default = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    media_type_id = S7::new_property(class = S7::class_integer, default = NA_integer_),
    ogd_flag = S7::new_property(class = S7::class_logical, default = TRUE),
    right_id = S7::new_property(class = S7::class_integer, default = NA_integer_),
    sort_order = S7::new_property(class = S7::class_integer, default = 0L),
    stat_server_flag = S7::new_property(class = S7::class_logical, default = TRUE),
    status_id = S7::new_property(class = S7::class_integer, default = NA_integer_),
    zhweb_flag = S7::new_property(class = S7::class_logical, default = TRUE)
  ),
  validator = function(self) {
    # Validate required fields
    if (length(self@title) != 1 || nchar(self@title) == 0) {
      return("Distribution must have a non-empty title")
    }
    if (length(self@description) != 1 || nchar(self@description) == 0) {
      return("Distribution must have a non-empty description")
    }
    if (is.na(self@dataset_id)) {
      return("Distribution must be associated with a dataset")
    }

    # Validate specific constraints
    if (!is.na(self@byte_size) && self@byte_size < -1) {
      return("byte_size must be greater than or equal to -1")
    }
    if (!is.na(self@sort_order) && self@sort_order < 0) {
      return("sort_order must be greater than or equal to 0")
    }

    # Additional validations could be added here
  }
)
