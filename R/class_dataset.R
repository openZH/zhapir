#' Dataset class for DCAT API
#'
#' @description
#' An S7 class representing a DCAT dataset in the data catalog.
#' Used for creating and updating datasets via the API.
#'
#' @details
#' This class implements the DCAT AP CH V2 standard for dataset metadata.
#' Each instance represents a single dataset in the catalog.
#'
#' @keywords internal
Dataset <- S7::new_class(
  "Dataset",
  package = "zhapir",
  properties = list(
    title = S7::new_property(class = S7::class_character),
    description = S7::new_property(class = S7::class_character),
    contact_id = S7::new_property(class = S7::class_integer),
    publisher_id = S7::new_property(class = S7::class_integer),
    status_id = S7::new_property(class = S7::class_integer),
    issued = S7::new_property(class = S7::class_character, default = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")),
    keyword_ids = S7::new_property(class = S7::class_list, default = list()),
    theme_ids = S7::new_property(class = S7::class_list, default = list()),
    landing_page = S7::new_property(class = S7::class_character, default = NA_character_),
    modified_next = S7::new_property(class = S7::class_character, default = NA_character_),
    relation_id = S7::new_property(class = S7::class_integer, default = NA_integer_),
    see_also_ids = S7::new_property(class = S7::class_list, default = list()),
    status = S7::new_property(class = S7::class_list, default = list()),
    temporal = S7::new_property(class = S7::class_list, default = list()),
    zhweb_catalog_ids = S7::new_property(class = S7::class_list, default = list())
  ),
  validator = function(self) {
    # Validate required fields
    if (length(self@title) != 1 || nchar(self@title) == 0) {
      return("Dataset must have a non-empty title")
    }
    if (length(self@description) != 1 || nchar(self@description) == 0) {
      return("Dataset must have a non-empty description")
    }
    if (is.na(self@contact_id)) {
      return("Dataset must have a contact_id")
    }
    if (is.na(self@publisher_id)) {
      return("Dataset must have a publisher_id")
    }
    if (is.na(self@status_id)) {
      return("Dataset must have a status_id")
    }

    # Additional validations could be added here
  }
)
