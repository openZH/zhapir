#' Distribution
#'
#' Create a new Distribution object for the KOSMOS data catalog (DCAT standard).
#'
#' @param title             character; distribution title (required on create, <=1000 characters)
#' @param dataset_id        numeric; ID of the related dataset (required)
#' @param stat_server_flag  logical; visibility flag for statistical server (optional)
#' @param zh_web_flag       logical; visibility flag for zh web portal (optional)
#' @param ogd_flag          logical; visibility flag for Open Government Data (optional)
#' @param sort_order        numeric; optional sort index for ordering multiple distributions
#' @param description       character; distribution description (optional, <=4000 characters)
#' @param modified          POSIXct or ISO datetime string; timestamp of last modification (optional)
#' @param access_url        character; URL to access the distribution (optional, must start with http:// or https://)
#' @param right             character; optional textual statement of usage rights
#' @param issued            POSIXct or ISO datetime string; publication date of the distribution (optional)
#' @param byte_size         numeric; size in bytes (optional, must be a positive integer)
#' @param license_id        numeric; license ID (optional)
#' @param format_id         numeric; file format ID (optional)
#' @param media_type_id     numeric; media type ID (optional)
#' @param periodicity_id    numeric; periodicity ID (optional)
#' @param file_upload_id    character; ID of the uploaded file (optional)
#'
#' @return An S7 `Distribution` object.
#' @export
Distribution <- S7::new_class(
  "Distribution",
  package = "zhapir",
  properties = list(

    # Title (required)
    title = S7::new_property(
      class = S7::class_character,
      default = NA_character_,
      validator = function(value) validate_optional_text(value, max = 1000, field = "title")
    ),

    # Dataset ID (required)
    dataset_id = S7::new_property(
      class = S7::class_numeric,
      validator = function(value) validate_id(value, allow_na = FALSE)
    ),

    # Flags (optional)
    stat_server_flag = S7::new_property(
      class = S7::class_logical,
      default = NA
    ),
    zh_web_flag = S7::new_property(
      class = S7::class_logical,
      default = NA
    ),
    ogd_flag = S7::new_property(
      class = S7::class_logical,
      default = NA
    ),

    # Sort order (optional)
    sort_order = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_,
      validator = function(value) validate_id(value, allow_na = TRUE)
    ),

    # Description (optional)
    description = S7::new_property(
      class = S7::class_character,
      default = NA_character_,
      validator = function(value) validate_optional_text(value, max = 10000, field = "description") # TODO max?
    ),

    # Modified (optional)
    modified = S7::new_property(
      class = S7::class_POSIXct,
      default = as.POSIXct(NA)
    ),

    # Access URL (optional)
    access_url = S7::new_property(
      class = S7::class_character,
      default = NA_character_,
      validator = function(value) validate_url(value, field = "access_url")
    ),

    # Right (optional)
    right = S7::new_property(
      class = S7::class_character,
      default = NA_character_
    ),

    # Issued (optional)
    issued = S7::new_property(
      class = S7::class_POSIXct,
      default = as.POSIXct(NA)
    ),

    # Byte size (optional)
    byte_size = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_,
    ),


    # License ID (optional)
    license_id = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_,
      validator = function(value) validate_id(value, allow_na = TRUE)
    ),

    # Format ID (optional)
    format_id = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_,
      validator = function(value) validate_id(value, allow_na = TRUE)
    ),

    # Media Type ID (optional)
    media_type_id = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_,
      validator = function(value) validate_id(value, allow_na = TRUE)
    ),

    # Periodicity ID (optional)
    periodicity_id = S7::new_property(
      class = S7::class_numeric,
      default = NA_real_,
      validator = function(value) validate_id(value, allow_na = TRUE)
    ),

    # File Upload ID (optional)
    file_upload_id = S7::new_property(
      class = S7::class_character,
      default = NA_character_
    )
  ),

)
