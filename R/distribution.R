#' Distribution
#'
#' Create a new Distribution object for the KOSMOS data catalog (DCAT standard).
#'
#' @param id                numeric; ID der Distribution (required for update)
#' @param title             character; distribution title (required on create, <=1000 characters)
#' @param dataset_id        numeric; ID of the related dataset (required)
#' @param stat_server_flag  logical; visibility flag for statistical server (optional)
#' @param zh_web_flag       logical; visibility flag for zh web portal (optional)
#' @param ogd_flag          logical; visibility flag for Open Government Data (optional)
#' @param sort_order        numeric; optional sort index for ordering multiple distributions
#' @param description       character; distribution description (optional, <=4000 characters)
#' @param access_url        character; URL to access the distribution (optional, must start with http:// or https://)
#' @param right             character; optional textual statement of usage rights
#' @param byte_size         numeric; size in bytes (optional, must be a positive integer)
#' @param status_id         numeric; status ID (optional)
#' @param license_id        numeric; license ID (optional)
#' @param format_id         numeric; file format ID (optional)
#' @param media_type_id     numeric; media type ID (optional)
#' @param periodicity_id    numeric; periodicity ID (optional)
#' @param file_upload_id    character; ID of the uploaded file (optional)
#'
#' @return An S7 `Distribution` object.
Distribution <- S7::new_class(
  "Distribution",
  package = "zhapir",
  properties = list(

    # Distribution ID (required for PATCH)
    id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Title (required for POST)
    title = prop_string(
      validator = validate_text,
      max_length = 1000L
    ),

    # Dataset ID (required for PATCH)
    dataset_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Flags (optional)
    stat_server_flag = prop_logical(),
    zh_web_flag = prop_logical(),
    ogd_flag = prop_logical(),

    # Sort order (optional)
    sort_order = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Description (optional)
    description = prop_string(
      validator = validate_text,
      max_length = 10000L
    ),

    # Access URL (optional)
    access_url = prop_string(
      validator = validate_url
    ),

    # Right (optional)
    right = prop_string(
      validator = validate_text
    ),

    # Byte size (optional)
    byte_size = prop_numeric(
      validator = validate_bytesize
    ),

    # Status ID (optional)
    status_id = prop_numeric(
      default = 1, #TODO ?
      validator = validate_id,
      allow_na = TRUE
    ),

    # License ID (optional)
    license_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Format ID (optional)
    format_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Media Type ID (optional)
    media_type_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Periodicity ID (optional)
    periodicity_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # File Upload ID (optional)
    file_upload_id = prop_string(
      validator = validate_text
    )
  ),
  constructor = function(
      id = S7::class_missing,
      title = S7::class_missing,
      dataset_id = S7::class_missing,
      stat_server_flag = S7::class_missing,
      zh_web_flag = S7::class_missing,
      ogd_flag = S7::class_missing,
      sort_order = S7::class_missing,
      description = S7::class_missing,
      access_url = S7::class_missing,
      right = S7::class_missing,
      byte_size = S7::class_missing,
      status_id = S7::class_missing,
      license_id = S7::class_missing,
      format_id = S7::class_missing,
      media_type_id = S7::class_missing,
      periodicity_id = S7::class_missing,
      file_upload_id = S7::class_missing) {
    S7::new_object(S7::S7_object(),
      id = id,
      title = title,
      dataset_id = dataset_id,
      stat_server_flag = stat_server_flag,
      zh_web_flag = zh_web_flag,
      ogd_flag = ogd_flag,
      sort_order = sort_order,
      description = description,
      access_url = access_url,
      right = right,
      byte_size = byte_size,
      status_id = convert_statuses_to_id(status_id),
      license_id = license_id,
      format_id = convert_formats_to_id(format_id),
      media_type_id = media_type_id,
      periodicity_id = convert_periodicities_to_id(periodicity_id)

      ,
      file_upload_id = file_upload_id
    )
  }
)
