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
#' @param identifier        character; optional string identifier for the distribution
#' @param right             character; optional textual statement of usage rights
#' @param issued            POSIXct or ISO datetime string; publication date of the distribution (optional)
#' @param byte_size         numeric; size in bytes (optional, must be a positive integer)
#' @param status_id         numeric; status ID (optional, default is 1)
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
    title = prop_string(
      validator = "text",
      max_length = 1000L
    ),

    # Dataset ID (required)
    dataset_id = prop_numeric(
      validator = "id",
      allow_na = FALSE
    ),

    # Flags (optional)
    stat_server_flag = prop_logical(),
    zh_web_flag = prop_logical(),
    ogd_flag = prop_logical(),

    # Sort order (optional)
    sort_order = prop_numeric(
      validator = "id",
      allow_na = TRUE
    ),

    # Description (optional)
    description = prop_string(
      validator = "text",
      max_length = 10000L
    ),

    # Modified (optional)
    modified = prop_posixct(),

    # Access URL (optional)
    access_url = prop_string(
      validator = "url"
    ),

    # Identifier (optional)
    identifier = prop_string(),

    # Right (optional)
    right = prop_string(),

    # Issued (optional)
    issued = prop_posixct(),

    # Byte size (optional)
    byte_size = prop_numeric(),

    # Status ID (optional)
    status_id = prop_numeric(
      default = 1,
      validator = "id",
      allow_na = TRUE
    ),

    # License ID (optional)
    license_id = prop_numeric(
      validator = "id",
      allow_na = TRUE
    ),

    # Format ID (optional)
    format_id = prop_numeric(
      validator = "id",
      allow_na = TRUE
    ),

    # Media Type ID (optional)
    media_type_id = prop_numeric(
      validator = "id",
      allow_na = TRUE
    ),

    # Periodicity ID (optional)
    periodicity_id = prop_numeric(
      validator = "id",
      allow_na = TRUE
    ),

    # File Upload ID (optional)
    file_upload_id = prop_string()
  ),
  constructor = function(
      title = S7::class_missing,
      dataset_id = S7::class_missing,
      stat_server_flag = S7::class_missing,
      zh_web_flag = S7::class_missing,
      ogd_flag = S7::class_missing,
      sort_order = S7::class_missing,
      description = S7::class_missing,
      modified = S7::class_missing,
      access_url = S7::class_missing,
      identifier = S7::class_missing,
      right = S7::class_missing,
      issued = S7::class_missing,
      byte_size = S7::class_missing,
      status_id = S7::class_missing,
      license_id = S7::class_missing,
      format_id = S7::class_missing,
      media_type_id = S7::class_missing,
      periodicity_id = S7::class_missing,
      file_upload_id = S7::class_missing) {
    S7::new_object(S7::S7_object(),
      title = title,
      dataset_id = dataset_id,
      stat_server_flag = stat_server_flag,
      zh_web_flag = zh_web_flag,
      ogd_flag = ogd_flag,
      sort_order = sort_order,
      description = description,
      modified = to_POSIXct(modified),
      access_url = access_url,
      identifier = identifier,
      right = right,
      issued = to_POSIXct(issued),
      byte_size = byte_size,
      status_id = status_id,
      license_id = license_id,
      format_id = format_id,
      media_type_id = media_type_id,
      periodicity_id = periodicity_id,
      file_upload_id = file_upload_id
    )
  }
)
