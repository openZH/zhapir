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
#' @param description       character; distribution description (optional, <=4000 characters)
#' @param access_url        character; URL to access the distribution (optional, must start with http:// or https://)
#' @param byte_size         numeric; size in bytes (optional, must be a positive integer)
#' @param status_id         character; status ID (optional)
#' @param license_id        numeric; license ID (optional)
#' @param file_format_id    character; file format ID (optional)
#' @param periodicity_id    character; periodicity ID (optional)
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

    # Description (optional)
    description = prop_string(
      validator = validate_text,
      max_length = 10000L
    ),

    # Access URL (optional)
    access_url = prop_string(
      validator = validate_url
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
    file_format_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Periodicity ID (optional)
    periodicity_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # File Upload ID (optional)
    file_upload_id = prop_numeric(
      validator = validate_id
    )
  ),
  constructor = function(
      id = S7::class_missing,
      title = S7::class_missing,
      dataset_id = S7::class_missing,
      stat_server_flag = S7::class_missing,
      zh_web_flag = S7::class_missing,
      ogd_flag = S7::class_missing,
      description = S7::class_missing,
      access_url = S7::class_missing,
      byte_size = S7::class_missing,
      status_id = S7::class_missing,
      license_id = S7::class_missing,
      file_format_id = S7::class_missing,
      periodicity_id = S7::class_missing,
      file_upload_id = S7::class_missing) {



    ## turn any explicit NULL into class_missing —
    args <- as.list(environment())
    for (nm in names(args)) {
      if (is.null(args[[nm]])) {
        assign(nm, S7::class_missing, envir = environment())
      }
    }

    # set defaults to surpass validation
    if (identical(id, S7::class_missing))               id             <- NA_real_
    if (identical(title, S7::class_missing))            title          <- NA_character_
    if (identical(dataset_id, S7::class_missing))       dataset_id     <- NA_real_
    if (identical(stat_server_flag, S7::class_missing)) stat_server_flag <- NA
    if (identical(zh_web_flag, S7::class_missing))      zh_web_flag    <- NA
    if (identical(ogd_flag, S7::class_missing))         ogd_flag       <- NA
    if (identical(description, S7::class_missing))      description    <- NA_character_
    if (identical(access_url, S7::class_missing))       access_url     <- NA_character_
    if (identical(byte_size, S7::class_missing))        byte_size      <- NA_real_
    if (identical(status_id, S7::class_missing))        status_id      <- NA_real_
    if (identical(license_id, S7::class_missing))       license_id     <- NA_real_
    if (identical(file_format_id, S7::class_missing))   file_format_id <- NA_real_
    if (identical(periodicity_id, S7::class_missing))   periodicity_id <- NA_real_
    if (identical(file_upload_id, S7::class_missing))   file_upload_id <- NA_real_


    S7::new_object(S7::S7_object(),
      id = id,
      title = title,
      dataset_id = dataset_id,
      stat_server_flag = stat_server_flag,
      zh_web_flag = zh_web_flag,
      ogd_flag = ogd_flag,
      description = description,
      access_url = access_url,
      byte_size = byte_size,
      status_id = status_id,
      license_id = license_id,
      file_format_id = file_format_id,
      periodicity_id = periodicity_id,
      file_upload_id = file_upload_id
    )
  }
)
