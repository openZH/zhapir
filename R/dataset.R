#' Dataset
#'
#' Create a new Dataset object for the MDV data catalog (DCAT standard).
#'
#' @param id                numeric; the dataset ID (server-generated; use `NA_real_` when creating)
#' @param title             character; dataset title (required on create, <=1000 characters)
#' @param organisation_id   numeric; organisation ID (required)
#' @param description       character; dataset description (optional)
#' @param contact_email     character; contact email (optional, must be a valid email)
#' @param landing_page      character; landing page URL (optional, must start with http:// or https://)
#' @param issued            POSIXct or ISO datetime string; publication date (optional)
#' @param start_date        POSIXct or ISO datetime string; start of timeseries (optional)
#' @param end_date          POSIXct or ISO datetime string; end of timeseries (optional)
#' @param modified          POSIXct or ISO datetime string; last modification timestamp (optional)
#' @param modified_next     POSIXct or ISO datetime string; next modification timestamp (optional)
#' @param keyword_ids       integer vector; keyword IDs (optional)
#' @param zh_web_catalog_ids integer vector; web catalog IDs (optional)
#' @param relation_ids      integer vector; relation IDs (optional)
#' @param see_also_ids      integer vector; see-also IDs (optional)
#' @param theme_ids         integer vector; theme IDs (optional)
#' @param periodicity_id    numeric; periodicity ID (optional)
#'
#' @return An S7 `Dataset` object.
#' @export
Dataset <- S7::new_class(
  "Dataset",
  package = "zhapir",
  properties = list(
    # ID des Datasets (wird serverseitig generiert)
    id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    ),

    # Titel optional (wird nur bei Erstellung geprüft)
    title = prop_string(
      validator = validate_text
    ),

    # Organisation ID (required)
    organisation_id = prop_numeric(
      validator = validate_id,
      allow_na = FALSE
    ),

    # Optionale Beschreibung und Kontakt
    description = prop_string(
      validator = validate_text,
      max_length = 10000L
    ),
    contact_email = prop_string(
      validator = validate_email
    ),

    # Weblink
    landing_page = prop_string(
      validator = validate_url
    ),

    # Zeitpunkte (optional)
    issued = prop_posixct(),
    start_date = prop_posixct(),
    end_date = prop_posixct(),
    modified = prop_posixct(),
    modified_next = prop_posixct(),

    # Relations- und Katalog-IDs
    keyword_ids = prop_list(
      validator = validate_natural_number_list
    ),
    zh_web_catalog_ids = prop_list(
      validator = validate_natural_number_list
    ),
    relation_ids = prop_list(
      validator = validate_natural_number_list
    ),
    see_also_ids = prop_list(
      validator = validate_natural_number_list
    ),
    theme_ids = prop_list(
      validator = validate_natural_number_list
    ),

    # Periodizität
    periodicity_id = prop_numeric(
      validator = validate_id,
      allow_na = TRUE
    )
  ),
  # Klasseneigene Validierung für Datum-Logik
  validator = function(self) {
    sd <- self@start_date
    ed <- self@end_date
    if (!is.na(sd) && !is.na(ed)) {
      if (as.Date(sd) > as.Date(ed)) {
        return(sprintf(
          "end_date (%s) muss gleich oder nach start_date (%s) sein",
          as.Date(ed),
          as.Date(sd)
        ))
      }
    }
  },
  constructor = function(
      id = S7::class_missing,
      title = S7::class_missing,
      organisation_id = S7::class_missing,
      description = S7::class_missing,
      contact_email = S7::class_missing,
      landing_page = S7::class_missing,
      issued = S7::class_missing,
      start_date = S7::class_missing,
      end_date = S7::class_missing,
      modified = S7::class_missing,
      modified_next = S7::class_missing,
      keyword_ids = S7::class_missing,
      zh_web_catalog_ids = S7::class_missing,
      relation_ids = S7::class_missing,
      see_also_ids = S7::class_missing,
      theme_ids = S7::class_missing,
      periodicity_id = S7::class_missing) {
    S7::new_object(S7::S7_object(),
      id = id,
      title = title,
      organisation_id = organisation_id,
      description = description,
      contact_email = contact_email,
      landing_page = landing_page,
      issued = to_POSIXct(issued),
      start_date = to_POSIXct(start_date),
      end_date = to_POSIXct(end_date),
      modified = to_POSIXct(modified),
      modified_next = to_POSIXct(modified_next),
      keyword_ids = to_list(convert_keywords_to_id(keyword_ids)),
      zh_web_catalog_ids = to_list(convert_zh_web_catalog_to_id(zh_web_catalog_ids)),
      relation_ids = to_list(relation_ids),
      see_also_ids = to_list(convert_datasets_to_id(see_also_ids)),
      theme_ids = to_list(convert_themes_to_id(theme_ids)),
      periodicity_id = convert_periodicities_to_id(periodicity_id)
    )
  }
)



