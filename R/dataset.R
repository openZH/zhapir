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
      validator = validate_text,
      field = "title"
    ),

    # Organisation ID (required)
    organisation_id = prop_numeric(
      validator = validate_id,
      allow_na = FALSE
    ),

    # Optionale Beschreibung und Kontakt
    description = prop_string(
      validator = validate_text,
      max_length = 10000L,
      field = "description"
    ),

    contact_email = prop_string(
      validator = validate_email
    ),

    # Weblink
    landing_page = prop_string(
      validator = validate_url,
      field = "landing_page"
    ),

    # Zeitpunkte (optional)
    issued        = prop_posixct(
      default = as.POSIXct(NA)
    ),
    start_date    = prop_posixct(),
    end_date      = prop_posixct(),
    modified      = prop_posixct(),
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
  }
)
