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
    id = S7::new_property(
      class   = S7::class_numeric,
      default = NA_real_,
      validator = function(value) validate_id(value, allow_na = TRUE)
    ),

    # Titel optional (wird nur bei Erstellung geprüft)
    title = S7::new_property(
      class   = S7::class_character,
      default = NA_character_,
      validator = function(value) {
        if (!is.na(value) && nzchar(value)) {
          if (length(value) != 1L) {
            return("title muss eine nicht-leere Zeichenkette sein.")
          }
          if (nchar(value) > 1000L) {
            return("title darf maximal 1000 Zeichen lang sein.")
          }
        }
      }
    ),

    # Organisation ID (required)
    organisation_id = S7::new_property(
      class     = S7::class_numeric,
      validator = function(value) validate_id(value, allow_na = FALSE)
    ),

    # Optionale Beschreibung und Kontakt
    description = S7::new_property(
      class   = S7::class_character,
      default = NA_character_
    ),
    contact_email = S7::new_property(
      class   = S7::class_character,
      default = NA_character_,
      validator = function(value) {
        if (!is.na(value) && nzchar(value)) {
          if (!grepl("^[^@]+@[^@]+\\.[^@]+$", value)) {
            return("contact_email muss eine gültige E-Mail-Adresse sein.")
          }
        }
      }
    ),

    # Weblink
    landing_page = S7::new_property(
      class   = S7::class_character,
      default = NA_character_,
      validator = function(value) {
        if (!is.na(value) && nzchar(value)) {
          if (!grepl(
            "^https?://[[:alnum:].-]+\\.[A-Za-z]{2,}(/[[:alnum:].-]*)*$",
            value
          )) {
            return("landing_page muss mit http:// oder https:// beginnen und eine gültige Domain haben")
          }
        }
      }
    ),

    # Zeitpunkte (optional)
    issued        = S7::new_property(class = S7::class_POSIXct, default = as.POSIXct(NA)),
    start_date    = S7::new_property(class = S7::class_POSIXct, default = as.POSIXct(NA)),
    end_date      = S7::new_property(class = S7::class_POSIXct, default = as.POSIXct(NA)),
    modified      = S7::new_property(class = S7::class_POSIXct, default = as.POSIXct(NA)),
    modified_next = S7::new_property(class = S7::class_POSIXct, default = as.POSIXct(NA)),

    # Relations- und Katalog-IDs
    keyword_ids        = S7::new_property(
      class   = S7::class_list,
      default = list(),
      validator = function(value) validate_natural_number_list(value)
    ),
    zh_web_catalog_ids = S7::new_property(
      class   = S7::class_list,
      default = list(),
      validator = function(value) validate_natural_number_list(value)
    ),
    relation_ids = S7::new_property(
      class   = S7::class_list,
      default = list(),
      validator = function(value) validate_natural_number_list(value)
    ),
    see_also_ids = S7::new_property(
      class   = S7::class_list,
      default = list(),
      validator = function(value) validate_natural_number_list(value)
    ),
    theme_ids = S7::new_property(
      class   = S7::class_list,
      default = list(),
      validator = function(value) validate_natural_number_list(value)
    ),

    # Periodizität
    periodicity_id = S7::new_property(
      class   = S7::class_numeric,
      default = NA_real_,
      validator = function(value) validate_id(value, allow_na = TRUE)
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
