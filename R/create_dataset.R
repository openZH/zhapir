#' Create a new dataset in the catalog
#'
#' @description
#' Creates a new dataset in the data catalog with the specified properties.
#'
#' @param auth_info Authentication information from login_to_api()
#' @param title Title of the dataset
#' @param description Description of the dataset
#' @param contact_id ID of the contact person
#' @param publisher_id ID of the publishing organization
#' @param status_id Status ID for the dataset
#' @param issued Date of publication (default: current time)
#' @param keyword_ids List of keyword IDs
#' @param theme_ids List of theme IDs
#' @param use_dev Whether to use the development environment (default: TRUE)
#' @param ... Additional dataset properties
#'
#' @return The created dataset information as returned by the API
#' @export
#'
#' @examples
#' \dontrun{
#' # First login
#' auth_info <- login_to_api("admin@example.com", "admin")
#'
#' # Create a dataset with required fields
#' create_dataset(
#'   auth_info,
#'   title = "Air Quality Measurements 2023",
#'   description = "Hourly air quality measurements for Canton Zurich",
#'   contact_id = 1,
#'   publisher_id = 1,
#'   status_id = 1
#' )
#' }
create_dataset <- function(auth_info,
                           title,
                           description,
                           contact_id,
                           publisher_id,
                           status_id,
                           issued = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
                           keyword_ids = list(),
                           theme_ids = list(),
                           use_dev = TRUE,
                           ...) {

  # Create the Dataset object
  dataset <- Dataset(
    title = title,
    description = description,
    contact_id = as.integer(contact_id),
    publisher_id = as.integer(publisher_id),
    status_id = as.integer(status_id),
    issued = issued,
    keyword_ids = keyword_ids,
    theme_ids = theme_ids,
    ...
  )

  # Create in catalog
  create(dataset, auth_info, use_dev)
}
