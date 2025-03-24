#' Update an existing dataset in the catalog
#'
#' @description
#' Updates an existing dataset in the data catalog with the specified properties.
#' Only the properties you provide will be updated - everything else will remain unchanged.
#'
#' @param auth_info Authentication information from login_to_api()
#' @param dataset_id ID of the dataset to update
#' @param title New title (optional)
#' @param description New description (optional)
#' @param modified_next New date for next expected modification (optional)
#' @param status_id New status ID (optional)
#' @param temporal Temporal coverage to update (list with start_date and/or end_time)
#' @param keyword_ids New keyword IDs (optional)
#' @param theme_ids New theme IDs (optional)
#' @param use_dev Whether to use the development environment (default: TRUE)
#' @param ... Additional dataset properties to update
#'
#' @return The updated dataset information as returned by the API
#' @export
#'
#' @examples
#' \dontrun{
#' # First login
#' auth_info <- login_to_api("admin@example.com", "admin")
#'
#' # Update just the title and description
#' update_dataset(
#'   auth_info,
#'   dataset_id = 123,
#'   title = "Updated Title",
#'   description = "Updated description"
#' )
#'
#' # Update the temporal coverage with a new end time
#' update_dataset(
#'   auth_info,
#'   dataset_id = 123,
#'   temporal = list(end_time = "2023-12-31T23:59:59Z")
#' )
#'
#' # Update multiple fields at once
#' update_dataset(
#'   auth_info,
#'   dataset_id = 123,
#'   title = "New Title",
#'   description = "New description",
#'   status_id = 2,
#'   modified_next = "2024-01-31",
#'   temporal = list(
#'     start_date = "2023-01-01T00:00:00Z",
#'     end_time = "2023-12-31T23:59:59Z"
#'   )
#' )
#' }
update_dataset <- function(auth_info,
                           dataset_id,
                           title = NULL,
                           description = NULL,
                           modified_next = NULL,
                           status_id = NULL,
                           temporal = NULL,
                           keyword_ids = NULL,
                           theme_ids = NULL,
                           use_dev = TRUE,
                           ...) {

  # Create a dataset object to hold the updates
  dataset <- Dataset()

  # Set properties that were provided
  if (!is.null(title)) dataset@title <- title
  if (!is.null(description)) dataset@description <- description
  if (!is.null(modified_next)) dataset@modified_next <- modified_next
  if (!is.null(status_id)) dataset@status_id <- as.integer(status_id)
  if (!is.null(temporal)) dataset@temporal <- temporal
  if (!is.null(keyword_ids)) dataset@keyword_ids <- keyword_ids
  if (!is.null(theme_ids)) dataset@theme_ids <- theme_ids

  # Handle additional properties from ...
  extra_args <- list(...)
  for (arg_name in names(extra_args)) {
    if (arg_name %in% names(S7::props(Dataset))) {
      value <- extra_args[[arg_name]]
      prop <- S7::props(Dataset)[[arg_name]]

      # Convert types if needed
      if (is.numeric(value) && S7::property_class(prop) == S7::class_integer) {
        value <- as.integer(value)
      }

      # Set the property
      dataset@.items[[arg_name]] <- value
    }
  }

  # Update in catalog
  update(dataset, dataset_id, auth_info, use_dev)
}
