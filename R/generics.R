#' Create a new object in the catalog
#'
#' @description
#' Generic method to create a new object in the data catalog via the API.
#'
#' @param object The object to create (Dataset or Distribution)
#' @param api_key Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment
#'
#' @return The created object with updated information from the API response
#' @keywords internal
create <- S7::new_generic("create", "object")



#' Update an existing object in the catalog
#'
#' @description
#' Generic method to update an existing object in the data catalog via the API.
#'
#' @param object The object to update
#' @param id ID of the object to update
#' @param api_key Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment
#'
#' @return The updated object with updated information from the API response
#' @keywords internal
update <- S7::new_generic("update", "object")


#' Set the status of a Dataset or Distribution via the "/set-status" endpoint
#'
#' @param object   A `Dataset` or `Distribution` S7 object with `@id` and `@status_id` set
#' @param api_key  API key string for authentication (optional)
#' @param use_dev  Logical; use the development API endpoint (default `TRUE`)
#' @param verbosity Integer; httr2 verbosity level (default `0`)
#'
#' @return Invisibly returns parsed response as a list.
#' @keywords internal
set_status <- S7::new_generic("set_status", "object")
