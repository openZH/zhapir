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
