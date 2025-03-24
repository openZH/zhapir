#' Create a new object in the catalog
#'
#' @description
#' Generic method to create a new object in the data catalog via the API.
#'
#' @param object The object to create (Dataset or Distribution)
#' @param auth_info Authentication information from login_to_api()
#' @param use_dev Whether to use the development environment
#'
#' @return The created object with updated information from the API response
#' @keywords internal
create <- S7::new_generic("create", "object")
