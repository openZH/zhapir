#' Create a new object in the catalog
#'
#' @description
#' Generic method to create a new object in the data catalog via the API.
#'
#' @param object The object to create (Dataset or Distribution)
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
#'
#' @return The updated object with updated information from the API response
#' @keywords internal
update <- S7::new_generic("update", "object")


#' Set the status of a Dataset or Distribution via the "/set-status" endpoint
#'
#' @param object   A `Dataset` or `Distribution` S7 object with `@id` and `@status_id` set
#' @return Invisibly returns parsed response as a list.
#' @keywords internal
set_status <- S7::new_generic("set_status", "object")
