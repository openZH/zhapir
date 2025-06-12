#' Get All Organisation and Their IDs
#'
#' Retrieves a data frame containing all available organisation along with their
#' associated IDs. This function is typically used to look up valid organisation
#' options for other functions.
#'
#' @param show_organisation_units If `TRUE`, organisation units are returned in
#' addition to the organisations.
#'
#' @return A data frame with two columns: one for organisation names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_organisation <- get_organisations()
#' head(df_organisation)
#' }
get_organisations <- function(show_organisation_units = TRUE) {
  req <- api_request(
    method = "GET",
    endpoint = "/api/v1/organisations",
    api_key = get_api_key()
  )

  # create data frame with all organisation information
  df_organisation_info <- purrr::map_df(req, ~ {
    x <- .x

    # retrieve organisation information
    organisation_id <- x$id
    organisation <- x$name

    # FIXME: potentially use below chunk to retrieve organisation units
    # retrieve organisation unit information within an additional list of a
    # given organisation
    if (length(x$organisation_units) != 0) {
      df_organisation_unit <- purrr::map_df(x$organisation_units, ~ {
        orga_unit_id <- .x$id
        orga_unit <- .x$label

        data.frame(organisation_unit_id = orga_unit_id, organisation_unit = orga_unit)
      })
    }

    # Combine organisation and organisation unit information to a data frame
    if (exists("df_organisation_unit") & show_organisation_units == TRUE) {
      df <- data.frame(
        organisation_id = organisation_id,
        organisation = organisation,
        df_organisation_unit
      )
    } else {
      df <- data.frame(
        organisation_id = organisation_id,
        organisation = organisation
      )
    }
  })

  return(df_organisation_info)
}




#' Retrieve Keyword ID by Name
#'
#' This function returns the ID associated with a keyword by first retrieving a
#' data frame of keywords and then matching the provided name to its
#' corresponding ID.
#'
#' @param name A character string or vector specifying the name(s) of the keyword(s).
#'
#' @return An integer value or vector representing the ID(s) of the keyword(s).
#' @export
#'
#' @examples
#' \dontrun{
#' get_keywords_id("abfall")
#' }
get_keywords_id <- function(name) {
  df_keywords <- get_keywords()
  id <- get_id(df_keywords, name, internal = TRUE)

  return(invisible(id))
}


#' Get All Keywords and Their IDs
#'
#' Retrieves a data frame containing all available keywords along with their
#' associated IDs. This function is typically used to look up valid keyword
#' options for other functions.
#'
#' @return A data frame with two columns: one for keyword names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_keywords <- get_keywords()
#' head(df_keywords)
#' }
get_keywords <- function(name = NULL) {
  df_keywords <- req_to_df("keywords")

  if (!is.null(name)) {
    ids <- get_id(df_keywords, name, internal = FALSE)

    df_keywords <- df_keywords |>
      dplyr::filter(id %in% ids)
  }


  return(df_keywords)
}



#' Retrieve zh-web-catalog keyword ID by Name
#'
#' This function returns the ID associated with a zh-web-catalog keyword by
#' first retrieving a data frame of keywords and then matching the provided name
#' to its corresponding ID.
#'
#' @param name A character string or vector specifying the name(s) of the
#' zh-web-catalog keyword(s).
#'
#' @return An integer value or vector representing the ID(s) of the
#' zh-web-catalog keyword(s)
#' @export
#'
#' @examples
#' \dontrun{
#' get_zh_web_catalog_id("Bevölkerung")
#' }
get_zh_web_catalog_id <- function(name) {
  df_zh_web_catalog <- get_zh_web_catalog()
  id <- get_id(df_zh_web_catalog, name)

  return(invisible(id))
}


#' Get All zh-web-catalog keywords and Their IDs
#'
#' Retrieves a data frame containing all available zh-web-catalog keywords along
#' with their associated IDs. This function is typically used to look up valid
#' keyword options for other functions.
#'
#' @return A data frame with two columns: one for zh-web-catalog keyword names
#' and one for their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_zh_web_catalog <- get_zh_web_catalog()
#' head(df_zh_web_catalog)
#' }
get_zh_web_catalog <- function() {
  df_zh_web_catalog <- req_to_df("zhweb-datenkataloge")

  return(df_zh_web_catalog)
}




#' Retrieve Themes ID by Name
#'
#' This function returns the ID associated with a theme by first retrieving a
#' data frame of themes and then matching the provided name to its
#' corresponding ID.
#'
#' @param name A character string or vector specifying the name(s) of the theme(s).
#'
#' @return An integer value or vector representing the ID(s) of the theme(s).
#' @export
#'
#' @examples
#' \dontrun{
#' get_themes_id("Wirtschaft und Finanzen")
#' }
get_themes_id <- function(name) {
  df_theme <- get_themes()
  id <- get_id(df_theme, name)

  return(invisible(id))
}




#' Get All Themes and Their IDs
#'
#' Retrieves a data frame containing all available themes along with their
#' associated IDs. This function is typically used to look up valid theme
#' options for other functions.
#'
#' @return A data frame with two columns: one for theme names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_themes <- get_themes()
#' head(df_themes)
#' }
get_themes <- function() {
  df_themes <- req_to_df("themes")

  return(df_themes)
}




#' Retrieve Periodicity ID by Name
#'
#' This function returns the ID associated with a periodicity name by first
#' retrieving a data frame of periodicities and then matching the provided name
#' to its corresponding ID.
#'
#' @param name A character string or vector specifying the name(s) of the
#' periodicity (or periodicities).
#'
#' @return An integer value or vector representing the ID(s) of the periodicity
#' (or periodicities).
#' @export
#'
#' @examples
#' \dontrun{
#' get_periodicity_id("Jährlich")
#' }
get_periodicities_id <- function(name) {
  df_periodicity <- get_periodicities()
  id <- get_id(df_periodicity, name)

  return(invisible(id))
}




#' Get All Periodicities and Their IDs
#'
#' Retrieves a data frame containing all available periodicities along with their
#' associated IDs. This function is typically used to look up valid periodicity
#' options for other functions.
#'
#' @return A data frame with two columns: one for periodicity names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_periocidity <- get_periodicity()
#' head(df_periocidity)
#' }
get_periodicities <- function() {
  df_periocidity <- req_to_df("periodicities")

  return(df_periocidity)
}




#' Retrieve Status ID by Name
#'
#' This function returns the ID associated with a status name by first retrieving
#' a data frame of statuses and then matching the provided name to its
#' corresponding ID.
#'
#' @param name A character string or vector specifying the name(s) of the status
#' (or statuses).
#'
#' @return An integer value or vector representing the ID(s) of the status
#' (or statuses).
#' @export
#'
#' @examples
#' \dontrun{
#' get_statuses_id("Entwurf")
#' }
get_statuses_id <- function(name) {
  df_status <- get_statuses()
  id <- get_id(df_status, name)

  return(invisible(id))
}




#' Get All Statuses and Their IDs
#'
#' Retrieves a data frame containing all available statuses along with their
#' associated IDs. This function is typically used to look up valid status
#' options for other functions.
#'
#' @return A data frame with two columns: one for status names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_status <- get_statuses()
#' head(df_status)
#' }
get_statuses <- function() {
  df_status <- req_to_df("statuses")

  return(df_status)
}



#' Retrieve License ID by Name
#'
#' This function returns the ID associated with a license name by first retrieving
#' a data frame of licenses and then matching the provided name to its
#' corresponding ID.
#'
#' @param name A character string or vector specifying the name(s) of the license(s).
#'
#' @return An integer value or vector representing the ID(s) of the license(s).
#' @export
#'
#' @examples
#' \dontrun{
#' get_licenses_id("NonCommercialAllowed-CommercialAllowed-ReferenceRequired")
#' }
get_licenses_id <- function(name) {
  df_license <- get_licenses()
  id <- get_id(df_license, name)

  return(invisible(id))
}



#' Get All Licenses and Their IDs
#'
#' Retrieves a data frame containing all available licenses along with their
#' associated IDs. This function is typically used to look up valid license
#' options for other functions.
#'
#' @return A data frame with two columns: one for license names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_license <- get_licenses()
#' head(df_license)
#' }
get_licenses <- function() {
  df_license <- req_to_df("licenses")

  return(df_license)
}




#' Retrieve Format ID by Name
#'
#' This function returns the ID associated with a format name by first retrieving
#' a data frame of formats and then matching the provided name to its
#' corresponding ID.
#'
#' @param name A character string or vector specifying the name(s) of the format(s).
#'
#' @return An integer value or vector representing the ID(s) of the format(s).
#' @export
#'
#' @examples
#' \dontrun{
#' get_formats_id("CSV")
#' }
get_formats_id <- function(name) {
  df_format <- get_formats()
  id <- get_id(df_format, name)

  return(invisible(id))
}




#' Get All Formats and Their IDs
#'
#' Retrieves a data frame containing all available formats along with their
#' associated IDs. This function is typically used to look up valid format
#' options for other functions.
#'
#' @return A data frame with two columns: one for format names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_format <- get_formats()
#' head(df_format)
#' }
get_formats <- function() {
  df_format <- req_to_df("formats")

  return(df_format)
}




#' Retrieve a Data Frame from API Endpoint Request
#'
#' For a given Endpoint, this function retrieves the ID's and corresponding
#' Endpoint's entries and returns it as a data frame.
#'
#' @param endpoint A character string of a Kosmos API endpoint.
#'
#' @returns A data frame with two columns: one for endpoint entries and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_formats  <- req_to_df("formats")
#' head(df_formats)
#' }
req_to_df <- function(endpoint) {
  req <- api_request(
    method = "GET",
    endpoint = paste0("/api/v1/", endpoint),
    api_key = get_api_key()
  )

  df <- purrr::map_df(req, ~ {
    # retrieve keyword information
    id <- .x$id
    label <- .x$label
    setNames(data.frame(label, id, stringsAsFactors = FALSE), c(endpoint, "id"))
  })

  return(df)
}





#' Get the ID(s) of Entries for Given Variable from a Data Frame
#'
#' This function retrieves the ID(s) corresponding to one or more given name(s)
#' from a data frame that contains at least one non-ID column representing names
#' and an "id" column. It matches names exactly or partially and handles errors
#' if no or multiple matches are found.
#'
#' The function expects the data frame to have one column for names
#' (e.g., "keyword", "themes", etc.)  and an "id" column. It uses the name
#' column to filter and find the corresponding ID(s).
#'
#' @param name A character vector of one or more names to look up.
#' @param df A data frame containing at least one name column and an "id" column.
#'
#' @return A named vector of IDs corresponding to the input name(s).
#' Names of the vector correspond to the matched names in the data frame.
#'
#' @details
#' - If an exact match is found, its ID is returned.
#' - If multiple partial matches are found, an error is raised listing the possible matches.
#' - If no match is found, an error is raised suggesting a function to explore valid names.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df_formats <- get_formats()
#'
#' get_id(df_formats, "CSV")
#' # example of handling of invalid names
#' get_id(df_formats, c("CSV", "blabla"))
#' }
get_id <- function(df, name, internal) {
  browser()

  label_col <- rlang::sym(names(df)[names(df) != "id"])
  name <- tolower(name)

  ids <- c()

  for (i in name) {
    switch(as.character(label_col),
      "keywords" = {
        error_noun <- "keyword"
        fun_name <- "get_keywords()"
      },
      "zhweb-datenkataloge" = {
        error_noun <- "zh_web_catalog"
        fun_name <- "get_zh_web_catalog()"
      },
      "themes" = {
        error_noun <- "theme"
        fun_name <- "get_themes()"
      },
      "periodicities" = {
        error_noun <- "periodicities"
        fun_name <- "get_periodicities()"
      },
      "statuses" = {
        error_noun <- "status"
        fun_name <- "get_statuses()"
      },
      "licenses" = {
        error_noun <- "license"
        fun_name <- "get_licenses()"
      },
      ,
      "formats" = {
        error_noun <- "format"
        fun_name <- "get_formats()"
      }
    )

    df_filtered <- df |>
      dplyr::mutate(filter_col_lower = tolower(!!label_col)) |>
      dplyr::filter(grepl(i, filter_col_lower))

    df_exact_match <- df |>
      dplyr::mutate(filter_col_lower = tolower(!!label_col)) |>
      dplyr::filter(filter_col_lower == i)


    if (internal == TRUE) {
      single_id <- name_to_single_id(df_filtered,
                                   df_exact_match,
                                   name = i,
                                   label_col,
                                   error_noun,
                                   fun_name)

      ids <- c(ids, single_id)
    } else {
      multiple_ids <- name_to_multiple_ids(df_filtered,
                                           name = i,
                                           label_col,
                                           error_noun,
                                           fun_name)
      ids <- c(ids, multiple_ids)
    }
  }

return(ids)
}




name_to_single_id <- function(df_filtered,
                              df_exact_match,
                              name,
                              label_col,
                              error_noun,
                              fun_name) {

  if (nrow(df_exact_match) != 1) {

    if (nrow(df_filtered) == 0){
      stop(paste0(
        "'", name, "' is not a valid ", error_noun, ".",
        " To explore all '", error_noun, "' run '", fun_name, "'."
      ))

    } else {
      not_exact_match <- df_filtered |>
        dplyr::pull(!!label_col)

      stop(
        paste0(
          "For '", name, "' there is not an exact match. The available option(s) is/are:\n",
          paste0(not_exact_match, collapse = ", "),
          paste0("\nTo explore all '", error_noun, "' run '", fun_name, "'.")
        )
      )

    }
  } else {

    single_id <- df_exact_match |>
      dplyr::pull(id)
    names(single_id) <- df_exact_match |>
      dplyr::pull(!!label_col)

  }

  return(single_id)

}




name_to_multiple_ids <- function(df_filtered,
                                 name,
                                 label_col,
                                 error_noun,
                                 fun_name) {

  if (nrow(df_filtered) == 0) {
    stop(paste0(
      "'", i, "' is not a valid ", error_noun, ".",
      " To explore all '", error_noun, "' run '", fun_name, "'."
    ))
  } else {
    multiple_ids <- df_filtered %>%
      dplyr::pull(id)

    names(multiple_ids) <- df_filtered %>%
      dplyr::pull(!!label_col)
  }

  return(multiple_ids)

}

get_label <- function(df, id) {

  label_col <- rlang::sym(names(df)[names(df) != "id"])

  labels <- c()

  for (i in id) {

  single_label <- df |>
    dplyr::filter(id == i) |>
    dplyr::pull(!!label_col)

  if (length(single_label) == 0) {
    cli::cli_abort("No entry found for id: {.val {i}}")
  }

  labels <- c(labels, single_label)

  }

  return(labels)
}



converter <- function(df, input, internal) {
browser()

  label_col <- rlang::sym(names(df)[names(df) != "id"])

  if(is.character(input)) {
    ids <- get_id(df, input, internal)

    output <- df |>
      dplyr::filter(id %in% ids)

  } else {
    labels <- get_label(df, input)

    output <- df |>
      dplyr::filter(!!label_col %in% labels)

  }
  return(output)
}
