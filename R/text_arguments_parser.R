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
#' convert_keywords_to_id("abfall")
#' }
convert_keywords_to_id <- function(name) {
  if(inherits(name, "S7_missing")){
    S7::class_missing
  } else {
  df_keywords <- get_keywords()
  id <- get_id(df_keywords, name, internal = TRUE)

  return(id)
  }
}


#' Get All Keywords and Their IDs
#'
#' Retrieves a data frame containing all available keywords along with their
#' associated IDs. This function is typically used to look up valid keyword
#' options for other functions.
#'
#' @param input Optional. A character string, numeric value, or a vector of either.
#' Used to filter the keyword dataset. If NULL (default), returns the full dataset.
#'
#' @return A data frame with two columns: one for keyword names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_keywords <- get_keywords()
#' head(df_keywords)
#'
#' get_keywords("abwasser")
#'
#' get_keyords(c(578, 590))
#'
#' }
get_keywords <- function(input = NULL) {

  df_keywords <- req_to_df("keywords")

  if (!is.null(input)) {
    df_keywords <- df_keywords |>
      converter(input, internal = FALSE)
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
#' convert_zh_web_catalog_to_id("Bevölkerung")
#' }
convert_zh_web_catalog_to_id <- function(name) {
  if(inherits(name, "S7_missing")){
    S7::class_missing
  } else {
  df_zh_web_catalog <- get_zh_web_catalog()
  id <- get_id(df_zh_web_catalog, name, internal = TRUE)

  return(id)
  }
}


#' Get All zh-web-catalog keywords and Their IDs
#'
#' Retrieves a data frame containing all available zh-web-catalog keywords along
#' with their associated IDs. This function is typically used to look up valid
#' keyword options for other functions.
#'
#' @param input Optional. A character string, numeric value, or a vector of either.
#' Used to filter the zh-web-catalog dataset. If NULL (default), returns the full
#' dataset.
#'
#' @return A data frame with two columns: one for zh-web-catalog keyword names
#' and one for their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_zh_web_catalog <- get_zh_web_catalog()
#' head(df_zh_web_catalog)
#'
#' get_zh_web_catalog("Wasserdaten")
#'
#' get_zh_web_catalog(13)
#' }
get_zh_web_catalog <- function(input = NULL) {
  df_zh_web_catalog <- req_to_df("zhweb-datenkataloge")

  if (!is.null(input)) {
    df_zh_web_catalog <- df_zh_web_catalog |>
      converter(input, internal = FALSE)
  }

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
#' convert_themes_to_id("Wirtschaft und Finanzen")
#' }
convert_themes_to_id <- function(name) {
  if(inherits(name, "S7_missing")){
    S7::class_missing
  } else {
  df_theme <- get_themes()
  id <- get_id(df_theme, name, internal = TRUE)

  return(id)
  }
}




#' Get All Themes and Their IDs
#'
#' Retrieves a data frame containing all available themes along with their
#' associated IDs. This function is typically used to look up valid theme
#' options for other functions.
#'
#' @param input Optional. A character string, numeric value, or a vector of either.
#' Used to filter themes dataset. If NULL (default), returns the full dataset.
#'
#' @return A data frame with two columns: one for theme names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_themes <- get_themes()
#' head(df_themes)
#'
#' get_themes("Verkehr")
#'
#' get_themes(41)
#'
#' }
get_themes <- function(input = NULL) {
  df_themes <- req_to_df("themes")

  if (!is.null(input)) {
    df_themes <- df_themes |>
      converter(input, internal = FALSE)
  }

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
#' convert_periodicities_to_id("Jährlich")
#' }
convert_periodicities_to_id <- function(name) {
  if(inherits(name, "S7_missing")){
    S7::class_missing
  } else {
  df_periodicity <- get_periodicities()
  id <- get_id(df_periodicity, name, internal = TRUE)

  return(id)
  }
}




#' Get All Periodicities and Their IDs
#'
#' Retrieves a data frame containing all available periodicities along with their
#' associated IDs. This function is typically used to look up valid periodicity
#' options for other functions.
#'
#' @param input Optional. A character string, numeric value, or a vector of either.
#' Used to filter periodicity dataset. If NULL (default), returns the full dataset.
#'
#' @return A data frame with two columns: one for periodicity names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_periocidities <- get_periodicities()
#' head(df_periocidities)
#'
#' get_periodicities("Jährlich")
#'
#' get_periodicities(42)
#' }
get_periodicities <- function(input = NULL) {
  df_periocidity <- req_to_df("periodicities")

  if (!is.null(input)) {
    df_periocidity <- df_periocidity |>
      converter(input, internal = FALSE)
  }

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
#' convert_statuses_to_id("Entwurf")
#' }
convert_statuses_to_id <- function(name) {
  if(inherits(name, "S7_missing")){
    S7::class_missing
  } else {
  df_status <- get_statuses()
  id <- get_id(df_status, name, internal = TRUE)

  return(id)
  }
}




#' Get All Statuses and Their IDs
#'
#' Retrieves a data frame containing all available statuses along with their
#' associated IDs. This function is typically used to look up valid status
#' options for other functions.
#'
#' @param input Optional. A character string, numeric value, or a vector of either.
#' Used to filter statuses dataset. If NULL (default), returns the full dataset.
#'
#' @return A data frame with two columns: one for status names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_status <- get_statuses()
#' head(df_status)
#'
#' get_statuses("verworfen")
#'
#' get_statuses(3)
#' }
get_statuses <- function(input = NULL) {
  df_status <- req_to_df("statuses")

  if (!is.null(input)) {
    df_status <- df_status |>
      converter(input, internal = FALSE)
  }

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
#' convert_licenses_to_id("NonCommercialAllowed-CommercialAllowed-ReferenceRequired")
#' }
convert_licenses_to_id <- function(name) {
  if(inherits(name, "S7_missing")){
    S7::class_missing
  } else {
  df_license <- get_licenses()
  id <- get_id(df_license, name, internal = TRUE)

  return(id)
  }
}



#' Get All Licenses and Their IDs
#'
#' Retrieves a data frame containing all available licenses along with their
#' associated IDs. This function is typically used to look up valid license
#' options for other functions.
#'
#' @param input Optional. A character string, numeric value, or a vector of either.
#' Used to filter licenses dataset. If NULL (default), returns the full dataset.
#'
#' @return A data frame with two columns: one for license names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_license <- get_licenses()
#' head(df_license)
#'
#' get_licenses(1)
#'
#' }
get_licenses <- function(input = NULL) {
  df_license <- req_to_df("licenses")

  if (!is.null(input)) {
    df_license <- df_license |>
      converter(input, internal = FALSE)
  }

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
#' convert_formats_to_id("CSV")
#' }
convert_formats_to_id <- function(name) {
  if(inherits(name, "S7_missing")){
    S7::class_missing
  } else {
    df_format <- get_formats()
    id <- get_id(df_format, name, internal = TRUE)

    return(id)
  }
}

  # df_format <- get_formats()
  # id <- get_id(df_format, name, internal = TRUE)
  #
  # return(id)

#' Get All Formats and Their IDs
#'
#' Retrieves a data frame containing all available formats along with their
#' associated IDs. This function is typically used to look up valid format
#' options for other functions.
#'
#' @param input Optional. A character string, numeric value, or a vector of either.
#' Used to filter formats dataset. If NULL (default), returns the full dataset.
#'
#' @return A data frame with two columns: one for format names and one for
#' their corresponding IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' df_format <- get_formats()
#' head(df_format)
#'
#' get_formats("xlsx")
#'
#' get_formats(3)
#' }
get_formats <- function(input = NULL) {
  df_format <- req_to_df("formats")

  if (!is.null(input)) {
    df_format <- df_format |>
      converter(input, internal = FALSE)
  }

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
#' @param internal TRUE if an exact match is required for internal processes.
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

  label_col <- rlang::sym(names(df)[names(df) != "id"])
  name <- tolower(name)

  ids <- c()

  error_noun <- label_switch(label_col)[["error_noun"]]
  fun_name <- label_switch(label_col)[["fun_name"]]

  for (i in name) {

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
      names(ids) <- NULL

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
      cli::cli_abort(c(
        "!" = "{.val {name}} is not a valid {.emph {error_noun}}.",
        ">" = "To explore all {.emph {error_noun}}, run `{fun_name}`."
      ))

    } else {
      not_exact_match <- df_filtered |>
        dplyr::pull(!!label_col)

      cli::cli_abort(c(
        "!" = "For {.val {name}}, there is not an exact match.",
        "i" = "The available option(s) {.emph are}: {.val {not_exact_match}}.",
        ">" = "To explore all {.emph {error_noun}}, run `{fun_name}`."
      ))

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
    cli::cli_abort(c(
      "!" = "{.val {name}} is not a valid {.emph {error_noun}}.",
      ">" = "To explore all {.emph {error_noun}}, run `{fun_name}`."
    ))
  } else {
    multiple_ids <- df_filtered %>%
      dplyr::pull(id)

    names(multiple_ids) <- df_filtered %>%
      dplyr::pull(!!label_col)
  }

  return(multiple_ids)

}

get_label <- function(df, id, error_noun, fun_name) {

  label_col <- rlang::sym(names(df)[names(df) != "id"])

  labels <- c()

  error_noun <- label_switch(label_col)[["error_noun"]]
  fun_name <- label_switch(label_col)[["fun_name"]]

  for (i in id) {

  single_label <- df |>
    dplyr::filter(id == i) |>
    dplyr::pull(!!label_col)

  if (length(single_label) == 0) {
    cli::cli_abort(c(
      "No entry found for id: {.val {i}}",
      ">" = "To explore all {.emph {error_noun}}, run `{fun_name}`."
      ))

  }

  labels <- c(labels, single_label)

  }

  return(labels)
}



converter <- function(df, input, internal) {

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

label_switch <- function(label_col) {

  switch(as.character(label_col),
       "keywords" = {
         error_noun <- "keywords"
         fun_name <- "get_keywords()"
       },
       "zhweb-datenkataloge" = {
         error_noun <- "zh_web_catalog"
         fun_name <- "get_zh_web_catalog()"
       },
       "themes" = {
         error_noun <- "themes"
         fun_name <- "get_themes()"
       },
       "periodicities" = {
         error_noun <- "periodicities"
         fun_name <- "get_periodicities()"
       },
       "statuses" = {
         error_noun <- "statuses"
         fun_name <- "get_statuses()"
       },
       "licenses" = {
         error_noun <- "licenses"
         fun_name <- "get_licenses()"
       },
       ,
       "formats" = {
         error_noun <- "formats"
         fun_name <- "get_formats()"
       })

  return(c(error_noun = error_noun,
           fun_name = fun_name))

}

