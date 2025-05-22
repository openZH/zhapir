
#' Get the ID's of all Organisations and their Units
#'
#' @returns
#' @export
#'
#' @examples
get_all_organisation_info <- function() {
  req <- api_request(
    method = "GET",
    endpoint = "/api/v1/organisations",
    api_key = get_api_key()
  )

  # create data frame with all organisation information
  df_organisation_info <- purrr::map_df(req, ~ {
    organisation <- .x

    # retrieve organisation information
    organisation_id <- organisation$id
    organisation <- organisation$name

    # FIXME: potentially use below chunk to retrieve organisation units
    # # retrieve organisation unit information within an additional list of a
    # # given organisation
    # if (length(organisation$organisation_units) != 0) {
    #   df_organisation_unit <- purrr::map_df(organisation$organisation_units, ~ {
    #     orga_unit_id <- .x$id
    #     orga_unit <- .x$label
    #
    #     data.frame(orga_unit_id = orga_unit_id, orga_unit = orga_unit)
    #   })
    # }

    # Combine organisation and organisation unit information to a data frame
    if (exists("df_organisation_unit")) {
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




#' Title
#'
#' @param name
#'
#' @returns
#' @export
#'
#' @examples
get_keywords_id <- function(name) {
  df_keywords <- get_keywords()

  id <- get_id(name, df_keywords)
}


#' Get all Keywords and their ID's
#'
#' @returns
#' @export
#'
#' @examples
get_keywords <- function() {

  df_keywords <- req_to_df("keywords")
  return(df_keywords)
}



#' Title
#'
#' @param name
#'
#' @returns
#' @export
#'
#' @examples
get_zh_web_catalog_id <- function(name) {
  df_zh_web_catalog <- get_zh_web_catalog()

  id <- get_id(name, df_zh_web_catalog)
}


#' Title
#'
#' @returns
#' @export
#'
#' @examples
get_zh_web_catalog <- function() {

  df_zh_web_catalog <- req_to_df("zhweb-datenkataloge")
  return(df_zh_web_catalog)
}




#' Title
#'
#' @param name
#'
#' @returns
#' @export
#'
#' @examples
get_theme_id <- function(name) {

  df_theme <- get_themes()

  id <- get_id(name, df_theme)

}




#' Title
#'
#' @returns
#' @export
#'
#' @examples
get_themes <- function() {

  df_themes <- req_to_df("themes")
  return(df_themes)
}




#' Title
#'
#' @param name
#'
#' @returns
#' @export
#'
#' @examples
get_periodicity_id <- function(name) {

  df_periodicity <- get_periocidity()

  id <- get_id(name, df_periodicity)

}




#' Title
#'
#' @returns
#' @export
#'
#' @examples
get_periodicity <- function() {

  df_periocidity <- req_to_df("periodicities")
  return(df_periocidity)
}





req_to_df <- function(variable) {

  req <- api_request(
    method = "GET",
    endpoint = paste0("/api/v1/", variable),
    api_key = get_api_key()
  )

  df <- purrr::map_df(req, ~ {
    # retrieve keyword information
    id <- .x$id
    label <- .x$label
    setNames(data.frame(label, id, stringsAsFactors = FALSE), c(variable, "id"))
  })

  return(df)
  }







#' Get the ID of a certain Variable
#'
#' @param name
#' @param df
#'
#' @returns
#' @export
#'
#' @examples
get_id <- function(name, df) {
  filter_col <- rlang::sym(names(df)[names(df) != "id"])

  ids <- c()

  for (i in name) {

    switch(as.character(filter_col),
      "keyword" = {
        error_noun <- "keywords"
        fun_name_suffix <- "keywords"
      },
      "zh_web_catalog_keyword" = {
        error_noun <- "zh_web_catalog"
        fun_name_suffix <- "get_zh_web_catalog"
      },
      "themes" = {
        error_noun <- "themes"
        fun_name_suffix <- "get_themes"
      },
      "periodicity" = {
        error_noun <- "periodicity"
        fun_name_suffix <- "get_periodicity"
      }
    )

    df_filtered <- df |>
      dplyr::filter(grepl(i, !!filter_col))

    exact_match <- df_filtered |>
      dplyr::filter(!!filter_col == i)

    if (nrow(exact_match == 1)) {
      single_id <- exact_match |>
        dplyr::pull(id)

      names(single_id) <- exact_match |>
        dplyr::pull(!!filter_col)

      ids <- c(ids, single_id)
    } else if (nrow(df_filtered) == 0) {
      stop(paste0(
        "'", i, "' is not a valid ", filter_col, ".",
        " To explore all ", error_noun, " run '", paste0("get_", fun_name_suffix), "()'."
      ))
    } else if (nrow(df_filtered) > 1) {
      multiple_matches <- df_filtered |>
        dplyr::pull(!!filter_col)

      stop(
        paste0(
          "For '", i, "' there are multiple entries:\n",
          paste0(multiple_matches, collapse = ", "),
          paste0("\nTo explore all ", error_noun, " run '", paste0("get_", fun_name_suffix), "()'.")
        )
      )
    } else {
      single_id <- df_filtered |>
        dplyr::pull(id)

      names(single_id) <- df_filtered |>
        dplyr::pull(!!filter_col)

      ids <- c(ids, single_id)
    }
  }
  print(ids)
  return(ids)
}
