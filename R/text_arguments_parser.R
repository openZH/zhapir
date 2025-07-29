#' Get All Organisations and Their IDs
#'
#' Retrieves a tibble of all organisations and their IDs. Optionally includes sub-units.
#'
#' @param show_organisation_units Logical; if TRUE, include sub-units.
#' @return A tibble with columns:
#'   - `organisation_id` (numeric)
#'   - `organisation` (character)
#'   - optionally `organisation_unit_id` (numeric) and `organisation_unit` (character)
#' @examples
#' \dontrun{
#'   # All organisations
#'   get_organisations()
#'
#'   # Without sub-units
#'   get_organisations(FALSE)
#' }
#' @export
get_organisations <- function(show_organisation_units = TRUE) {
  req <- api_request(
    method       = "GET",
    endpoint     = "/api/v1/organisations",
    api_key      = get_api_key(),
    object_label = "Organisation"
  )

  purrr::map_df(req, function(x) {
    base <- tibble::tibble(
      organisation_id = x$id,
      organisation    = x$name
    )
    if (show_organisation_units && length(x$organisation_units) > 0) {
      units <- purrr::map_df(
        x$organisation_units,
        ~ tibble::tibble(
          organisation_unit_id = .x$id,
          organisation_unit    = .x$label
        )
      )
      dplyr::bind_cols(base, units)
    } else {
      base
    }
  })
}

#' Get All Keywords and Their IDs
#'
#' Retrieves a tibble of all keywords and their IDs. Optionally filters by name or ID.
#'
#' @param input Optional character vector of keyword names or numeric IDs.
#' @return A tibble with two columns:
#'   - `keyword` (character): the keyword label
#'   - `id` (numeric): the keyword ID
#' @examples
#' \dontrun{
#'   # All keywords
#'   get_keywords()
#'
#'   # By name
#'   get_keywords("abwasser")
#'
#'   # By ID
#'   get_keywords(578)
#' }
#' @export
get_keywords <- function(input = NULL) {
  df <- req_to_df("keywords")
  if (!is.null(input)) df <- converter(df, input, internal = FALSE)
  df
}

#' Convert keyword names to IDs
#' @param name Character vector of keyword names.
#' @return Numeric vector of IDs.
#' @keywords internal
convert_keywords_to_id <- function(name) {
  if (inherits(name, "S7_missing")) return(S7::class_missing)

  df <- get_keywords()
  get_id(df, name, internal = TRUE)
}


#' Get All Datasets and Their IDs
#'
#' Retrieves a tibble of datasets (title and id), with optional filtering.
#'
#' @param input Optional character vector of dataset titles or numeric IDs.
#' @return A tibble with columns:
#'   - `dataset` (character): dataset title
#'   - `id` (numeric): dataset ID
#' @examples
#' \dontrun{
#'   # All datasets
#'   get_datasets()
#'
#'   # Filter by title
#'   get_datasets("Hotels")
#'
#'   # Filter by ID
#'   get_datasets(10)
#' }
#' @export
get_datasets <- function(input = NULL) {
  req <- api_request(
    method       = "GET",
    endpoint     = "/api/v1/datasets",
    api_key      = get_api_key(),
    object_label = "Dataset"
  )
  df <- purrr::map_df(req$items, function(x) {
    tibble::tibble(
      dataset = x$title,
      id      = x$id
    )
  })
  if (!is.null(input)) {
    df <- converter(df, input, internal = FALSE)
  }
  df
}

#' Get All zh-web-catalog Keywords and Their IDs
#'
#' Retrieves a tibble of all zh-web-catalog entries and their IDs. Optionally filters by label or ID.
#'
#' @param input Optional character vector of catalog labels or numeric IDs.
#' @return A tibble with two columns:
#'   - `zh_web_catalog` (character): the catalog label
#'   - `id` (numeric): the catalog ID
#' @examples
#' \dontrun{
#'   # All catalog entries
#'   get_zh_web_catalog()
#'
#'   # By label
#'   get_zh_web_catalog("Bevölkerung")
#'
#'   # By ID
#'   get_zh_web_catalog(13)
#' }
#' @export
get_zh_web_catalog <- function(input = NULL) {
  df <- req_to_df("zh-web-datacatalogs")
  if (!is.null(input)) df <- converter(df, input, internal = FALSE)
  df
}

#' Convert zh-web-catalog names to IDs
#' @keywords keywords internal
convert_zh_web_catalog_to_id <- function(name) {
  if (inherits(name, "S7_missing")) return(S7::class_missing)
  df <- get_zh_web_catalog()
  get_id(df, name, internal = TRUE)
}

#' Get All Themes and Their IDs
#'
#' Retrieves a tibble of all themes and their IDs. Optionally filters by name or ID.
#'
#' @param input Optional character vector of theme names or numeric IDs.
#' @return A tibble with two columns:
#'   - `theme` (character): the theme label
#'   - `id` (numeric): the theme ID
#' @examples
#' \dontrun{
#'   # All themes
#'   get_themes()
#'
#'   # By name
#'   get_themes("Verkehr")
#'
#'   # By ID
#'   get_themes(41)
#' }
#' @export
get_themes <- function(input = NULL) {
  df <- req_to_df("themes")
  if (!is.null(input)) df <- converter(df, input, internal = FALSE)
  df
}

#' Convert theme names to IDs
#' @keywords keywords internal
convert_themes_to_id <- function(name) {
  if (inherits(name, "S7_missing")) return(S7::class_missing)
  df <- get_themes()
  get_id(df, name, internal = TRUE)
}

#' Get All Periodicities and Their IDs
#'
#' Retrieves a tibble of all periodicities and their IDs. Optionally filters by name or ID.
#'
#' @param input Optional character vector of periodicity names or numeric IDs.
#' @return A tibble with two columns:
#'   - `periodicity` (character): the periodicity label
#'   - `id` (numeric): the periodicity ID
#' @examples
#' \dontrun{
#'   # All periodicities
#'   get_periodicities()
#'
#'   # By name
#'   get_periodicities("Jährlich")
#'
#'   # By ID
#'   get_periodicities(42)
#' }
#' @export
get_periodicities <- function(input = NULL) {
  df <- req_to_df("periodicities")
  if (!is.null(input)) df <- converter(df, input, internal = FALSE)
  df
}

#' Convert periodicity names to IDs
#' @keywords internal
convert_periodicities_to_id <- function(name) {
  if (inherits(name, "S7_missing")) return(S7::class_missing)
  df <- get_periodicities()
  get_id(df, name, internal = TRUE)
}


#' Get All Statuses and Their IDs
#'
#' Retrieves a tibble of all statuses and their IDs. Optionally filters by name or ID.
#'
#' @param input Optional character vector of status names or numeric IDs.
#' @return A tibble with two columns:
#'   - `status` (character): the status label
#'   - `id` (numeric): the status ID
#' @examples
#' \dontrun{
#'   # All statuses
#'   get_statuses()
#'
#'   # By name
#'   get_statuses("Entwurf")
#'
#'   # By ID
#'   get_statuses(3)
#' }
#' @export
get_statuses <- function(input = NULL) {
  df <- req_to_df("statuses")
  if (!is.null(input)) df <- converter(df, input, internal = FALSE)
  df
}

#' Convert status names to IDs
#' @keywords internal
convert_statuses_to_id <- function(name) {
  if (inherits(name, "S7_missing")) return(S7::class_missing)
  df <- get_statuses()
  get_id(df, name, internal = TRUE)
}

#' Get All Licenses and Their IDs
#' @keywords internal
get_licenses <- function(input = NULL) {
  df <- req_to_df("licenses")
  if (!is.null(input)) df <- converter(df, input, internal = FALSE)
  df
}

#' Convert license names to IDs
#' @keywords internal
convert_licenses_to_id <- function(name) {
  if (inherits(name, "S7_missing")) return(S7::class_missing)
  df <- get_licenses()
  get_id(df, name, internal = TRUE)
}

#' Get All Formats and Their IDs
#' @keywords internal
get_formats <- function(input = NULL) {
  df <- req_to_df("file-formats")
  if (!is.null(input)) df <- converter(df, input, internal = FALSE)
  df
}

#' Convert format names to IDs
#' @keywords internal
convert_formats_to_id <- function(name) {
  if (inherits(name, "S7_missing")) return(S7::class_missing)
  df <- get_formats()
  get_id(df, name, internal = TRUE)
}

#' Retrieve a Data Frame from API Endpoint
#'
#' Generic helper to fetch <endpoint> entries with labels and ids.
#'
#' @param endpoint One of: "keywords", "themes", etc.
#' @return A tibble with columns `<endpoint>` and `id`.
#' @keywords internal
#' Retrieve a Data Frame from API Endpoint
#'
#' Generic helper to fetch <endpoint> entries with labels and ids.
#'
#' @param endpoint One of: "keywords", "themes", etc.
#' @return A tibble with columns `<endpoint>` and `id`.
#' @keywords internal
req_to_df <- function(endpoint) {

  label <- switch(
    endpoint,
    "keywords"            = "Keyword",
    "zh-web-datacatalogs" = "ZhWebCatalog",
    "themes"              = "Theme",
    "periodicities"       = "Periodicity",
    "statuses"            = "Status",
    "licenses"            = "License",
    "file-formats"        = "FileFormat",
    stop("Unknown endpoint: ", endpoint)
  )
  req <- api_request(
    method       = "GET",
    endpoint     = paste0("/api/v1/", endpoint),
    api_key      = get_api_key(),
    object_label = label
  )
  purrr::map_df(req, function(x) {
    tibble::tibble(
      !!endpoint := x$label,
      id          = x$id
    )
  })
}

#' Get the ID(s) of Entries for Given Variable
#' @keywords internal
get_id <- function(df, name, internal) {
  label_col <- rlang::sym(names(df)[names(df) != "id"])
  name_lower <- tolower(name)
  ids <- numeric(0)
  err <- label_switch(label_col)
  for (nm in name_lower) {
    tmp <- df |> dplyr::mutate(filter_col = tolower(!!label_col))
    filt <- tmp |> dplyr::filter(grepl(nm, filter_col))
    exact <- tmp |> dplyr::filter(filter_col == nm)
    if (internal) {
      if (nrow(exact)==1) ids <- c(ids, exact$id)
      else if (nrow(filt)==0) cli::cli_abort(c("!"=sprintf("%%s not valid", nm), ">"=sprintf("run %s", err["fun_name"])) )
      else cli::cli_abort(c("!"=sprintf("no exact match for %s", nm), ">"=sprintf("run %s", err["fun_name"])) )
    } else {
      if (nrow(filt)==0) cli::cli_abort(c("!"=sprintf("%%s not valid", nm), ">"=sprintf("run %s", err["fun_name"])) )
      ids <- c(ids, filt$id)
    }
  }
  ids
}

#' Convert ID(s) back to label(s)
#' @keywords internal
get_label <- function(df, id) {
  label_col <- rlang::sym(names(df)[names(df)!="id"])
  err <- label_switch(label_col)
  vapply(id, function(i) {
    res <- df |> dplyr::filter(id==i) |> dplyr::pull(!!label_col)
    if (!length(res)) cli::cli_abort(c("!"=sprintf("no entry for %%s", i), ">"=sprintf("run %s", err["fun_name"])) )
    res
  }, character(1))
}

#' Filter a tibble by name or ID input
#' @keywords internal
converter <- function(df, input, internal) {
  if (is.character(input)) dplyr::filter(df, id %in% get_id(df, input, internal))
  else dplyr::filter(df, !!rlang::sym(names(df)[1]) %in% get_label(df, input))
}

#' Map a label column to error context
#' @keywords internal
label_switch <- function(label_col) {
  switch(as.character(label_col),
         "keywords"="c(error_noun='keywords', fun_name='get_keywords()')",
         "zh-web-datacatalogs"="c(error_noun='zh-web-catalog entries', fun_name='get_zh_web_catalog()')",
         "themes"="c(error_noun='themes', fun_name='get_themes()')",
         "periodicities"="c(error_noun='periodicities', fun_name='get_periodicities()')",
         "statuses"="c(error_noun='statuses', fun_name='get_statuses()')",
         "licenses"="c(error_noun='licenses', fun_name='get_licenses()')",
         "file-formats"="c(error_noun='file formats', fun_name='get_formats()')",
         "datasets"="c(error_noun='datasets', fun_name='get_datasets()')",
         stop("Unknown label column: ", label_col)
  )
}
