# These tests mock api_request() so nothing goes to the network.

testthat::test_that("req_to_df builds correct tibbles and errors on unknown endpoint", {

  # Mock data per endpoint
  fake_db <- list(
    "keywords" = list(
      list(label = "Abwasser", id = 1),
      list(label = "Abfall",   id = 2),
      list(label = "Wasser",   id = 3)
    ),
    "themes" = list(
      list(label = "Verkehr", id = 10),
      list(label = "Energie", id = 11)
    ),
    "periodicities" = list(
      list(label = "Jährlich", id = 20),
      list(label = "Monatlich", id = 21)
    ),
    "statuses" = list(
      list(label = "Entwurf", id = 30),
      list(label = "Publiziert", id = 31)
    ),
    "licenses" = list(
      list(label = "CC BY", id = 40)
    ),
    "file-formats" = list(
      list(label = "CSV", id = 50),
      list(label = "JSON", id = 51)
    ),
    "zh-web-datacatalogs" = list(
      list(label = "Bevölkerung", id = 60),
      list(label = "Wirtschaft", id = 61)
    )
  )

  mock_api_request <- function(method, endpoint, api_key, object_label) {
    # endpoints look like "/api/v1/<endpoint>"
    ep <- sub("^/api/v1/", "", endpoint)
    # special for datasets and organisations (tested later), otherwise return list
    if (!is.null(fake_db[[ep]])) {
      return(fake_db[[ep]])
    }
    stop("Unknown endpoint in mock: ", ep)
  }

  testthat::local_mocked_bindings(
    .package = "zhapir",
    api_request = mock_api_request,
    get_api_key = function() "fake"
  )

  # keywords
  df_kw <- req_to_df("keywords")
  testthat::expect_true(all(names(df_kw) == c("keywords", "id")))
  testthat::expect_identical(nrow(df_kw), 3L)

  # themes
  df_th <- req_to_df("themes")
  testthat::expect_true(all(names(df_th) == c("themes", "id")))
  testthat::expect_identical(nrow(df_th), 2L)

  # file-formats
  df_ff <- req_to_df("file-formats")
  testthat::expect_true(all(names(df_ff) == c("file-formats", "id")))
  testthat::expect_identical(nrow(df_ff), 2L)

  # unknown endpoint -> error
  testthat::expect_error(req_to_df("nope"), "Unknown endpoint")
})


testthat::test_that("label_switch maps known labels and errors on unknown", {
  x1 <- label_switch(rlang::sym("keywords"))
  testthat::expect_identical(unname(x1["error_noun"]), "keywords")
  testthat::expect_identical(unname(x1["fun_name"]), "get_keywords()")

  x2 <- label_switch(rlang::sym("themes"))
  testthat::expect_identical(unname(x2["error_noun"]), "themes")
  testthat::expect_identical(unname(x2["fun_name"]), "get_themes()")

  testthat::expect_error(label_switch(rlang::sym("nope")), "Unknown label column")
})


testthat::test_that("get_id works for internal and non-internal modes incl. errors", {

  df <- tibble::tibble(
    keywords = c("Abwasser", "Abfall", "Wasser"),
    id = c(1, 2, 3)
  )

  # internal = TRUE -> requires exact match; partial matches -> 'no exact match'
  testthat::expect_identical(
    get_id(df, "Abwasser", internal = TRUE),
    1
  )

  # case-insensitive exact
  testthat::expect_identical(
    get_id(df, "abfall", internal = TRUE),
    2
  )

  # no match
  testthat::expect_error(
    get_id(df, "foobar", internal = TRUE),
    "not valid"
  )

  # partial match exists but not exact -> error "no exact match"
  testthat::expect_error(
    get_id(df, "wasser", internal = TRUE), # matches "Wasser" exactly; to force partial, use "was"
    NA
  )
  # Force partial only scenario
  testthat::expect_error(
    get_id(df, "was", internal = TRUE),
    "no exact match"
  )

  # internal = FALSE -> partial allowed, returns all matching ids (duplicates possible)
  testthat::expect_identical(
    sort(get_id(df, c("was", "ab"), internal = FALSE)),
    sort(c(1, 1, 2, 3))
  )

  # internal = FALSE -> no match -> error "not valid"
  testthat::expect_error(
    get_id(df, "zzz", internal = FALSE),
    "not valid"
  )
})


testthat::test_that("get_label returns labels and errors when id missing", {
  df <- tibble::tibble(
    themes = c("Verkehr", "Energie"),
    id = c(10, 11)
  )
  testthat::expect_identical(
    get_label(df, c(10, 11)),
    c("Verkehr", "Energie")
  )
  testthat::expect_error(
    get_label(df, 999),
    "no entry"
  )
})

testthat::test_that("converter dispatches on type (character vs numeric)", {
  df <- tibble::tibble(
    keywords = c("Abwasser", "Abfall", "Wasser"),
    id = c(1, 2, 3)
  )

  # character input -> use regex to avoid overlapping matches
  out1 <- converter(df, input = c("^abfall$", "^wasser$"), internal = FALSE)
  testthat::expect_setequal(out1$id, c(2, 3))

  # numeric input -> uses get_label then filters by label column
  out2 <- converter(df, input = c(1, 3), internal = FALSE)
  testthat::expect_identical(out2$keywords, c("Abwasser", "Wasser"))
})



testthat::test_that("get_organisations returns base and units correctly", {
  # Build a mock return for /organisations
  org_payload <- list(
    list(
      id = 100,
      name = "Org A",
      organisation_units = list(
        list(id = 1001, label = "Unit A1"),
        list(id = 1002, label = "Unit A2")
      )
    ),
    list(
      id = 200,
      name = "Org B",
      organisation_units = list() # no units
    )
  )

  mock_api_request <- function(method, endpoint, api_key, object_label) {
    ep <- sub("^/api/v1/", "", endpoint)
    if (ep == "organisations") return(org_payload)
    stop("Unexpected endpoint in mock: ", ep)
  }

  testthat::local_mocked_bindings(
    .package = "zhapir",
    api_request = mock_api_request,
    get_api_key = function() "fake"
  )

  # with units (default TRUE)
  df1 <- get_organisations()
  # Org A expands to two rows (due to units), Org B remains base only
  testthat::expect_true(all(c("organisation_id", "organisation") %in% names(df1)))
  testthat::expect_true(all(c("organisation_unit_id", "organisation_unit") %in% names(df1)))
  testthat::expect_identical(nrow(df1), 3L)

  # without units -> only base columns and 2 rows
  df2 <- get_organisations(FALSE)
  testthat::expect_identical(names(df2), c("organisation_id", "organisation"))
  testthat::expect_identical(nrow(df2), 2L)
})


testthat::test_that("req_to_df-backed getters and converters behave (with dataset label fix)", {
  # Build a single mock that handles all endpoints used here
  fake_db <- list(
    "keywords" = list(
      list(label = "Abwasser", id = 1),
      list(label = "Abfall",   id = 2),
      list(label = "Wasser",   id = 3)
    ),
    "themes" = list(
      list(label = "Verkehr", id = 10),
      list(label = "Energie", id = 11)
    ),
    "periodicities" = list(
      list(label = "Jährlich",  id = 20),
      list(label = "Monatlich", id = 21)
    ),
    "statuses" = list(
      list(label = "Entwurf",    id = 30),
      list(label = "Publiziert", id = 31)
    ),
    "licenses" = list(list(label = "CC BY", id = 40)),
    "file-formats" = list(list(label = "CSV", id = 50)),
    "zh-web-datacatalogs" = list(
      list(label = "Bevölkerung", id = 60)
    ),
    # datasets uses a different shape (has $items)
    "datasets" = list(
      items = list(
        list(title = "Hotels Zürich",     id = 900),
        list(title = "Hotels Winterthur", id = 901)
      )
    )
  )

  mock_api_request <- function(method, endpoint, api_key, object_label) {
    ep <- sub("^/api/v1/", "", endpoint)
    if (ep == "datasets") return(fake_db[["datasets"]])
    if (!is.null(fake_db[[ep]])) return(fake_db[[ep]])
    stop("Unexpected endpoint in mock: ", ep)
  }

  testthat::local_mocked_bindings(
    .package   = "zhapir",
    api_request = mock_api_request,
    get_api_key = function() "fake"
  )

  # (Lock the fix) label_switch must accept 'dataset' and map to get_datasets()
  m <- zhapir:::label_switch(rlang::sym("dataset"))
  testthat::expect_identical(unname(m["error_noun"]), "datasets")
  testthat::expect_identical(unname(m["fun_name"]),   "get_datasets()")

  # keywords (passthrough)
  all_kw <- zhapir::get_keywords()
  testthat::expect_identical(names(all_kw), c("keywords", "id"))
  testthat::expect_identical(nrow(all_kw), 3L)

  # keywords filter by name -> uses converter(..., internal = FALSE)
  kw_sel <- zhapir::get_keywords("abf")
  testthat::expect_identical(kw_sel$id, 2)

  # themes
  th_all <- zhapir::get_themes()
  testthat::expect_identical(nrow(th_all), 2L)
  th_sel <- zhapir::get_themes("energie")
  testthat::expect_identical(th_sel$id, 11)

  # periodicities
  pe_all <- zhapir::get_periodicities()
  testthat::expect_identical(nrow(pe_all), 2L)
  pe_sel <- zhapir::get_periodicities("jähr")
  testthat::expect_true(all(pe_sel$id %in% c(20, 21)))

  # statuses
  st_all <- zhapir::get_statuses()
  testthat::expect_identical(nrow(st_all), 2L)
  st_sel <- zhapir::get_statuses("entwurf")
  testthat::expect_identical(st_sel$id, 30)

  # licenses
  li_all <- zhapir:::get_licenses()
  testthat::expect_identical(nrow(li_all), 1L)

  # formats
  ff_all <- zhapir:::get_formats()
  testthat::expect_identical(nrow(ff_all), 1L)

  # zh web catalog
  zwc_all <- zhapir:::get_zh_web_catalog()
  testthat::expect_identical(nrow(zwc_all), 1L)

  # datasets (special shape)
  ds_all <- zhapir:::get_datasets()
  testthat::expect_identical(names(ds_all), c("dataset", "id"))
  testthat::expect_identical(nrow(ds_all), 2L)

  # datasets filter by title (uses converter -> label_switch('dataset') path)
  ds_sel <- zhapir:::get_datasets("winterthur")
  testthat::expect_identical(ds_sel$id, 901)
})
