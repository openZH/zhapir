test_that("update_dataset(preview=TRUE) preserves existing organisation_id and title via get_dataset()", {
  calls <- list()

  testthat::local_mocked_bindings(
    get_api_key = function(...) "DUMMY",
    get_dataset = function(id, api_key, use_dev) {
      calls$fetch <<- TRUE
      list(
        id = id,
        title = "Existing Title",
        organisation = list(id = 55L)
      )
    },
    # Avoid accidental conversions that could hit the network
    convert_keywords_to_id       = function(x) x,
    convert_zh_web_catalog_to_id = function(x) x,
    convert_themes_to_id         = function(x) x,
    convert_periodicities_to_id  = function(x) x,
    .env = asNamespace("zhapir")
  )

  ds <- update_dataset(
    id               = 999L,
    # intentionally omit title and organisation_id to trigger fetch
    description      = "New description",
    theme_ids        = c(41L, 42L),
    preview          = TRUE
  )

  expect_true(isTRUE(calls$fetch))                  # fetch happened
  expect_true(inherits(ds, "zhapir::Dataset"))      # S7 class check
  expect_equal(ds@id,               999L)
  expect_equal(ds@title,            "Existing Title")
  expect_equal(ds@organisation_id,  55L)
  expect_equal(ds@description,      "New description")
  expect_equal(ds@theme_ids,        list(41L, 42L))
})

test_that("update_dataset(preview=TRUE) uses provided title/org if supplied (no fetch override)", {
  calls <- list(fetch = FALSE)

  testthat::local_mocked_bindings(
    get_api_key = function(...) "DUMMY",
    get_dataset = function(...) { calls$fetch <<- TRUE; stop("should not be called") },
    .env = asNamespace("zhapir")
  )

  ds <- update_dataset(
    id               = 1001L,
    title            = "New Title",
    organisation_id  = 77L,
    description      = "Keep as provided",
    preview          = TRUE
  )

  expect_false(isTRUE(calls$fetch))                 # shouldn't have fetched
  expect_true(inherits(ds, "zhapir::Dataset"))
  expect_equal(ds@id,              1001L)
  expect_equal(ds@title,           "New Title")
  expect_equal(ds@organisation_id, 77L)
  expect_equal(ds@description,     "Keep as provided")
})
