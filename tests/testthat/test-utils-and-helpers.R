test_that("get_base_url returns correct endpoints", {
  expect_equal(get_base_url(TRUE),  "https://mdv-dev.nebula.statzh.ch")
  expect_equal(get_base_url(FALSE), "https://mdv.nebula.statzh.ch")
})

test_that("get_api_key uses explicit argument over env var", {
  withr::local_envvar(c(ZHAPIR_API_KEY = "FROM_ENV"))
  expect_equal(get_api_key("FROM_ARG"), "FROM_ARG")
})

test_that("get_api_key falls back to env var", {
  withr::local_envvar(c(ZHAPIR_API_KEY = "FROM_ENV"))
  expect_equal(get_api_key(NULL), "FROM_ENV")
})

test_that("get_api_key errors when neither arg nor env nor prompt are available", {
  skip_if(interactive(), "This test is only reliable in non-interactive runs (CI).")

  withr::local_envvar(c(ZHAPIR_API_KEY = ""))

  expect_error(
    get_api_key(NULL),
    "No API key found\\. Supply via argument or set ZHAPIR_API_KEY environment variable\\.",
    fixed = FALSE
  )
})

test_that("object_to_payload formats dates and drops empty fields", {
  testthat::local_mocked_bindings(
    get_api_key = function(...) "DUMMY"
  )

  # Build a Dataset with a mix of values
  ds <- Dataset(
    title           = "Dates & Defaults",
    organisation_id = 1L,
    start_date      = as.Date("2024-01-02"),
    end_date        = as.Date(NA),      # should be dropped
    keyword_ids     = c(2, 4),
    relation_ids    = c(),              # empty list → dropped
    contact_email   = NA_character_,    # single NA → dropped
    periodicity_id  = NA_real_          # single NA → dropped
  )

  pl <- object_to_payload(ds)

  # Kept fields
  expect_equal(pl$title, "Dates & Defaults")
  expect_equal(pl$organisation_id, 1)
  expect_equal(pl$start_date, "2024-01-02")
  expect_equal(pl$keyword_ids, list(2, 4))  # stays as JSON array

  # Dropped fields
  expect_false("end_date" %in% names(pl))
  expect_false("relation_ids" %in% names(pl))
  expect_false("contact_email" %in% names(pl))
  expect_false("periodicity_id" %in% names(pl))
})

test_that("to_date returns Date or S7::class_missing accordingly", {
  # Non-missing → Date
  expect_equal(to_date("2025-01-01"), as.Date("2025-01-01"))
  # Missing sentinel → class_missing
  expect_identical(to_date(S7::class_missing), S7::class_missing)
})

test_that("to_list wraps vectors and preserves S7 missing", {
  expect_equal(to_list(c(1, 2)), list(1, 2))
  expect_identical(to_list(S7::class_missing), S7::class_missing)
})
