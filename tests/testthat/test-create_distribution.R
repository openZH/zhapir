test_that("create_distribution(preview=TRUE) returns a Distribution S7 object with correct slots", {
  testthat::local_mocked_bindings(
    get_api_key = function(...) "DUMMY"
  )

  dist <- create_distribution(
    title            = "Preview Dist",
    dataset_id       = 42,
    stat_server_flag = TRUE,
    zh_web_flag      = FALSE,
    ogd_flag         = TRUE,
    description      = "just a preview",
    access_url       = "https://example.org/data.csv",
    byte_size        = 2048,
    status_id        = 2L,    # note: NOT part of the Distribution object; applied later in real calls
    license_id       = 3L,    # numeric → no conversion call
    file_format_id   = 4L,    # numeric → no conversion call
    periodicity_id   = 12L,   # numeric required by S7; avoids lookup + passes validation
    file_path        = "/tmp/should/not/upload.csv",
    start_date       = "2025-08-01",
    end_date         = "2025-08-31",
    preview          = TRUE
  )

  expect_true(inherits(dist, "zhapir::Distribution"))
  expect_equal(dist@title,            "Preview Dist")
  expect_equal(dist@dataset_id,       42L)
  expect_true(dist@stat_server_flag)
  expect_false(dist@zh_web_flag)
  expect_true(dist@ogd_flag)
  expect_equal(dist@description,      "just a preview")
  expect_equal(dist@access_url,       "https://example.org/data.csv")
  expect_equal(dist@byte_size,        2048)
  # status_id is NOT a slot of Distribution → no assertion here
  expect_equal(dist@license_id,       3L)
  expect_equal(dist@file_format_id,   4L)
  expect_equal(dist@periodicity_id,   12L)
  expect_true(is.na(dist@file_upload_id))
})
