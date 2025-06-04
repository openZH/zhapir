test_that("the distribution object is correctly created", {
  expect_no_error(
    Distribution(
      title = "Hello Distribution 1",
      dataset_id = 6819,
      stat_server_flag = FALSE,
      zh_web_flag = FALSE,
      ogd_flag = TRUE,
      sort_order = 1,
      description = "WOW this is a distribution!",
      modified = "2025-03-31",
      access_url = "https://test.ch",
      identifier = "test",
      right = "test",
      issued = "2025-03-31",
      byte_size = 12345,
      status_id = 1,
      license_id = 1,
      format_id = 1,
      media_type_id = 1,
      periodicity_id = 1,
      file_upload_id = "2"
    )
  )
})
