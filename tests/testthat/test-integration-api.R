test_that("convert_keywords_to_id actually finds known keywords", {

  skip_if_ci()
  skip_if_no_dev_token()

  dev_key <- Sys.getenv("MDV_DEV_API_TOKEN_TEST")

  # This will do a real GET /keywords
  ids <- convert_keywords_to_id(c("abfall", "volksschule"), use_dev = TRUE, api_key = dev_key)
  expect_type(ids, "double")
  expect_true(all(ids > 0))
})
