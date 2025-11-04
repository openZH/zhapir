testthat::local_mocked_bindings(
  get_api_key = function(key = NULL) {
    tok <- base::Sys.getenv("MDV_DEV_API_TOKEN_TEST")
    if (!base::nzchar(tok)) base::stop("MDV_DEV_API_TOKEN_TEST not set")
    tok
  },
  .package = "zhapir"
)

test_that("convert_keywords_to_id actually finds known keywords", {
  skip_if_not_e2e()
  # This will do a real GET /keywords
  ids <- convert_keywords_to_id(c("abfall", "volksschule"))
  expect_type(ids, "double")
  expect_true(all(ids > 0))
})
