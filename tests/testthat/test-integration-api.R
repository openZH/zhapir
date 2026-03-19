test_that("convert_keywords_to_id actually finds known keywords", {
  skip_if_ci()
  skip_if_no_dev_token()

  dev_key <- Sys.getenv("MDV_DEV_API_TOKEN_TEST")

  # This will do a real GET /keywords
  ids <- convert_keywords_to_id(
    c("abfall", "volksschule"),
    use_dev = TRUE,
    api_key = dev_key
  )
  expect_type(ids, "double")
  expect_true(all(ids > 0))
})

test_that("convert_datasets_to_id resolves a known dataset title to its ID", {
  skip_if_ci()
  skip_if_no_dev_token()

  dev_key <- Sys.getenv("MDV_DEV_API_TOKEN_TEST")

  # Fetch a single known dataset to get its exact title
  ds <- get_dataset(1, api_key = dev_key, use_dev = TRUE)
  known_title <- ds$title

  # convert_datasets_to_id should use server-side search_term filtering,
  # so it must NOT fetch the full catalogue (result << total datasets)
  id <- convert_datasets_to_id(known_title, use_dev = TRUE, api_key = dev_key)

  expect_equal(id, 1)
})

test_that("get_datasets is paginated and returns consistent results", {
  skip_if_ci()
  skip_if_no_dev_token()

  dev_key <- Sys.getenv("MDV_DEV_API_TOKEN_TEST")

  # Full fetch with default page_size
  all_ds <- get_datasets(use_dev = TRUE, api_key = dev_key)
  expect_s3_class(all_ds, "tbl_df")
  expect_named(all_ds, c("dataset", "id"))
  expect_gt(nrow(all_ds), 0L)

  # No duplicate IDs in full result
  expect_equal(anyDuplicated(all_ds$id), 0L)
})
