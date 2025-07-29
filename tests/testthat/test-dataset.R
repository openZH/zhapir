test_that("the dataset object is correctly created", {
  expect_no_error(
    Dataset(
      title           = "Hello Dataset 1",
      organisation_id = 14,
      description     = "Such insights, much wow!",
      contact_email   = "test.test@blabla.com",
      landing_page    = "https://test.ch",
      start_date      = "2023-03-31",
      end_date        = "2025-03-31",
      modified_next   = "2026-03-31",
      keyword_ids     = c(2, 4, 11),        # numeric IDs
      zh_web_catalogs = c(10, 11),         # numeric IDs
      relation_ids    = c(1, 2, 3),
      theme_ids       = c(41, 45)          # numeric IDs
    )
  )
})

ds_test <- Dataset(
  title           = "Hello Dataset 1",
  organisation_id = 14,
  description     = "Such insights, much wow!",
  theme_ids       = c(42, 43)            # numeric IDs for the later check
)

test_that("The the values are correctly set", {
  expect_equal(ds_test@title, "Hello Dataset 1")
  expect_equal(ds_test@organisation_id, 14)
  expect_equal(ds_test@description, "Such insights, much wow!")
  expect_equal(ds_test@theme_ids, list(42, 43))
})

test_that("The defaults are correctly set", {
  expect_equal(ds_test@end_date, as.Date(NA))
  expect_equal(ds_test@relation_ids, list())
  expect_equal(ds_test@contact_email, NA_character_)
  expect_equal(ds_test@periodicity_id, NA_real_)
})

test_that("The values are correctly validated", {
  # if type is correctly checked
  expect_error(
    Dataset(
      title = 1,
      organisation_id = 14
    ),
    "title must be <character>"
  )

  # return error when email not correctly written
  expect_error(
    Dataset(
      title = "Hello Dataset 1",
      organisation_id = 14,
      contact_email = "blabla.ch"
    ),
    "@contact_email must be a valid address"
  )

  # return error when a list entry contains non numeric entries
  expect_error(
    Dataset(
      title = "Hello Dataset 1",
      organisation_id = 14,
      theme_ids = c(42, "foo", 43)
    ),
    "all elements must be numeric"
  )

  # return error when a url is not correctly written
  expect_error(
    Dataset(
      title = "Hello Dataset 1",
      organisation_id = 14,
      landing_page = "blabla.ch"
    ),
    "@landing_page must start with http:// or https://"
  )

  # return error when an id is not correctly set
  expect_error(
    Dataset(
      title = "Hello Dataset 1",
      organisation_id = c(14, 2)
    ),
    "@organisation_id must have exactly one value"
  )

  expect_error(
    Dataset(
      title = "Hello Dataset 1",
      organisation_id = -1
    ),
    "@organisation_id must be a positive number"
  )

  expect_error(
    Dataset(
      title = "Hello Dataset 1",
      organisation_id = NA_real_
    ),
    "@organisation_id cannot be NA"
  )

  expect_error(
    Dataset(
      title = "Hello Dataset 1",
      organisation_id = 1.25
    ),
    "@organisation_id must be a whole number"
  )
})
