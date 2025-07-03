test_that("dataset object is correctly created by the create_dataset function", {

  testthat::local_mocked_bindings(
    get_api_key = function(...) NA
  )


  testthat::local_mocked_bindings(
    convert_themes_to_id = function(x) {
      if (identical(x, c("Energie", "Gesundheit"))) {
        return(c(42, 43))
      } else {
        stop("Unexpected input")
      }
    }
  )



  ds <- create_dataset(
    "Hello Dataset 1",
    organisation_id = 14,
    description = "Such insights, much wow!",
    issued = "2025-03-31",
    theme_ids = c("Energie", "Gesundheit"),
    test = TRUE
  )

  ds_test <- Dataset(
    title = "Hello Dataset 1",
    organisation_id = 14,
    description = "Such insights, much wow!",
    theme_ids = c("Energie", "Gesundheit"),
    issued = "2025-03-31",
  )

  expect_equal(ds, ds_test)



})



test_that("an error is returned if no title is set", {
  testthat::local_mocked_bindings(
    get_api_key = function(...) NA
  )

  testthat::local_mocked_bindings(
    convert_themes_to_id = function(x) {
      if (identical(x, c("Energie", "Gesundheit"))) {
        return(c(42, 43))
      } else {
        stop("Unexpected input")
      }
    }
  )


  expect_error(ds <- create_dataset(
    NA,
    organisation_id = 14,
    description = "Such insights, much wow!",
    theme_ids = c("Energie", "Gesundheit"),
    issued = "2025-03-31",
    test = TRUE
  ),
  "`title` ist erforderlich"
  )
})

