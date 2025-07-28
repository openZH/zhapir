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
    theme_ids = c("Energie", "Gesundheit"),
  )
    preview = TRUE)

  ds_test <- Dataset(
    title = "Hello Dataset 1",
    organisation_id = 14,
    description = "Such insights, much wow!",
    theme_ids = c("Energie", "Gesundheit"),

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
    preview = TRUE
  ),
  "`title` ist erforderlich"
  )
})

