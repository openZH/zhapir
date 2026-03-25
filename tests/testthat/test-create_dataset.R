test_that("dataset object is correctly created by the create_dataset function", {

  testthat::local_mocked_bindings(
    get_api_key = function(...) NA
  )


  testthat::local_mocked_bindings(
    convert_themes_to_id = function(x, ...) {
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
    preview = TRUE
  )

  # since convert_themes_to_id() is mocked to return c(42,43), build ds_test with numeric IDs:
  ds_test <- Dataset(
    title           = "Hello Dataset 1",
    organisation_id = 14,
    description     = "Such insights, much wow!",
    theme_ids       = c(42, 43)
  )

  expect_equal(ds, ds_test)



})



test_that("an error is returned if no title is set", {
  testthat::local_mocked_bindings(
    get_api_key = function(...) NA
  )

  testthat::local_mocked_bindings(
    convert_themes_to_id = function(x, ...) {
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

testthat::test_that("create_dataset() resolves one and two see_also_ids in preview mode", {
  seen_endpoints <- character()

  mock_api_request <- function(method, endpoint, ...) {
    testthat::expect_length(endpoint, 1L)
    seen_endpoints <<- c(seen_endpoints, endpoint)

    if (!grepl("^/api/v1/datasets\\?", endpoint)) {
      stop("Unexpected endpoint: ", endpoint)
    }

    if (grepl("searchTerm=Dataset%20A", endpoint)) {
      return(list(
        total = 1L,
        items = list(
          list(title = "Dataset A", id = 101L)
        )
      ))
    }

    if (grepl("searchTerm=Dataset%20B", endpoint)) {
      return(list(
        total = 1L,
        items = list(
          list(title = "Dataset B", id = 202L)
        )
      ))
    }

    return(list(
      total = 0L,
      items = list()
    ))
  }

  testthat::local_mocked_bindings(
    .package = "zhapir",
    api_request = mock_api_request,
    get_api_key = function(api_key = NULL) "fake"
  )

  ds_one <- zhapir::create_dataset(
    title = "My dataset",
    organisation_id = 1,
    see_also_ids = "Dataset A",
    preview = TRUE
  )

  testthat::expect_identical(unlist(ds_one@see_also_ids), 101)

  ds_two <- zhapir::create_dataset(
    title = "My dataset",
    organisation_id = 1,
    see_also_ids = c("Dataset A", "Dataset B"),
    preview = TRUE
  )

  testthat::expect_identical(unlist(ds_two@see_also_ids), c(101, 202))

  testthat::expect_true(any(grepl("searchTerm=Dataset%20A", seen_endpoints)))
  testthat::expect_true(any(grepl("searchTerm=Dataset%20B", seen_endpoints)))
})
