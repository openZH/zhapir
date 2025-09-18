testthat::test_that("prop_date: type and defaults", {
  p <- prop_date()
  testthat::expect_s3_class(p, "S7_property")
  testthat::expect_identical(p$class, S7::class_Date)

  # default value is NA Date
  testthat::expect_true(inherits(p$default, "Date"))
  testthat::expect_true(is.na(p$default))

  # custom default
  d <- as.Date("2024-01-02")
  p2 <- prop_date(default = d)
  testthat::expect_identical(p2$default, d)

  # validator is called (must return NULL on success)
  calls <- 0L
  validate_date_noop <- function(value) { calls <<- calls + 1L; NULL }
  p3 <- prop_date(validator = validate_date_noop)

  K <- S7::new_class("KDate", properties = list(x = p3))
  k <- K()

  # validator likely called once here for the default value
  before <- calls

  k@x <- as.Date("2024-02-29")
  testthat::expect_identical(calls, before + 1L)
})

testthat::test_that("prop_string: type, defaults, validator with ...", {
  p <- prop_string()
  testthat::expect_s3_class(p, "S7_property")
  testthat::expect_identical(p$class, S7::class_character)
  testthat::expect_identical(p$default, NA_character_)

  p2 <- prop_string(default = "abc")
  testthat::expect_identical(p2$default, "abc")

  # validator with ...: return NULL if ok, or a character message if invalid
  min_len_validator <- function(value, min_nchar) {
    if (!is.na(value) && nchar(value) < min_nchar) {
      return("too short")
    }
    NULL
  }
  p3 <- prop_string(validator = min_len_validator, min_nchar = 3)

  K <- S7::new_class("KStr", properties = list(x = p3))
  k <- K()

  k@x <- "hey"  # ok
  testthat::expect_identical(k@x, "hey")

  testthat::expect_error({ k@x <- "hi" }, "too short")
})

testthat::test_that("prop_numeric: type, defaults, validator with ...", {
  p <- prop_numeric()
  testthat::expect_s3_class(p, "S7_property")
  testthat::expect_identical(p$class, S7::class_numeric)
  testthat::expect_true(is.na(p$default))  # NA_real_

  p2 <- prop_numeric(default = 1.5)
  testthat::expect_identical(p2$default, 1.5)

  range_validator <- function(value, min = -Inf, max = Inf) {
    if (!is.na(value) && (value < min || value > max)) {
      return("out of range")
    }
    NULL
  }
  p3 <- prop_numeric(validator = range_validator, min = 0, max = 10)

  K <- S7::new_class("KNum", properties = list(x = p3))
  k <- K()

  k@x <- 3
  testthat::expect_identical(k@x, 3)

  testthat::expect_error({ k@x <- -1 }, "out of range")
  testthat::expect_error({ k@x <- 11 }, "out of range")
})

testthat::test_that("prop_list: type, defaults, validator", {
  p <- prop_list()
  testthat::expect_s3_class(p, "S7_property")
  testthat::expect_identical(p$class, S7::class_list)
  testthat::expect_identical(p$default, list())

  p2 <- prop_list(default = list(a = 1))
  testthat::expect_identical(p2$default, list(a = 1))

  list_len_validator <- function(value) {
    if (length(value) < 2) return("list too short")
    NULL
  }

  # ✅ Make the default pass the validator
  p3 <- prop_list(default = list(1, 2), validator = list_len_validator)

  K <- S7::new_class("KList", properties = list(x = p3))
  k <- K()

  # Now we can test failing assignment and passing assignment
  testthat::expect_error({ k@x <- list(1) }, "list too short")
  k@x <- list(1, 2)
  testthat::expect_identical(k@x, list(1, 2))
})

testthat::test_that("prop_logical: type and defaults", {
  p <- prop_logical()
  testthat::expect_s3_class(p, "S7_property")
  testthat::expect_identical(p$class, S7::class_logical)
  testthat::expect_true(is.na(p$default))

  p2 <- prop_logical(default = TRUE)
  testthat::expect_identical(p2$default, TRUE)
})

