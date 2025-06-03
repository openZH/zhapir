# test_that("the POSIXct property is correctly created", {
#   posixct_property <- prop_posixct()
#
#   expect_equal(class(posixct_property), "S7_property")
#   expect_equal(posixct_property$class$class, "POSIXct")
#   expect_equal(posixct_property$default, as.POSIXct(NA))
#
#   posixct_property <- prop_posixct(default = as.POSIXct("2025-03-31", tz = "UTC"))
#
#   expect_equal(posixct_property$default, as.POSIXct("2025-03-31", tz = "UTC"))
# })
#
#
# test_that("the string property is correctly created", {
#   string_property <- prop_string()
#
#   expect_equal(class(string_property), "S7_property")
#   expect_equal(string_property$class$class, "character")
#   expect_equal(string_property$default, NA_character_)
#
#   string_property <- prop_string(default = "test", validator = validate_email)
#
#   expect_equal(string_property$default, "test")
#
#   expect_true(!is.null(environment(string_property[["validator"]])[["validator"]]))
# })
#
# test_that("the numeric property is correctly created", {
#   numeric_property <- prop_numeric()
#
#   expect_equal(class(numeric_property), "S7_property")
#   expect_equal(numeric_property$class$classes[[1]]$class, "integer")
#   expect_equal(numeric_property$default, NA_real_)
#
#   numeric_property <- prop_numeric(default = 1, validator = validate_id)
#
#   expect_equal(numeric_property$default, 1)
#   expect_true(!is.null(environment(numeric_property[["validator"]])[["validator"]]))
# })
#
# test_that("the list property is correctly created", {
#   list_property <- prop_list()
#
#   expect_equal(class(list_property), "S7_property")
#   expect_equal(list_property$class$class, "list")
#   expect_equal(list_property$default, list())
#
#   list_property <- prop_list(default = as.list(c(1,2,3)), validator = validate_natural_number_list)
#
#   expect_equal(list_property$default, as.list(c(1,2,3)))
#   expect_true(!is.null(environment(list_property[["validator"]])[["validator"]]))
# })
#
# test_that("the logical property is correctly created", {
#   logical_property <- prop_logical()
#
#   expect_equal(class(logical_property), "S7_property")
#   expect_equal(logical_property$class$class, "logical")
#
#   logical_property <- prop_logical(default = TRUE)
#
#   expect_equal(logical_property$default, TRUE)
# })
