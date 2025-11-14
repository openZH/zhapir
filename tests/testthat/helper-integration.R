# tests/testthat/helper-integration

skip_if_ci <- function() {
  testthat::skip_on_ci()
}

skip_if_no_dev_token <- function() {
  if (!nzchar(Sys.getenv("MDV_DEV_API_TOKEN_TEST"))) {
    testthat::skip("MDV_DEV_API_TOKEN_TEST missing; cannot run dev API tests.")
  }
}
