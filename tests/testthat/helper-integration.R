# Opt-in only: never run e2e unless the user explicitly asks for it.
skip_if_not_e2e <- function() {
  # Hard stop on CI
  testthat::skip_on_ci()

  # Explicit opt-in switch
  if (Sys.getenv("ZHAPIR_RUN_E2E") != "1") {
    testthat::skip("Set ZHAPIR_RUN_E2E=1 in .Renviron file to run end-to-end integration tests (local only).")
  }

  # Require key
  if (!nzchar(Sys.getenv("ZHAPIR_API_KEY"))) {
    testthat::skip("ZHAPIR_API_KEY missing; cannot run end-to-end tests.")
  }
}


skip_if_no_api_key <- function() {
  if (nzchar(Sys.getenv("ZHAPIR_API_KEY")) == FALSE)
    skip("no ZHAPIR_API_KEY; skipping integration tests")
}
