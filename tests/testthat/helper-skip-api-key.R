skip_if_no_api_key <- function() {
  if (nzchar(Sys.getenv("ZHAPIR_API_KEY")) == FALSE)
    skip("no ZHAPIR_API_KEY; skipping integration tests")
}
