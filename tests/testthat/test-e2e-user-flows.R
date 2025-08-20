test_that("E2E: status change fails when ogd_flag is missing", {
  skip_if_not_e2e()

  ds <- create_dataset(
    title           = paste0("E2E DS no OGD ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    organisation_id = 14,
    description     = "Intentionally missing ogd_flag for dist status test",
    contact_email   = "team@example.org",
    keyword_ids     = c("abfall"),
    theme_ids       = c("Energie"),
    periodicity_id  = "Jährlich"
  )
  ds_id <- ds$id

  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  write.csv(data.frame(a = 1:3), tf, row.names = FALSE)

  expect_error(
    create_distribution(
      title          = paste0("Dist no OGD ", format(Sys.time(), "%H:%M:%S")),
      dataset_id     = ds$id,
      file_path      = tf,
      license_id     = 1,
      file_format_id = "CSV",
      status_id      = 2   # triggers set_status() internally
    ),
    regexp = "(?i)Request failed \\(400\\).*ogd_flag|zwingend erforderlich|required"
  )
})


# test if with ogd flag the status change is allowed
test_that("E2E: status change succeeds after setting ogd_flag", {
  skip_if_not_e2e()

  ds <- create_dataset(
    title           = paste0("E2E DS with OGD ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    organisation_id = 14,
    description     = "Dataset prepared for publish",
    contact_email   = "team@example.org",
    keyword_ids     = c("abfall"),
    theme_ids       = c("Energie"),
    periodicity_id  = "Jährlich"
  )
  ds_id <- ds$id

  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  write.csv(data.frame(a = 4:6), tf, row.names = FALSE)

  dist <- create_distribution(
    title          = paste0("Dist with OGD ", format(Sys.time(), "%H:%M:%S")),
    dataset_id     = ds_id,
    file_path      = tf,
    license_id     = 1,
    file_format_id = "CSV",
    status_id      = 1
  )

  # Make the distribution eligible (set ogd_flag)
  res_upd <- update_distribution(
    id          = dist$id,
    dataset_id  = ds_id,
    ogd_flag    = TRUE
  )
  expect_true(is.list(res_upd))
})


# tests update + bump end_date
test_that("E2E: update distribution, bump dataset end_date, and advance status with ogd_flag", {
  skip_if_not_e2e()

  ds <- create_dataset(
    title           = paste0("E2E DS for Update ", format(Sys.time(), "%H:%M:%S")),
    organisation_id = 14,
    description     = "Dataset created for update test",
    contact_email   = "update-test@example.org",
    keyword_ids     = c("agglomeration"),
    theme_ids       = c("Bevölkerung und Gesellschaft"),
    periodicity_id  = "Jährlich"
  )
  ds_id <- ds$id

  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  write.csv(data.frame(x = 11:15), tf, row.names = FALSE)

  dist <- create_distribution(
    title          = paste0("E2E Dist to Update ", format(Sys.time(), "%H:%M:%S")),
    dataset_id     = ds_id,
    file_path      = tf,
    license_id     = 2,
    file_format_id = "CSV",
  )
  dist_id <- dist$id
  expect_true(dist_id > 0)

  # Update: bump end_date and set ogd_flag
  res_upd <- update_distribution(
    id          = dist_id,
    dataset_id  = ds_id,
    description = "Updated description",
    end_date    = format(Sys.Date(), "%Y-%m-%d"),
    ogd_flag    = TRUE
  )
  expect_true(is.list(res_upd))
})
