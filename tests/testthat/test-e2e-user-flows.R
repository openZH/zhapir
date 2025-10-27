test_that("E2E: distribution cannot exceed dataset status", {
  skip_if_not_e2e()

  # Datensatz anlegen (bleibt im Default-Status, z. B. 'Entwurf')
  ds <- zhapir::create_dataset(
    title           = paste0("E2E DS baseline ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    organisation_id = 14,
    description     = "Negative test: distribution status > dataset status",
    contact_email   = "team@example.org",
    keyword_ids     = c("abfall"),
    theme_ids       = c("Energie"),
    periodicity_id  = "Jährlich"
  )
  ds_id <- ds$id

  # Datei vorbereiten
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  utils::write.csv(data.frame(a = 1:3), tf, row.names = FALSE)

  # Distribution sofort höher einstufen (z. B. 'Publiziert' = 2),
  # obwohl der Datensatz niedriger ist -> MUSS fehlschlagen.
  testthat::expect_error(
    zhapir::create_distribution(
      title          = paste0("E2E Dist > DS ", format(Sys.time(), "%H:%M:%S")),
      dataset_id     = ds_id,
      file_path      = tf,
      license_id     = 1,
      file_format_id = "CSV",
      status_id      = 2
    ),
    # Gruppiert, case-insensitive. Deckt alte und neue Backend-Message ab.
    regexp = "(?i)(Request failed \\(400\\).*(ogd_flag|zwingend erforderlich|required|Status der Distribution darf nicht .* gesetzt werden als ihr Datensatz))"
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


# --- E2E: Status-Validierung nach Create/Update ---

test_that("E2E: dataset ohne start_date ist nicht valid für nächsten Status", {

  skip_if_not_e2e()


  # Minimaler Datensatz OHNE start_date (bewusst invalid für Publish)
  ds <- create_dataset(
    title           = paste0("E2E DS missing start_date ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    organisation_id = 14,
    description     = "Dataset ohne start_date für Negativtest",
    contact_email   = "team@example.org",
    keyword_ids     = c("abfall"),
    theme_ids       = c("Energie"),
    periodicity_id  = "Jährlich"
    # start_date absichtlich weggelassen
  )
  ds_id <- ds$id

  # expect that id is a positive number
  expect_true(ds_id > 0)

  # expect the CLI message
  expect_message(
    {
      resp_invalid <- dataset_is_valid_for_status(
        id = ds_id,
        use_dev = TRUE,
        verbosity = 0,
        fail_on_invalid = FALSE
      )
    },
    regexp = "nicht valid",  # German snippet is stable enough
    perl = TRUE
  )

  # also assert programmatically
  testthat::expect_false(isTRUE(resp_invalid$is_valid))

})


test_that("E2E: dataset wird valid nach Setzen von start_date und Anlegen einer gültigen Distribution", {
  skip_if_not_e2e()

  # Create dataset WITH start_date
  ds <- create_dataset(
    title           = paste0("E2E DS valid path ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    organisation_id = 14,
    description     = "Dataset mit start_date; Distribution wird ergänzt",
    contact_email   = "team@example.org",
    start_date      = format(Sys.Date() - 365, "%Y-%m-%d"),
    keyword_ids     = c("abfall"),
    theme_ids       = c("Energie"),
    periodicity_id  = "Jährlich"
  )
  ds_id <- ds$id
  expect_true(ds_id > 0)

  # Prepare a small CSV for upload
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  utils::write.csv(data.frame(a = 1:3), tf, row.names = FALSE)

  # Create a valid distribution
  dist <- create_distribution(
    title          = paste0("E2E Dist valid for DS ", format(Sys.time(), "%H:%M:%S")),
    dataset_id     = ds_id,
    file_path      = tf,
    license_id     = 1,
    file_format_id = "CSV",
    status_id      = 1
  )
  expect_true(is.list(dist))
  expect_true(dist$id > 0)

  # Now the status check should be valid
  expect_message(
    {
      resp_ok <- dataset_is_valid_for_status(
        id = ds_id,
        use_dev = TRUE,
        verbosity = 0,
        fail_on_invalid = FALSE
      )
    },
    regexp = "ist .*valid",
    perl   = TRUE
  )
  expect_true(isTRUE(resp_ok$is_valid))
  # and definitely not "nicht valid"
  msg_ok <- testthat::capture_messages(
    dataset_is_valid_for_status(
      id = ds_id,
      use_dev = TRUE,
      verbosity = 0,
      fail_on_invalid = FALSE
    )
  )
  expect_false(grepl("nicht valid", msg_ok, perl = TRUE))
})


test_that("E2E: update_dataset setzt start_date nachträglich; mit Distribution wird Dataset valid", {
  skip_if_not_e2e()

  # Start invalid (no start_date)
  ds <- create_dataset(
    title           = paste0("E2E DS to fix via update ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    organisation_id = 14,
    description     = "Wird via update_dataset repariert",
    contact_email   = "team@example.org",
    keyword_ids     = c("abfall"),
    theme_ids       = c("Energie"),
    periodicity_id  = "Jährlich"
    # start_date intentionally omitted
  )
  ds_id <- ds$id
  expect_true(ds_id > 0)

  # Fix: set start_date
  upd <- update_dataset(
    id         = ds_id,
    start_date = format(Sys.Date() - 30, "%Y-%m-%d")
  )
  expect_true(is.list(upd))

  # Add a valid distribution
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  utils::write.csv(data.frame(b = 4:6), tf, row.names = FALSE)

  dist <- create_distribution(
    title          = paste0("E2E Dist after update ", format(Sys.time(), "%H:%M:%S")),
    dataset_id     = ds_id,
    file_path      = tf,
    license_id     = 1,
    file_format_id = "CSV",
    status_id      = 1
  )
  expect_true(is.list(dist))
  expect_true(dist$id > 0)

  # Now should be valid
  expect_message(
    {
      resp_ok2 <- dataset_is_valid_for_status(
        id = ds_id,
        use_dev = TRUE,
        verbosity = 0,
        fail_on_invalid = FALSE
      )
    },
    regexp = "ist .*valid",
    perl   = TRUE
  )
  expect_true(isTRUE(resp_ok2$is_valid))
})
