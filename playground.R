devtools::load_all(".")

# dataset user facing functions
create_dataset("Katze Test 1", organisation_id = 14, description = "Such insights, much wow!")

update_dataset(id = 6805, title = "Katze Test 1 - updated")



# distribution user facing functions
create_distribution(
  title = "Hund Distribution",
  dataset_id = 6811,
  stat_server_flag = TRUE,
  zh_web_flag = TRUE,
  ogd_flag = TRUE,
  sort_order = 0,
  description = "string",
  access_url = "https://example.com",
  right = "string",
  byte_size = 0,
  status_id = 1,
  license_id = NULL,
  format_id = NULL,
  media_type_id = NULL,
  periodicity_id = NULL,
  file_upload_id = NULL
)

update_distribution(id = 5816, title = "Hund Distribution - updated", status_id = 1)


