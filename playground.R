devtools::load_all(".")

# dataset user facing functions
create_dataset("Tabea Test 1", organisation_id = 14, description = "Such insights, much wow!")

update_dataset(id = 6799, title = "Tabea Test 1 - updated")



# distribution user facing functions
create_distribution(
  title = "Tabea Distribution",
  dataset_id = 6800,
  stat_server_flag = TRUE,
  zh_web_flag = TRUE,
  ogd_flag = TRUE,
  sort_order = 0,
  description = "string",
  modified = NULL,
  access_url = "https://example.com",
  right = "string",
  issued = "2019-08-24T14:15:22Z",
  byte_size = 0,
  license_id = NULL,
  format_id = NULL,
  media_type_id = NULL,
  periodicity_id = NULL,
  file_upload_id = NULL
)

update_distribution(id = 5814, title = "Tabea Distribution - updated")


