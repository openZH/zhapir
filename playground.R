devtools::load_all(".")

# dataset user facing functions
create_dataset("Katze Test 1", organisation_id = 14, description = "Such insights, much wow!")

update_dataset(id = 6814, title = "Katze Test 1 - updated")



# distribution user facing functions
create_distribution(
  title = "Hund Distribution XY",
  dataset_id = 6805,
  stat_server_flag = TRUE,
  zh_web_flag = TRUE,
  ogd_flag = TRUE,
  sort_order = 0,
  description = "string",
  access_url = "https://example.com",
  right = "string",
  byte_size = 0,
  #status_id = 1
)

update_distribution(id = 5845, title = "Hund Distribution - updated 2", status_id = 2)


