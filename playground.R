devtools::load_all(".")

# dataset user facing functions
create_dataset("Pony 1", organisation_id = 14, description = "Such insights, much wow!")

update_dataset(id = 6829, organisation_id = 14, title = "Pony 1 - updated")



# distribution user facing functions
create_distribution(
  title = "Pony Distribution 3",
  dataset_id = 6829,
  stat_server_flag = TRUE,
  zh_web_flag = TRUE,
  ogd_flag = TRUE,
  sort_order = 0,
  description = "string",
  access_url = "https://example.com",
  right = "string",
  byte_size = 0
  #status_id = 1
)

update_distribution(id = 5822, dataset_id = 6829, title = "Hund Distribution - updated 2")

api_request(method = "GET",
            endpoint = "/api/v1/organisation-units",
            api_key =  get_api_key())


get_keywords()


# Test File upload
create_file("test_dist.csv")


