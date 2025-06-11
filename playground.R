devtools::load_all(".")

# dataset user facing functions
create_dataset("Pony 3", organisation_id = 14, description = "Such insights, much wow!")

update_dataset(id = 6829, organisation_id = 14, title = "Pony 1 - updated")



# distribution user facing functions
create_distribution(
  title = "Pony Distribution 3",
  description = "description",
  dataset_id = 6831,
  stat_server_flag = TRUE,
  zh_web_flag = TRUE,
  ogd_flag = TRUE
  #file_path = "test_dist.csv"
  #access_url = "https://link.ch",
  #status_id = 1
)

update_distribution(id = 5871, dataset_id = 6831, file_path = "test_dist.csv")

api_request(method = "GET",
            endpoint = "/api/v1/organisation-units",
            api_key =  get_api_key())


get_keywords()



