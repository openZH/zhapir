devtools::load_all(".")

# dataset user facing functions
create_dataset("Pony 4", organisation_id = 14, description = "Such insights, much wow!")

update_dataset(id = 6829, organisation_id = 14, title = "Pony 1 - updated")


# distribution user facing functions
create_distribution(
  title = "Pony Distribution 3",
  description = "description",
  dataset_id = 6831,
  stat_server_flag = TRUE,
  zh_web_flag = TRUE,
  ogd_flag = TRUE,
  file_path = "test_dist.csv"
  #access_url = "https://link.ch",
  #status_id = 1
)

update_distribution(id = 5871, dataset_id = 6831, file_path = "test_dist.csv")

# getter user facting functions
api_request(method = "GET",
            endpoint = "/api/v1/organisation-units",
            api_key =  get_api_key())

get_keywords()


# create and delete a dummy csv file
dummy_data <- data.frame(
  id = 1:3,
  name = c("Gemeinde", "Anzahl Kühe", "Beliebtester Kuhname"),
  value = c("User", 999, "Anneliseli")
)
write.csv(dummy_data, "test_dist.csv", row.names = FALSE)

# Delete the file
file.remove("test_dist.csv")
