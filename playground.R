devtools::load_all(".")

# dataset user facing functions
create_dataset("Katze Test 2", organisation_id = 14, description = "Such insights, much wow!", keywords = c("wasser"))

create_dataset("maus Test 1", organisation_id = 14, description = "Such insights, much wow!",
               keywords = c("wasser", "s-bahn"),
               periodicity_id = "jährlich")

update_dataset(id = 6809, organisation_id = 14, title = "Katze Test 1 - updated", keywords = c("Wasser", "abwasser"),
               see_also_ids = "Hotels [Anz.]")

create_dataset(
  title = "test bla bla bla 2",
  organisation_id = 14,
  description = "Such insights, much wow!",
  contact_email = "test.test@blabla.com",
  landing_page = "https://test.ch",
  issued = "2025-03-31",
  start_date = "2023-03-31",
  end_date = "2025-03-31",
  modified = "2025-03-31",
  #modified_next = "2026-03-31",
  keyword_ids = c("abfall", "abfallanlagen", "volksschule"),
  zh_web_catalogs = c("Bevölkerung", "Wahlarchiv"),

  relation_ids = NULL,
  theme_ids = c("Energie", "Gesundheit"),
  periodicity_id = "Jährlich",
  see_also_ids = NULL)



update_dataset(id = 6829, organisation_id = 14, title = "Pony 1 - updated")


create_distribution(
  title = "Hund Distribution XY123",
  dataset_id = 6809,
  stat_server_flag = TRUE,
  zh_web_flag = TRUE,
  ogd_flag = TRUE,
  sort_order = 0,
  description = "string",
  access_url = "https://example.com",
  right = "string",
  byte_size = 0,
  periodicity_id = "jährlich",
  status_id = "Entwurf"

  #status_id = 1
)

update_distribution(id = 5871, dataset_id = 6831, file_path = "test_dist.csv")

# getter user facting functions
api_request(method = "GET",
            endpoint = "/api/v1/organisation-units",
            api_key =  get_api_key())

api_request_wrapper(method = "GET",
                    endpoint = "/api/v1/organisation-units",
                    api_key = get_api_key()
                    )

df <- get_keywords()




# create and delete a dummy csv file
dummy_data <- data.frame(
  id = 1:3,
  name = c("Gemeinde", "Anzahl Kühe", "Beliebtester Kuhname"),
  value = c("User", 999, "Anneliseli")
)
write.csv(dummy_data, "test_dist.csv", row.names = FALSE)

# Delete the file
file.remove("test_dist.csv")
