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

update_distribution(id = 5810, dataset_id = 6809, title = "Hund Distribution - updated 2", status_id = 2)

api_request(method = "GET",
            endpoint = "/api/v1/organisation-units",
            api_key =  get_api_key())

api_request_wrapper(method = "GET",
                    endpoint = "/api/v1/organisation-units",
                    api_key = get_api_key()
                    )

df <- get_keywords()


