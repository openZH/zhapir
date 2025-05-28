devtools::load_all(".")

create_dataset("Hello Dataset 1",
               organisation_id = 14,
               description = "Such insights, much wow!",
               keyword_ids  = get_keywords_id(c("abfall", "abgase", "luftqualitaet"))
               )

create_distribution(title = "Hello Distribution 1", dataset_id = 6819, description = "WOW this is a distribution!")

api_request(method = "GET",
            endpoint = "/api/v1/organisation-units",
            api_key =  get_api_key())


get_keywords()
get
