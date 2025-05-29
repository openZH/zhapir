devtools::load_all(".")

create_dataset("Hello Dataset 1", organisation_id = 14, description = "Such insights, much wow!")

create_distribution(title = "Hello Distribution 1", dataset_id = 6819, description = "WOW this is a distribution!")

update_dataset(id = 6798, title = "Wow this title is updated now!")
