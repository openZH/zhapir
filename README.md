 <!-- badges: start -->
  [![R-CMD-check](https://github.com/openZH/zhapir/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/openZH/zhapir/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->


# zhapir

## API-Endpoint-specification

When creating/updating a dataset or a distribution, you have to specify the values for the respective API-Enpoints. For a dataset, for example, you have to specify relevant keywords (among other endpoint values). To address this, `zahpir` offers two related function types: 
1. **get_\[...]-Functions:** Retrieve all possible values and the corresponding IDs of a given endpoint 
```
# Example for the endpoint 'keywords'

get_keywords()
```
This returns you a data frame with a column for the endpoint entries and one for their corresponding IDs.

2. **get_\[...]_id-Functions:** Retrieve the ID associated with a keyword
```
# Example for the endpoint 'keywords'

get_keywords_id(c("abfall", "abgase", "luftqualitaet"))
```
This returns you a named vector of IDs corresponding to the input name(s).


Note that that in theory, you can retrieve the ID's from the `get_keywords`-ouput manually. However, unless specified differently, it is more transparend to specify the a new dataset or distribution using the actual values of the endpoints. 

```
create_dataset("Hello Dataset 1", organisation_id = 14, description = "Such insights, much wow!")
```

Example:
```
create_dataset("Hello Dataset 1",
               organisation_id = 14,
               description = "Such insights, much wow!",
               keyword_ids  = get_keywords_id(c("abfall", "abgase", "luftqualitaet"))
               )
```

To get a list of all endpoint-get-function and endpoint where you have to specify the actual ID, see **XXXXXX**.
