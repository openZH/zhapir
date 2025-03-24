login_to_api <- function(username, password, use_dev = TRUE) {
  base_url <- if(use_dev) "https://mdv-dev.nebula.statzh.ch" else "https://mdv.nebula.statzh.ch"

  # Create a temporary file for storing cookies
  cookie_file <- tempfile()

  # Make login request with cookie preservation
  req <- httr2::request(paste0(base_url, "/api/v1/auth/login")) |>
    httr2::req_headers(
      "Content-Type" = "application/json",
      "Accept" = "application/json, application/problem+json"
    ) |>
    httr2::req_body_json(
      list(
        username = username,
        password = password
      )
    ) |>
    httr2::req_method("POST") |>
    httr2::req_cookie_preserve(cookie_file)

  # Perform the request
  response <- httr2::req_perform(req)

  # Read the cookie file content
  cookie_content <- readLines(cookie_file)

  # Find the session-id line
  session_id_line <- cookie_content[grep("session-id", cookie_content)]
  if (length(session_id_line) == 0) {
    stop("No session-id cookie found")
  }

  # Parse the cookie fields
  fields <- strsplit(session_id_line, "\t")[[1]]
  cookie_value <- fields[length(fields)]

  # Create the cookie string
  cookie_str <- paste0("session-id=", cookie_value)

  # Clean up
  unlink(cookie_file)

  # Return the authentication information
  return(list(
    cookie = cookie_str,
    user_info = httr2::resp_body_json(response)
  ))
}
