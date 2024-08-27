get_upcoming_games <- function(res) {
  tibble::tibble(
    game_id = res$id,
    home = res$home.name,
    away = res$away.name,
    date = as.POSIXct(as.numeric(res$time), origin = "1970-01-01", tz = "UTC")
  )
}

get_upcoming_events <- function(sport_name, country_name = NULL, league_id = NULL, date = NULL, page = 1, max_pages = 5) {
  base_url <- "https://api.b365api.com/v3/events/upcoming"

  # Use the mappings to convert sport and country names to IDs
  sport_id <- sport_id_mapping[tolower(sport_name)]
  country_code <- country_code_mapping[tolower(country_name)]

  all_results <- list()

  # Loop to handle pagination
  for (p in 1:max_pages) {
    query_params <- list(
      sport_id = sport_id,
      cc = country_code,
      league_id = league_id,
      day = date,
      page = p,
      token = Sys.getenv("BETSAPI_KEY")
    )

    response <- GET(base_url, query = query_params)
    response_list <- fromJSON(content(response, "text"), flatten = TRUE)

    if (length(response_list$results) == 0) {
      break
    }

    all_results <- c(all_results, response_list$results)
  }

  return(all_results)
}

#' Fetch Upcoming Events from the API
#'
#' This function retrieves a list of upcoming sports events based on the specified criteria.
#'
#' @param sport_name Character. The name of the sport.
#' @param country_name Character. The name of the country. Defaults to NULL.
#' @param league_id Integer. The ID of the league. Defaults to NULL.
#' @param date Character. The specific date for the events in "YYYY-MM-DD" format. Defaults to NULL.
#' @param page Integer. The starting page number for pagination. Defaults to 1.
#' @param max_pages Integer. The maximum number of pages to retrieve. Defaults to 5.
#'
#' @return A list containing details of upcoming events.
#' @export
#' @examples
#' \dontrun{
#' upcoming_events <- get_upcoming_events("soccer", "USA", league_id = 1234, date = "2023-09-01")
#' }
get_upcoming_events <- function(sport_name, country_name = NULL, league_id = NULL, date = NULL, page = 1, max_pages = 5) {
  base_url <- "https://api.b365api.com/v3/events/upcoming"

  # Use the mappings to convert sport and country names to IDs
  sport_id <- sport_id_mapping[tolower(sport_name)]
  country_code <- country_code_mapping[tolower(country_name)]

  all_results <- list()

  # Loop to handle pagination
  for (p in 1:max_pages) {
    query_params <- list(
      sport_id = sport_id,
      cc = country_code,
      league_id = league_id,
      day = date,
      page = p,
      token = Sys.getenv("BETSAPI_KEY")
    )

    response <- httr::GET(base_url, query = query_params)
    response_list <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

    if (length(response_list$results) == 0) {
      break
    }

    all_results <- c(all_results, response_list$results)
  }

  return(all_results)
}
