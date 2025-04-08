#' Extract Upcoming Games Information
#'
#' This function extracts essential game information from a BetsAPI upcoming events response.
#'
#' @param res A list. The API response containing upcoming game data.
#'
#' @return A tibble with columns for game_id, home team, away team, and game date.
#' @export
#' @examples
#' \dontrun{
#' upcoming_response <- get_upcoming_events("basketball", "USA")
#' games_info <- get_upcoming_games(upcoming_response)
#' }
get_upcoming_games <- function(res) {
  # Handle empty or invalid responses
  if (is.null(res) || length(res) == 0) {
    warning("Empty or invalid response provided")
    return(tibble::tibble())
  }
  
  tibble::tibble(
    game_id = res$id,
    home = res$home.name,
    away = res$away.name,
    date = convert_to_datetime(res$time)
  )
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
get_upcoming_events <- function(sport_name, 
                                country_name = NULL, 
                                league_id = NULL, 
                                date = NULL, 
                                page = 1, 
                                max_pages = 5) {
  
  return(get_all_pages(
    sport_name = sport_name,
    country_name = country_name,
    date = date,
    league_id = league_id,
    max_pages = max_pages,
    endpoint = "EVENTS_UPCOMING"
  ))
}

#' Get All Upcoming Games
#'
#' This function combines fetching upcoming events and processing them into a clean
#' data frame of upcoming games in a single convenient call.
#'
#' @inheritParams get_upcoming_events
#'
#' @return A tibble with upcoming games information.
#' @export
#' @examples
#' \dontrun{
#' games <- get_all_upcoming_games("basketball", "USA", league_id = 12345)
#' }
get_all_upcoming_games <- function(sport_name, 
                                   country_name = NULL, 
                                   league_id = NULL, 
                                   date = NULL, 
                                   max_pages = 5) {
  
  upcoming_events <- get_upcoming_events(
    sport_name = sport_name,
    country_name = country_name, 
    league_id = league_id,
    date = date,
    max_pages = max_pages
  )
  
  # Process the events into a clean data frame
  if (length(upcoming_events) == 0) {
    message("No upcoming games found")
    return(tibble::tibble())
  }
  
  # Map each result through the processor and combine
  result <- purrr::map_dfr(upcoming_events, get_upcoming_games)
  
  message(sprintf("Retrieved %d upcoming games", nrow(result)))
  return(result)
} 