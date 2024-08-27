#' Fetch Events from the API
#'
#' This function retrieves event data from the API for a specified sport and optional filters.
#'
#' @param sport_name Character. The name of the sport.
#' @param country_name Character. The name of the country. Defaults to NULL.
#' @param date Date. The specific date for events. Defaults to NULL.
#' @param league_id Integer. The ID of the league. Defaults to NULL.
#' @param team_id Integer. The ID of the team. Defaults to NULL.
#' @param skip_esports Logical. Whether to skip esports events. Defaults to FALSE.
#' @param page Integer. The page number for pagination. Defaults to NULL.
#'
#' @return A list containing the events data in JSON format.
#' @export
#' @examples
#' \dontrun{
#' events <- get_events("soccer", "USA", "2023-08-01")
#' }
get_events <- function(sport_name,
                       country_name = NULL,
                       date = NULL,
                       league_id = NULL,
                       team_id = NULL,
                       skip_esports = FALSE,
                       page = NULL) {
  url <- construct_url(sport_name,
                       country_name,
                       date,
                       league_id,
                       team_id,
                       skip_esports,
                       page)

  response <- httr::GET(url)

  if (httr::status_code(response) != 200) {
    stop("Failed to fetch data: ", httr::status_code(response))
  }

  content <- httr::content(response, as = "parsed", type = "application/json")
  return(content)
}

#' Fetch All Pages of Event Data
#'
#' This function retrieves event data from the API across multiple pages.
#'
#' @inheritParams get_events
#' @param max_pages Integer. The maximum number of pages to retrieve. Defaults to 5.
#'
#' @return A list containing event data from all pages.
#' @export
#' @examples
#' \dontrun{
#' all_events <- get_all_pages("soccer", "USA", "2023-08-01")
#' }
get_all_pages <- function(sport_name,
                          country_name = NULL,
                          date = NULL,
                          league_id = NULL,
                          team_id = NULL,
                          skip_esports = FALSE,
                          max_pages = 5) {
  all_results <- list()

  for (page in 1:max_pages) {
    response_list <- get_events(sport_name,
                                country_name,
                                date,
                                league_id,
                                team_id,
                                skip_esports,
                                page)

    if (length(response_list$results) == 0) {
      break
    }

    all_results <- c(all_results, response_list$results)
  }

  return(all_results)
}

#' Retrieve and Process All Games
#'
#' This function fetches all event data and processes it into a data frame of game details.
#'
#' @inheritParams get_all_pages
#'
#' @return A data frame containing all game details.
#' @export
#' @examples
#' \dontrun{
#' games <- get_all_games("soccer", "USA", "2023-08-01")
#' }
get_all_games <- function(sport_name,
                          country_name = NULL,
                          date = NULL,
                          league_id = NULL,
                          team_id = NULL,
                          skip_esports = FALSE,
                          max_pages = 5) {
  all_results <- get_all_pages(sport_name,
                               country_name,
                               date,
                               league_id,
                               team_id,
                               skip_esports,
                               max_pages)
  all_games <- purrr::map(all_results, extract_game_details) |>
    purrr::compact() |>
    dplyr::bind_rows()

  return(all_games)
}
