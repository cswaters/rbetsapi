#' Fetch Event Odds Summary
#'
#' This function retrieves a summary of odds for a specific event using the BetsAPI.
#'
#' @param event_id Character. The unique identifier for the event.
#'
#' @return A list containing the summary of odds for the event.
#' @export
#' @examples
#' \dontrun{
#' odds_summary <- get_event_odds_summary("123456")
#' }
get_event_odds_summary <- function(event_id) {
  base_url <- "https://api.b365api.com/v2/event/odds/summary"

  # Build the query parameters
  query_params <- list(event_id = event_id, token = Sys.getenv("BETSAPI_KEY"))

  # Make the API request
  response <- httr::GET(base_url, query = query_params)
  response_list <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

  return(response_list)
}

#' Extract BetsAPI Odds
#'
#' This function extracts odds information from a BetsAPI response for a specific sportsbook.
#'
#' @param res A list representing the API response containing odds data.
#' @param sportsbook Character. The name of the sportsbook. Defaults to "PinnacleSports".
#'
#' @return A tibble containing the open and close odds for moneyline, spread, and total.
#' @export
#' @examples
#' \dontrun{
#' odds_data <- get_betsapi_odds(odds_summary, "PinnacleSports")
#' }
get_betsapi_odds <- function(res, sportsbook = "PinnacleSports") {
  open_ml <- res$results[[sportsbook]]$odds$start$`18_1`
  open_spread <- res$results[[sportsbook]]$odds$start$`18_2`
  open_total <- res$results[[sportsbook]]$odds$start$`18_3`

  close_ml <- res$results[[sportsbook]]$odds$kickoff$`18_1`
  close_spread <- res$results[[sportsbook]]$odds$kickoff$`18_2`
  close_total <- res$results[[sportsbook]]$odds$kickoff$`18_3`

  tibble::tibble(
    open_ml_time = as.POSIXct(
      as.numeric(open_ml$add_time),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    open_ml_home = open_ml$home_od,
    open_ml_away = open_ml$away_od,
    open_spread_time = as.POSIXct(
      as.numeric(open_spread$add_time),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    open_spread_line = open_spread$handicap,
    open_spread_home = open_spread$home_od,
    open_spread_away = open_spread$away_od,
    open_total_time = as.POSIXct(
      as.numeric(open_total$add_time),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    open_total_line = open_total$handicap,
    open_total_over = open_total$over_od,
    open_total_under = open_total$under_od,
    close_ml_time = as.POSIXct(
      as.numeric(close_ml$add_time),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    close_ml_home = close_ml$home_od,
    close_ml_away = close_ml$away_od,
    close_spread_time = as.POSIXct(
      as.numeric(close_spread$add_time),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    close_spread_line = close_spread$handicap,
    close_spread_home = close_spread$home_od,
    close_spread_away = close_spread$away_od,
    close_total_time = as.POSIXct(
      as.numeric(close_total$add_time),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    close_total_line = close_total$handicap,
    close_total_over = close_total$over_od,
    close_total_under = close_total$under_od
  )
}

#' Retrieve All Event Odds
#'
#' This function retrieves and processes odds for a list of events from BetsAPI.
#'
#' @param event_ids A character vector of event IDs.
#' @param sportsbook Character. The name of the sportsbook. Defaults to "PinnacleSports".
#'
#' @return A tibble containing the odds for each event.
#' @export
#' @examples
#' \dontrun{
#' all_odds <- get_all_event_odds(c("123456", "789012"), "PinnacleSports")
#' }
get_all_event_odds <- function(event_ids, sportsbook = "PinnacleSports") {
  odds_list <- lapply(event_ids, get_event_odds_summary)
  odds_df <- purrr::map_dfr(odds_list, get_betsapi_odds, sportsbook = sportsbook)
  tibble::tibble(event_id = event_ids, odds_df |> dplyr::mutate(dplyr::across(where(is.character), as.numeric)))
}

#' Fetch Full Event Odds
#'
#' This function retrieves detailed odds information for a specific event, with options to filter by odds type, source, and time.
#'
#' @param event_id Character. The unique identifier for the event.
#' @param odds_types Character vector. The types of odds to retrieve. Defaults to NULL.
#' @param source Character. The source of the odds. Defaults to NULL.
#' @param since_time Numeric. A Unix timestamp to filter odds since that time. Defaults to NULL.
#'
#' @return A list containing the detailed odds for the event.
#' @export
#' @examples
#' \dontrun{
#' full_odds <- get_event_odds_full("123456", odds_types = c("moneyline", "spread"))
#' }
get_event_odds_full <- function(event_id, odds_types = NULL, source = NULL, since_time = NULL) {
  base_url <- "https://api.b365api.com/v2/event/odds"

  # Map odds types to market keys
  if (!is.null(odds_types)) {
    market_keys <- unlist(lapply(tolower(odds_types), function(x) odds_type_to_market_key[x]))
    odds_market <- paste(market_keys, collapse = ",")
  } else {
    odds_market <- NULL
  }

  # Build the query parameters
  query_params <- list(
    event_id = event_id,
    source = source,
    since_time = since_time,
    odds_market = odds_market,
    token = Sys.getenv("BETSAPI_KEY")
  )

  # Make the API request
  response <- httr::GET(base_url, query = query_params)
  response_list <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

  return(response_list)
}


