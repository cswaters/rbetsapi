#' Create a fetcher for betting odds from BetsAPI
#' 
#' Creates an object with functions to fetch and process betting odds from BetsAPI.
#' The fetcher provides methods to retrieve opening and closing odds for moneyline,
#' spread, and total markets for specified events.
#' 
#' @param api_token API token for BetsAPI. If NULL, the function will attempt to
#'   use the BETSAPI_KEY environment variable.
#'   
#' @return A list containing the following functions:
#'   \itemize{
#'     \item \code{fetch_event_data}: Fetches odds for a single event
#'       from a specified sportsbook
#'     \item \code{fetch_and_combine_odds}: Fetches odds for multiple events
#'       and combines them into a single data frame
#'   }
#'   
#' @export
#' @examples
#' \dontrun{
#' # Create a fetcher using an API token
#' odds_fetcher <- create_event_odds_fetcher("your_api_token")
#' 
#' # Fetch odds for a single event from Pinnacle
#' event_odds <- odds_fetcher$fetch_event_data("123456", "PinnacleSports")
#' 
#' # Fetch and combine odds for multiple events
#' events <- c("123456", "789012")
#' combined_odds <- odds_fetcher$fetch_and_combine_odds(events)
#' }
create_event_odds_fetcher <- function(api_token = NULL) {
  
  # Use environment variable if token not provided
  api_token <- api_token %||% get_api_key()
  
  # Function to fetch event data
  fetch_event_data <- function(event_id, sportsbook) {
    params <- list(
      event_id = event_id,
      token = api_token
    )
    
    tryCatch({
      # Fetch the odds summary from the API
      url <- BETSAPI_ENDPOINTS$ODDS_SUMMARY
      response <- httr::GET(url, query = params)
      httr::stop_for_status(response)
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      res <- jsonlite::fromJSON(content_text, simplifyVector = FALSE)
      
      # Extract relevant odds information
      results <- safe_extract(res, "results", list())
      sportsbook_data <- safe_extract(results, sportsbook, list())
      
      # Check if we have data for this sportsbook
      if (length(sportsbook_data) == 0) {
        message(sprintf("No data available for sportsbook '%s' in event_id=%s", sportsbook, event_id))
        return(data.frame())
      }
      
      odds <- safe_extract(sportsbook_data, "odds", list())
      start_odds <- safe_extract(odds, "start", list())
      end_odds <- safe_extract(odds, "end", list())
      
      open_ml <- safe_extract(start_odds, "18_1", list())
      open_spread <- safe_extract(start_odds, "18_2", list())
      open_total <- safe_extract(start_odds, "18_3", list())
      
      close_ml <- safe_extract(end_odds, "18_1", list())
      close_spread <- safe_extract(end_odds, "18_2", list())
      close_total <- safe_extract(end_odds, "18_3", list())
      
      # Create a one-row data frame with NA for missing values
      df <- data.frame(
        game_id = event_id,
        stringsAsFactors = FALSE
      )
      
      # Safely add each column with consistent length
      df$open_ml_time <- convert_to_datetime(safe_extract(open_ml, "add_time"))
      df$open_ml_home <- safe_extract(open_ml, "home_od")
      df$open_ml_away <- safe_extract(open_ml, "away_od")
      
      df$open_spread_time <- convert_to_datetime(safe_extract(open_spread, "add_time"))
      df$open_spread_line <- safe_extract(open_spread, "handicap")
      df$open_spread_home <- safe_extract(open_spread, "home_od")
      df$open_spread_away <- safe_extract(open_spread, "away_od")
      
      df$open_total_time <- convert_to_datetime(safe_extract(open_total, "add_time"))
      df$open_total_line <- safe_extract(open_total, "handicap")
      df$open_total_over <- safe_extract(open_total, "over_od")
      df$open_total_under <- safe_extract(open_total, "under_od")
      
      df$close_ml_time <- convert_to_datetime(safe_extract(close_ml, "add_time"))
      df$close_ml_home <- safe_extract(close_ml, "home_od")
      df$close_ml_away <- safe_extract(close_ml, "away_od")
      
      df$close_spread_time <- convert_to_datetime(safe_extract(close_spread, "add_time"))
      df$close_spread_line <- safe_extract(close_spread, "handicap")
      df$close_spread_home <- safe_extract(close_spread, "home_od")
      df$close_spread_away <- safe_extract(close_spread, "away_od")
      
      df$close_total_time <- convert_to_datetime(safe_extract(close_total, "add_time"))
      df$close_total_line <- safe_extract(close_total, "handicap")
      df$close_total_over <- safe_extract(close_total, "over_od")
      df$close_total_under <- safe_extract(close_total, "under_od")
      
      return(df)
      
    }, error = function(e) {
      sprintf("Failed to fetch data for event_id=%s: %s", event_id, e$message) |> 
        message()
      data.frame()  # Return an empty data frame
    })
  }
  
  # Function to fetch and combine odds
  fetch_and_combine_odds <- function(event_ids, sportsbook = "PinnacleSports") {
    results <- list()
    total_games <- length(event_ids)
    games_downloaded <- 0
    
    # Initial progress message
    message(sprintf("%d games to download", total_games))
    
    for (game_id in event_ids) {
      tryCatch({
        df <- fetch_event_data(game_id, sportsbook)
        if (nrow(df) > 0) {
          results <- c(results, list(df))
          games_downloaded <- games_downloaded + 1
          # Progress update message
          message(sprintf("%d games downloaded, %d games to go", 
                          games_downloaded, total_games - games_downloaded))
        }
      }, error = function(e) {
        message(sprintf("Error processing event_id=%s: %s", game_id, e$message))
      })
    }
    
    if (length(results) > 0) {
      return(dplyr::bind_rows(results))
    } else {
      message("No valid data was fetched.")
      return(data.frame())
    }
  }
  
  # Return list of functions
  list(
    fetch_event_data = fetch_event_data,
    fetch_and_combine_odds = fetch_and_combine_odds
  )
}

#' Query Event Odds from BetsAPI
#'
#' This function retrieves odds history for a specific event from BetsAPI using the
#' /v2/event/odds endpoint. Unlike the odds/summary endpoint, this provides the
#' full odds history for an event rather than just opening and closing lines.
#'
#' @param event_id Character. The unique identifier for the event.
#' @param source Character. The sportsbook source (default: "bet365").
#' @param since_time Numeric. Optional Unix timestamp to get odds changes since a specific time.
#' @param odds_market Character vector. Market types to fetch. Defaults to c("1", "2", "3")
#'   which represents moneyline, spread, and totals markets.
#'
#' @return A list containing the full odds history for the specified markets.
#' @export
#' @examples
#' \dontrun{
#' # Get full odds history for an event
#' odds_history <- query_event_odds("123456")
#' 
#' # Get odds changes since a specific time
#' odds_updates <- query_event_odds("123456", since_time = 1635000000)
#' }
query_event_odds <- function(event_id,
                             source = "bet365",
                             since_time = NULL,
                             odds_market = c("1", "2", "3")) {
  # Build the query parameters
  query_params <- list(
    event_id = event_id,
    source = source,
    odds_market = paste(odds_market, collapse = ","),
    token = get_api_key()
  )

  # Optionally add since_time only if it's specified
  if (!is.null(since_time)) {
    query_params$since_time <- since_time
  }

  # Make the API request
  url <- BETSAPI_ENDPOINTS$ODDS_HISTORY
  tryCatch({
    response <- httr::GET(url, query = query_params)
    
    # Check for a successful response
    httr::stop_for_status(response)
    
    # Parse the JSON response
    raw_content <- httr::content(response, "text")
    response_list <- jsonlite::fromJSON(raw_content, flatten = TRUE)
    
    return(response_list)
  }, error = function(e) {
    stop("Failed to retrieve event odds: ", e$message)
  })
}

#' Get Most Recent Odds for an Event
#'
#' Extracts the most recent odds from a query_event_odds response for each market type.
#' This is useful for getting the current odds rather than the full odds history.
#'
#' @param res A list. The response from query_event_odds().
#' @param game_id Character. The game identifier to include in the output.
#'
#' @return A tibble with one row containing the most recent odds data across all markets.
#' @export
#' @examples
#' \dontrun{
#' # Get the most recent odds for a game
#' odds_response <- query_event_odds("123456")
#' current_odds <- get_most_recent_odds(odds_response, "123456")
#' }
get_most_recent_odds <- function(res, game_id) {
  # Check if response contains expected data
  if (is.null(res) || is.null(res$results) || is.null(res$results$odds)) {
    warning("Invalid odds response format")
    return(tibble::tibble())
  }
  
  # Extract the odds data
  odds <- res$results$odds

  # Helper function to get the most recent entry for each market
  get_most_recent <- function(odds_market) {
    # Handle empty market
    if (is.null(odds_market) || length(odds_market) == 0) {
      return(list(
        home_od = NA,
        away_od = NA,
        handicap = NA,
        over_od = NA,
        under_od = NA,
        add_time = NA
      ))
    }
    
    # Find most recent entry by timestamp
    most_recent_idx <- which.max(as.numeric(odds_market$add_time))
    
    return(list(
      home_od = odds_market$home_od[most_recent_idx],
      away_od = odds_market$away_od[most_recent_idx],
      handicap = if ("handicap" %in% names(odds_market)) odds_market$handicap[most_recent_idx] else NA,
      over_od = if ("over_od" %in% names(odds_market)) odds_market$over_od[most_recent_idx] else NA,
      under_od = if ("under_od" %in% names(odds_market)) odds_market$under_od[most_recent_idx] else NA,
      add_time = convert_to_datetime(odds_market$add_time[most_recent_idx])
    ))
  }

  # Get the most recent odds for spread, moneyline, and total
  tryCatch({
    most_recent_spread <- get_most_recent(odds$`18_2`)
    most_recent_moneyline <- get_most_recent(odds$`18_1`)
    most_recent_total <- get_most_recent(odds$`18_3`)
    
    # Calculate max timestamp
    timestamp <- max(
      most_recent_spread$add_time, 
      most_recent_moneyline$add_time, 
      most_recent_total$add_time,
      na.rm = TRUE
    )
    
    # Create a tibble with the most recent data (1 row only)
    tibble::tibble(
      game_id = game_id,
      spread_handicap = most_recent_spread$handicap,
      spread_home_odds = most_recent_spread$home_od,
      spread_away_odds = most_recent_spread$away_od,
      moneyline_home_odds = most_recent_moneyline$home_od,
      moneyline_away_odds = most_recent_moneyline$away_od,
      total_points_line = most_recent_total$handicap,
      total_over_odds = most_recent_total$over_od,
      total_under_odds = most_recent_total$under_od,
      timestamp = timestamp
    )
  }, error = function(e) {
    warning(sprintf("Error processing odds for game_id %s: %s", game_id, e$message))
    tibble::tibble()
  })
}

#' Fetch Opening and Closing Odds for Multiple Events
#'
#' A convenience function to fetch opening and closing odds for multiple events 
#' using the recommended fetcher API.
#'
#' @param event_ids A character vector of event IDs.
#' @param sportsbook Character. The name of the sportsbook. Defaults to "PinnacleSports".
#' @param api_token Character. Optional. API token for BetsAPI. If NULL, uses environment variable.
#'
#' @return A data frame with opening and closing odds for all events.
#' @export
#' @examples
#' \dontrun{
#' odds_data <- get_event_opening_closing_odds(c("123456", "789012"))
#' }
get_event_opening_closing_odds <- function(event_ids, sportsbook = "PinnacleSports", api_token = NULL) {
  fetcher <- create_event_odds_fetcher(api_token)
  fetcher$fetch_and_combine_odds(event_ids, sportsbook)
} 