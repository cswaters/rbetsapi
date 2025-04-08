#' Construct API URL for BetsAPI
#'
#' @param endpoint Character. The API endpoint to use. Defaults to EVENTS_ENDED.
#' @param sport_name Character. Name of the sport.
#' @param country_name Character. Optional. Name of the country.
#' @param date Character. Optional. Date in YYYY-MM-DD format.
#' @param league_id Integer. Optional. ID of the league.
#' @param team_id Integer. Optional. ID of the team.
#' @param event_id Character. Optional. ID of the event.
#' @param skip_esports Logical. Optional. Whether to skip esports events.
#' @param page Integer. Optional. Page number for pagination.
#' @param ... Additional parameters to include in the URL.
#'
#' @return Character. Fully constructed API URL.
#'
#' @details
#' This function constructs the URL for the BetsAPI endpoint, incorporating
#' various query parameters based on the provided arguments. It uses environment
#' variables for the API key and internal mappings for sport IDs and country codes.
#'
#' @examples
#' \dontrun{
#' url <- construct_url(endpoint = "EVENTS_ENDED", 
#'                      sport_name = "soccer", 
#'                      country_name = "England", 
#'                      date = "2024-08-27")
#' }
#'
#' @importFrom utils URLencode
#' @export
construct_url <- function(endpoint = "EVENTS_ENDED",
                           sport_name = NULL,
                           country_name = NULL,
                           date = NULL,
                           league_id = NULL,
                           team_id = NULL,
                           event_id = NULL,
                           skip_esports = NULL,
                           page = NULL,
                           ...) {
  # Get the base URL from the endpoints list
  base_url <- BETSAPI_ENDPOINTS[[endpoint]]
  if (is.null(base_url)) {
    stop("Invalid endpoint specified: ", endpoint)
  }
  
  # Get API key
  api_key <- get_api_key()
  
  # Initialize params list with common parameters
  params <- list(token = api_key, ...)
  
  # If sport_name is provided, convert to sport_id
  if (!is.null(sport_name)) {
    sport_id <- sport_id_mapping[tolower(sport_name)]
    if (is.null(sport_id) || length(sport_id) == 0) {
      stop("Invalid sport name provided: ", sport_name)
    }
    params$sport_id <- sport_id
  }
  
  # If country_name is provided, convert to country code
  if (!is.null(country_name)) {
    country_code <- country_code_mapping[tolower(country_name)]
    if (is.null(country_code) || length(country_code) == 0) {
      stop("Invalid country name provided: ", country_name)
    }
    params$cc <- country_code
  }
  
  # Add other parameters if they're not NULL
  if (!is.null(date)) params$day <- date
  if (!is.null(league_id)) params$league_id <- league_id
  if (!is.null(team_id)) params$team_id <- team_id
  if (!is.null(event_id)) params$event_id <- event_id
  if (!is.null(skip_esports)) params$skip_esports <- as.integer(skip_esports)
  if (!is.null(page)) params$page <- page
  
  # Build the query string
  query_parts <- vapply(names(params), function(key) {
    if (!is.null(params[[key]])) {
      paste0(key, "=", URLencode(as.character(params[[key]]), reserved = TRUE))
    } else {
      NULL
    }
  }, character(1))
  
  # Filter out NULL entries and join with "&"
  query_string <- paste(stats::na.omit(query_parts), collapse = "&")
  
  # Return the full URL
  paste0(base_url, "?", query_string)
} 