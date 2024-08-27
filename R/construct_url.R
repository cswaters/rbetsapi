#' Construct API URL for BetsAPI
#'
#' @param sport_name Character. Name of the sport.
#' @param country_name Character. Optional. Name of the country.
#' @param date Character. Optional. Date in YYYY-MM-DD format.
#' @param league_id Integer. Optional. ID of the league.
#' @param team_id Integer. Optional. ID of the team.
#' @param skip_esports Logical. Optional. Whether to skip esports events.
#' @param page Integer. Optional. Page number for pagination.
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
#' url <- construct_url("soccer", country_name = "England", date = "2024-08-27")
#' }
#'
#' @importFrom utils URLencode
#' @export
#'
construct_url <- function(sport_name,
                          country_name = NULL,
                          date = NULL,
                          league_id = NULL,
                          team_id = NULL,
                          skip_esports = NULL,
                          page = NULL) {
  base_url <- "https://api.b365api.com/v3/events/ended"
  api_key <- Sys.getenv("BETSAPI_KEY")

  # Convert sport name to sport ID
  sport_id <- sport_id_mapping[tolower(sport_name)]
  if (is.null(sport_id)) {
    stop("Invalid sport name provided")
  }

  # Convert country name to country code
  country_code <- if (!is.null(country_name)) {
    country_code_mapping[tolower(country_name)]
  } else {
    NULL
  }
  if (!is.null(country_name) && is.null(country_code)) {
    stop("Invalid country name provided")
  }

  params <- list(
    sport_id = sport_id,
    token = api_key,
    cc = country_code,
    day = date,
    league_id = league_id,
    team_id = team_id,
    skip_esports = skip_esports,
    page = page
  )

  query_string <- paste(unlist(lapply(names(params), function(key) {
    if (!is.null(params[[key]])) {
      return(paste0(key, "=", params[[key]]))
    }
  })), collapse = "&")

  full_url <- paste0(base_url, "?", query_string)
  return(full_url)
}
