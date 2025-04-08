#' Reshape Game Scores Data
#'
#' This function takes a data frame of game scores and reshapes it to have both
#' home and away perspectives in separate rows.
#'
#' @param scores A data frame. Contains game results with home and away team information.
#'
#' @return A data frame with reshaped scores, doubling the number of rows to show
#'   both perspectives of each game.
#' @keywords internal
reshape_scores <- function(scores) {
  # Return empty data frame if input is empty
  if (is.null(scores) || nrow(scores) == 0) {
    warning("Empty scores data provided to reshape_scores")
    return(data.frame())
  }
  
  # Create home perspective rows
  home_perspective <- scores |>
    dplyr::rename(
      team = home,
      opp = away,
      team_score = hscore,
      opp_score = ascore
    ) |>
    dplyr::mutate(
      is_home = 1,
      id = dplyr::row_number()
    )
  
  # Create away perspective rows
  away_perspective <- scores |>
    dplyr::rename(
      team = away,
      opp = home,
      team_score = ascore,
      opp_score = hscore
    ) |>
    dplyr::mutate(
      is_home = 0,
      id = dplyr::row_number()
    )
  
  # Combine both perspectives
  dplyr::bind_rows(home_perspective, away_perspective)
}

#' Prepare Raw Game Data for Analysis
#'
#' This function takes raw game data and prepares it for statistical analysis
#' by reshaping it and adding useful game sequence information.
#'
#' @param scores A data frame. Contains game results with home and away team information.
#'
#' @return A data frame with prepared data, including team game numbers and opponent data.
#' @export
prepare_data <- function(scores) {
  # Return empty data frame if input is empty
  if (is.null(scores) || nrow(scores) == 0) {
    warning("Empty scores data provided to prepare_data")
    return(data.frame())
  }
  
  # Reshape scores to get both perspectives
  df <- reshape_scores(scores)

  # Convert date and add team-specific game numbers
  df <- df |>
    dplyr::mutate(
      # Make sure date is in date format
      date = if (is.character(date)) lubridate::mdy(date) else date
    ) |>
    dplyr::arrange(date) |>
    dplyr::group_by(team) |>
    dplyr::mutate(
      # Use reverse row number as the team's game number
      team_game_num = rev(dplyr::row_number()) - 1
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct()

  # Join with opponent information
  opp_df <- df |>
    dplyr::select(id, date, team, opp, team_game_num) |>
    dplyr::rename(
      opp_x = team,
      team = opp,
      opp_game_num = team_game_num
    )
  
  # Join and arrange the final data
  dplyr::left_join(
    df,
    opp_df,
    by = c("id", "date", "team")
  ) |>
    dplyr::rename(opp = opp_x) |>
    dplyr::arrange(date, id, is_home)
}

#' Clean Game Results 
#'
#' This function filters and formats game results for a specific time period.
#'
#' @param df A data frame. Contains raw game results.
#' @param date_cutoff Character. Date string in format "YYYY-MM-DD" to filter games after. Defaults to "2023-01-01".
#'
#' @return A data frame with cleaned and filtered results.
#' @keywords internal
clean_results <- function(df, date_cutoff = "2023-01-01") {
  if (is.null(df) || nrow(df) == 0) {
    warning("Empty data frame provided to clean_results")
    return(data.frame())
  }
  
  # Convert date cutoff to proper date object
  date_cutoff_obj <- as.Date(date_cutoff)
  
  df |>
    # Make sure date is a date object for comparison
    dplyr::mutate(
      date_obj = if (is.character(date)) as.Date(date) else as.Date(date)
    ) |>
    # Filter by date cutoff
    dplyr::filter(date_obj > date_cutoff_obj) |>
    # Format date for output
    dplyr::mutate(date = format(date, format = "%m/%d/%Y")) |>
    # Select only needed columns
    dplyr::select(date, game_id, home, away, hscore, ascore, ot)
}

#' Flip US Event Data to Standardize Format
#'
#' This function renames columns and flips home status for US events to standardize data format.
#'
#' @param df A data frame. Contains game event data.
#'
#' @return A data frame with standardized column names and flipped home status.
#' @keywords internal
flip_us_events <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    warning("Empty data frame provided to flip_us_events")
    return(data.frame())
  }
  
  df |>
    # Rename columns to standard format
    purrr::set_names(
      c(
        "date",
        "game_id",
        "opp",
        "team",
        "opp_score",
        "team_score",
        "is_home",
        "id",
        "opp_game_num",
        "team_game_num"
      )
    ) |>
    # Flip home status
    dplyr::mutate(is_home = !is_home)
}

#' Flip US Upcoming Games Format
#'
#' This function standardizes the column names for upcoming US games data.
#'
#' @param df A data frame. Contains upcoming games data.
#'
#' @return A data frame with standardized column names.
#' @keywords internal
flip_us_upcoming <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    warning("Empty data frame provided to flip_us_upcoming")
    return(data.frame())
  }
  
  purrr::set_names(df, c("game_id", "away", "home", "date"))
}

#' Download and Prepare Modeling Data
#'
#' This function downloads and prepares data for statistical modeling by
#' fetching both completed games and upcoming games for a specific sport and league.
#'
#' @param country Character. The country name.
#' @param league_id Integer. The league ID.
#' @param max_pages Integer. Maximum number of pages to fetch. Defaults to 20.
#' @param sport_name Character. Name of the sport. Defaults to "basketball".
#' @param date_cutoff Character. Date string in format "YYYY-MM-DD" to filter games after. Defaults to "2022-01-01".
#'
#' @return A list containing:
#'   \itemize{
#'     \item raw: Raw results data
#'     \item df: Prepared data frame with game results and statistics
#'     \item upcoming_games: Data frame of upcoming games
#'   }
#' @export
#' @examples
#' \dontrun{
#' nba_data <- download_modeling_data("USA", 12345, sport_name = "basketball")
#' }
download_modeling_data <- function(country,
                                   league_id,
                                   max_pages = 20,
                                   sport_name = "basketball",
                                   date_cutoff = "2022-01-01") {
  
  # Display initial message
  message(sprintf("Downloading modeling data for %s, league_id: %s", country, league_id))
  
  # Fetch historical results
  message("Fetching historical game results...")
  results <- get_all_games(
    sport_name = sport_name,
    country_name = country,
    league_id = league_id,
    max_pages = max_pages
  )
  
  # Check if we got any results
  if (nrow(results) == 0) {
    warning("No historical game results found")
  } else {
    message(sprintf("Retrieved %d historical games", nrow(results)))
  }
  
  # Fetch upcoming events
  message("Fetching upcoming events...")
  upcoming_games <- get_all_upcoming_games(
    sport_name = sport_name,
    country_name = country,
    league_id = league_id
  )
  
  # Process the results data
  message("Processing historical game data...")
  cleaned_results <- clean_results(results, date_cutoff = date_cutoff)
  df <- prepare_data(cleaned_results)
  
  # Add margin of victory and total points
  if (nrow(df) > 0) {
    df <- df |>
      dplyr::mutate(
        mov = team_score - opp_score, 
        total = team_score + opp_score
      )
    message(sprintf("Processed %d historical games", nrow(df) / 2))  # Divide by 2 since we have home/away perspectives
  } else {
    warning("No games remain after filtering")
  }

  # Return the complete dataset
  list(
    raw = results,
    df = df,
    upcoming_games = upcoming_games
  )
} 