#' Extract Game Details
#'
#' This function extracts detailed information about a game from a single API response.
#'
#' @param res A list representing the API response for a single game.
#'
#' @return A named list with detailed game information, including scores by quarter, halves, 
#'   and overtime, as well as metadata like game ID, league, and team names. 
#'   If the `scores` field is empty, the function returns `NULL`.
#' @export
#' @examples
#' \dontrun{
#' game_details <- extract_game_details(api_response_list)
#' }
extract_game_details <- function(res) {
  # Skip if no scores data 
  if (is.null(res) || is.null(res$scores) || length(res$scores) == 0) {
    return(NULL)
  }

  to_numeric <- function(x) as.numeric(x)  # Helper function for conversion

  # Extract main score information
  score_s <- stringr::str_split(res$ss, "-")[[1]]
  hscore <- as.numeric(score_s[1])
  ascore <- as.numeric(score_s[2])
  
  # Get quarter scores using vectorized operations when possible
  tryCatch({
    h_q1 <- to_numeric(res$scores$`1`$home)
    h_q2 <- to_numeric(res$scores$`2`$home)
    a_q1 <- to_numeric(res$scores$`1`$away)
    a_q2 <- to_numeric(res$scores$`2`$away)
    
    # Game has overtime if we have 7 score entries
    has_overtime <- length(res$scores) == 7
    
    # Second half and total scores
    h_q3 <- to_numeric(res$scores$`4`$home)
    h_q4 <- to_numeric(res$scores$`5`$home)
    a_q3 <- to_numeric(res$scores$`4`$away)
    a_q4 <- to_numeric(res$scores$`5`$away)
    h_final <- to_numeric(res$scores$`7`$home)
    a_final <- to_numeric(res$scores$`7`$away)
    
    # Calculate based on whether game had overtime
    if (has_overtime) {
      ot <- 1
      h_1h <- to_numeric(res$scores$`3`$home)
      a_1h <- to_numeric(res$scores$`3`$away)
      h_ot <- to_numeric(res$scores$`6`$home)
      a_ot <- to_numeric(res$scores$`6`$away)
      h_2h <- h_q3 + h_q4
      a_2h <- a_q3 + a_q4
    } else {
      ot <- 0
      h_1h <- h_q1 + h_q2
      a_1h <- a_q1 + a_q2
      h_2h <- h_q3 + h_q4
      a_2h <- a_q3 + a_q4
      h_ot <- 0
      a_ot <- 0
    }
    
    # Create the result list
    list(
      game_id = res$id,
      league_id = res$league$id,
      league = res$league$name,
      round = res$round,
      date = convert_to_datetime(res$time),
      home = res$home$name,
      home_id = res$home$id,
      away = res$away$name,
      away_id = res$away$id,
      hscore = hscore,
      ascore = ascore,
      ot = ot,
      h_q1 = h_q1,
      h_q2 = h_q2,
      a_q1 = a_q1,
      a_q2 = a_q2,
      h_1h = h_1h,
      a_1h = a_1h,
      h_q3 = h_q3,
      h_q4 = h_q4,
      a_q3 = a_q3,
      a_q4 = a_q4,
      h_2h = h_2h,
      a_2h = a_2h,
      h_ot = h_ot,
      a_ot = a_ot,
      h_final = h_final,
      a_final = a_final
    )
  }, error = function(e) {
    warning(sprintf("Error extracting game details for game %s: %s", 
                    ifelse(is.null(res$id), "unknown", res$id), 
                    e$message))
    return(NULL)
  })
} 