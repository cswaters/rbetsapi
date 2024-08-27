#' Extract Game Details
#'
#' This function extracts detailed information about a game from a single API response.
#'
#' @param res A list representing the API response for a single game.
#'
#' @return A named list with detailed game information, including scores by quarter, halves, and overtime, as well as metadata like game ID, league, and team names. If the `scores` field is empty, the function returns `NULL`.
#' @export
#' @examples
#' \dontrun{
#' game_details <- extract_game_details(api_response_list)
#' }
#'
extract_game_details <- function(res) {
  if (length(res$scores) == 0) {
    return(NULL)
  }

  to_numeric <- function(x) as.numeric(x)  # Helper function for conversion

  h_q1 <- res$scores$`1`$home |> to_numeric()
  h_q2 <- res$scores$`2`$home |> to_numeric()
  a_q1 <- res$scores$`1`$away |> to_numeric()
  a_q2 <- res$scores$`2`$away |> to_numeric()

  if (length(res$scores) == 7) {
    ot <- 1
    h_1h <- res$scores$`3`$home |> to_numeric()
    a_1h <- res$scores$`3`$away |> to_numeric()
    h_q3 <- res$scores$`4`$home |> to_numeric()
    h_q4 <- res$scores$`5`$home |> to_numeric()
    a_q3 <- res$scores$`4`$away |> to_numeric()
    a_q4 <- res$scores$`5`$away |> to_numeric()
    h_ot <- res$scores$`6`$home |> to_numeric()
    a_ot <- res$scores$`6`$away |> to_numeric()
    h_final <- res$scores$`7`$home |> to_numeric()
    a_final <- res$scores$`7`$away |> to_numeric()
    h_2h <- h_q3 + h_q4
    a_2h <- a_q3 + a_q4
  } else {
    ot <- 0
    h_1h <- h_q1 + h_q2
    a_1h <- a_q1 + a_q2
    h_q3 <- res$scores$`4`$home |> to_numeric()
    h_q4 <- res$scores$`5`$home |> to_numeric()
    a_q3 <- res$scores$`4`$away |> to_numeric()
    a_q4 <- res$scores$`5`$away |> to_numeric()
    h_final <- res$scores$`7`$home |> to_numeric()
    a_final <- res$scores$`7`$away |> to_numeric()
    h_2h <- h_q3 + h_q4
    a_2h <- a_q3 + a_q4
    h_ot <- 0
    a_ot <- 0
  }

  score_s <- stringr::str_split(res$ss, "-")[[1]]
  hscore <- as.numeric(score_s[1])
  ascore <- as.numeric(score_s[2])

  list(
    game_id = res$id,
    league_id = res$league$id,
    league = res$league$name,
    round = res$round,
    date = as.POSIXct(as.numeric(res$time), origin = "1970-01-01", tz = "UTC"),
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
}
