#' @keywords internal
"_PACKAGE"

#' rbetsapi: Interface to the BetsAPI for Sports Data and Odds
#'
#' The rbetsapi package provides a streamlined interface to the BetsAPI
#' for retrieving sports events, results, and betting odds. It includes
#' functions for fetching historical game data, upcoming events, and betting
#' odds (both opening/closing lines and historical odds movement).
#'
#' @section Main Functions:
#' \itemize{
#'   \item Fetch events: \code{\link{get_events}}, \code{\link{get_all_games}}
#'   \item Fetch upcoming events: \code{\link{get_upcoming_events}}, \code{\link{get_all_upcoming_games}}
#'   \item Fetch odds: \code{\link{create_event_odds_fetcher}}, \code{\link{query_event_odds}}
#'   \item Data preparation: \code{\link{prepare_data}}, \code{\link{download_modeling_data}}
#' }
#'
#' @section Authentication:
#' This package requires a BetsAPI API key. You should set the environment variable
#' BETSAPI_KEY with your API key before using the package functions.
#'
#' @import dplyr
#' @importFrom purrr map map_dfr compact keep
#' @importFrom tibble tibble
#' @importFrom httr GET content stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom stats na.omit
#' @importFrom utils URLencode
#' @importFrom stringr str_split
#' @importFrom lubridate mdy
#'
#' @docType package
#' @name rbetsapi
NULL 