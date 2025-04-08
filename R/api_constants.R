#' API Endpoint URLs
#' 
#' Constants for BetsAPI endpoint URLs used throughout the package.
#' 
#' @keywords internal
BETSAPI_ENDPOINTS <- list(
  ODDS_SUMMARY = "https://api.b365api.com/v2/event/odds/summary",
  ODDS_HISTORY = "https://api.b365api.com/v2/event/odds",
  EVENTS_ENDED = "https://api.b365api.com/v3/events/ended",
  EVENTS_UPCOMING = "https://api.b365api.com/v3/events/upcoming"
)

#' Sport ID Mappings for BetsAPI
#'
#' A named vector mapping sport names to their corresponding numeric IDs in the BetsAPI system.
#' Used for converting human-readable sport names to the API's internal identifiers.
#'
#' @format A named vector where names are sport names and values are numeric IDs
#' @keywords internal
sport_id_mapping <- c(
  soccer = 1,
  basketball = 18,
  tennis = 13,
  volleyball = 91,
  handball = 78,
  baseball = 16,
  horse_racing = 2,
  greyhounds = 4,
  ice_hockey = 17,
  snooker = 14,
  american_football = 12,
  cricket = 3,
  futsal = 83,
  darts = 15,
  table_tennis = 92,
  badminton = 94,
  rugby_union = 8,
  rugby_league = 19,
  australian_rules = 36,
  bowls = 66,
  boxing = 9,
  gaelic_sports = 75,
  floorball = 90,
  beach_volleyball = 95,
  water_polo = 110,
  squash = 107,
  e_sports = 151,
  mma = 162,
  surfing = 148
)

#' Odds Type to Market Key Mapping for BetsAPI
#'
#' A named vector mapping descriptive odds type names to their corresponding
#' market keys used in the BetsAPI system. These keys are used to identify
#' the type of betting market in API responses and requests.
#'
#' @format A named vector where names are descriptive market types and values are market keys
#' @keywords internal
odds_type_to_market_key <- c(
  "1X2" = "1_1",
  "asian handicap" = "1_2",
  "over/under" = "1_3",
  "asian corners" = "1_4",
  "1st half asian handicap" = "1_5",
  "1st half goal line" = "1_6",
  "1st half asian corners" = "1_7",
  "half time result" = "1_8",
  "money line" = "18_1",
  "spread" = "18_2",
  "total points" = "18_3",
  "money line (half)" = "18_4",
  "spread (half)" = "18_5",
  "total points (half)" = "18_6",
  "quarter - winner (2-way)" = "18_7",
  "quarter - handicap" = "18_8",
  "quarter - total (2-way)" = "18_9",
  "match winner 2-way" = "*_1",
  "asian handicap" = "*_2",
  "over/under" = "*_3",
  "draw no bet" = "3_4"
) 