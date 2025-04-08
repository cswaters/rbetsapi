
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbetsapi

<!-- badges: start -->

<!-- badges: end -->

rbetsapi is an R package to interact with the
[BetsAPI](https://betsapi.com/) sports data and odds API.

## Installation

You can install the development version of rbetsapi from GitHub with:

``` r
# install.packages("pak")
pak::pak("cswaters/rbetsapi")
```

## Authentication

This package requires a BetsAPI API key. You can set it as an
environment variable:

``` r
Sys.setenv(BETSAPI_KEY = "your_api_key_here")
```

## Features

rbetsapi offers a clean, consistent interface to the BetsAPI with
functions for:

- Fetching historical sports events and results
- Retrieving upcoming games
- Getting betting odds (opening/closing and historical movements)
- Preparing data for sports modeling

## Examples

### Fetch historical game results

``` r
library(rbetsapi)

# Get past NBA games
results <- get_all_games(
  sport_name = "basketball",
  country_name = "usa",
  league_id = "244",
  max_pages = 5
)

head(results)
```

### Get upcoming events

``` r
# Get upcoming games with a more efficient call
upcoming_games <- get_all_upcoming_games(
  sport_name = "basketball",
  country_name = "usa",
  league_id = "244"
)

head(upcoming_games)
```

### Get betting odds

``` r
# Create an odds fetcher
odds_fetcher <- create_event_odds_fetcher()

# Fetch odds for a single event from Pinnacle
event_odds <- odds_fetcher$fetch_event_data("123456", "PinnacleSports")

# Fetch odds for multiple events
event_ids <- c("123456", "789012")
combined_odds <- odds_fetcher$fetch_and_combine_odds(event_ids)

# Get historical odds movement
odds_history <- query_event_odds("123456")

# Extract most recent odds
recent_odds <- get_most_recent_odds(odds_history, "123456")
```

### Prepare data for modeling

``` r
# Download and prepare data for NBA modeling
nba_data <- download_modeling_data(
  country = "usa",
  league_id = "244",
  sport_name = "basketball"
)

# Access the components
head(nba_data$df)           # Prepared data frame
head(nba_data$upcoming_games) # Upcoming games
```

## Package Structure

The package is organized into several logical components:

- API access utilities and constants
- Event retrieval functions
- Odds data retrieval functions
- Game details extraction
- Data preparation tools

## License

This package is released under the MIT License.
