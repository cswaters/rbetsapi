
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rbetsapi

<!-- badges: start -->

<!-- badges: end -->

rbetsapi is a package to work with the <https://betsapi.com/> Events
API.

## Installation

You can install the development version of rbetsapi like so:

``` r
pak::pak("cswaters/rbetsapi")
```

## Example

No one but me should find any value out of this package. But here’s an
example.

``` r
library(rbetsapi)
country <- "usa"
league_id <- "244"

# get past scores ----
results <- get_all_games(
  sport_name = "basketball",
  country_name = country,
  league_id = league_id,
  max_pages = 20
)

head(results)
```

Get upcoming events

``` r
upcoming <- get_upcoming_events(sport_name = "basketball",
                                country_name = country,
                                league_id = league_id)
# function to clean up response ----
upcoming_games <- get_upcoming_games(upcoming)
head(upcoming_games)
```
