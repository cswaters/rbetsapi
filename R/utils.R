#' Null-coalescing operator
#' 
#' A helper operator that returns the right-hand side value if the left-hand side is NULL.
#' 
#' @param x Left-hand side value to check for NULL
#' @param y Right-hand side value to return if x is NULL
#' 
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Convert timestamp to datetime
#' 
#' A helper function to convert Unix timestamps to POSIXct datetime objects.
#' 
#' @param timestamp Numeric. Unix timestamp.
#' 
#' @return POSIXct datetime object or NULL if timestamp is NULL
#' @keywords internal
convert_to_datetime <- function(timestamp) {
  if (is.null(timestamp)) return(NULL)
  as.POSIXct(as.numeric(timestamp), origin = "1970-01-01", tz = "UTC")
}

#' Safely extract values from nested lists
#' 
#' A helper function to extract values from nested lists with error handling.
#' 
#' @param data A list. The data structure to extract from.
#' @param key Character. The key to extract.
#' @param default Any. The default value to return if extraction fails.
#' 
#' @return The extracted value or the default value if extraction fails.
#' @keywords internal
safe_extract <- function(data, key, default = NULL) {
  if (is.null(data) || !is.list(data)) return(default)
  if (!key %in% names(data)) return(default)
  return(data[[key]])
}

#' Get API key from environment
#' 
#' @return Character. The API key.
#' @keywords internal
get_api_key <- function() {
  api_key <- Sys.getenv("BETSAPI_KEY")
  if (api_key == "") {
    stop("API key not found. Please set the BETSAPI_KEY environment variable.")
  }
  api_key
} 