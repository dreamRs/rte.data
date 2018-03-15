
#' @title Retrieve consumption data
#'
#' @description Data on French electricity consumption in real time.
#'  Forecasts are also available, they include electricity losses
#'  on the grid but do not include pumping consumption of waterworks.
#'
#' @param resource Which ressource to use between \code{short_term}
#'  (short-term consumption forecasts), \code{weekly_forecast}
#'  (weekly consumption forecasts) and \code{annual_forecast}
#'  (annual consumption forecasts).
#' @param type Forecast type of consumption, one or several
#' between 'REALISED', 'ID', 'D-1', 'D-2'.
#' @param start_date Optional, starting date to filter results,
#' if used, \code{end_date} must be set as well.
#' @param end_date Optional, ending date to filter results.
#' @param token Token obtained with \code{\link{get_token}}.
#' @param raw Return output from \code{fromJSON}.
#'
#' @return a \code{data.table}.
#' @export
#'
#' @importFrom crul HttpClient proxy
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'
#' # First you need a token
#' id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' token <- get_token(
#'   key = list(id_client = id_client, id_secret = id_secret)
#' )
#'
#' # Then you can retrieve consumption data
#' consumption <- get_consumption("short_term", token = token)
#'
#' }
get_consumption <- function(resource = c("short_term", "weekly_forecast", "annual_forecast"),
                            type = c("REALISED", "ID", "D-1", "D-2"),
                            start_date = NULL, end_date = NULL, token = NULL, raw = FALSE) {

  resource <- match.arg(resource)
  type <- match.arg(type, several.ok = TRUE)
  get_open_api(
    api = "consumption",
    resource = resource,
    start_date = start_date,
    end_date = end_date,
    q = list(type = paste(type, collapse = ",")),
    token = token,
    raw = raw
  )
}




#' @title Retrieve actual generation data
#'
#' @description Production data aggregated by sector and group
#'  (in MW) on an intraday basis, corresponding to net
#'  production injected into the network.
#'
#' @param resource Which ressource to use between \code{actual_generations_per_production_type}
#'  (production data aggregated by branch), \code{actual_generations_per_unit}
#'  (production data realised by group), \code{water_reserves}
#'  (hydraulic stock data) and \code{generation_mix_15min_time_scale}
#'  (production data realized from the global energy mix).
#' @param start_date Optional, starting date to filter results,
#' if used, \code{end_date} must be set as well.
#' @param end_date Optional, ending date to filter results.
#' @param token Token obtained with \code{\link{get_token}}.
#' @param raw Return output from \code{fromJSON}.
#'
#' @return a \code{data.table}.
#' @export
#'
#' @importFrom crul HttpClient proxy
#' @importFrom jsonlite fromJSON
#'
#' @examples
#' \dontrun{
#'
#' # First you need a token
#' id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' token <- get_token(
#'   key = list(id_client = id_client, id_secret = id_secret)
#' )
#'
#' # Then you can retrieve consumption data
#' per_prod_type <- get_actual_generation(
#'   "actual_generations_per_production_type", token = token
#' )
#'
#' }
get_actual_generation <- function(resource = c("actual_generations_per_production_type", "actual_generations_per_unit",
                                               "water_reserves", "generation_mix_15min_time_scale"),
                                  start_date = NULL, end_date = NULL, token = NULL, raw = FALSE) {

  resource <- match.arg(resource)
  get_open_api(
    api = "actual_generation",
    resource = resource,
    start_date = start_date,
    end_date = end_date,
    q = list(),
    token = token,
    raw = raw
  )
}
