
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
#' @param start_date Optional, starting date to filter results.
#' @param end_date Optional, ending date to filter results.
#' @param token Token obtained with \code{\link{get_token}}.
#' @param raw Return output from \code{fromJSON}.
#'
#' @return a \code{data.table} or a \code{list} if \code{raw = TRUE}.
#' @export
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
get_consumption <- function(resource = c("short_term", "weekly_forecasts", "annual_forecasts"),
                            type = c("REALISED", "ID", "D-1", "D-2"),
                            start_date = NULL, end_date = NULL, token = NULL, raw = FALSE) {

  resource <- match.arg(resource)
  type <- match.arg(type, several.ok = TRUE)
  get_open_api(
    api = "consumption",
    resource = resource,
    start_date = start_date,
    end_date = end_date,
    q = list(type = I(paste(type, collapse = ","))),
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
#' @param start_date Optional, starting date to filter results.
#' @param end_date Optional, ending date to filter results.
#' @param token Token obtained with \code{\link{get_token}}.
#' @param raw Return output from \code{fromJSON}.
#'
#' @return a \code{data.table} or a \code{list} if \code{raw = TRUE}.
#' @export
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




#' @title Retrieve physical flows data
#'
#' @description Data about physical cross-border schedules detailing
#' electricity flows actually transiting across the interconnection
#' lines directly linking countries.
#'
#' @param code_eic Optional, EIC code of the desired country.
#' @param start_date Optional, starting date to filter results.
#' @param end_date Optional, ending date to filter results.
#' @param token Token obtained with \code{\link{get_token}}.
#' @param raw Return output from \code{fromJSON}.
#'
#' @return a \code{data.table} or a \code{list} if \code{raw = TRUE}.
#' @export
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
#' # Then you can retrieve flow data
#' phys_flow <- get_physical_flows(token = token)
#'
#' }
get_physical_flows <- function(code_eic = NULL, start_date = NULL, end_date = NULL, token = NULL, raw = FALSE) {
  code_eic <- get_code_eic(code_eic)
  get_open_api(
    api = "physical_flow",
    resource = "physical_flows",
    start_date = start_date,
    end_date = end_date,
    q = list(country_eic_code = code_eic),
    token = token,
    raw = raw
  )
}

get_code_eic <- function(x) {
  if (is.null(x))
    return(NULL)
  eic <- c("10YFR-RTE------C",
           "10YCB-GERMANY--8",
           "10YGB----------A",
           "10YBE----------2",
           "10YES-REE------0",
           "10YIT-GRTN-----B",
           "10YCH-SWISSGRIDZ",
           "10YDOM-REGION-1V")
  code <- c("FR", "DE", "GB", "BE", "ES", "IT", "CH", "CWE")
  if (x %in% code) {
    eic[which(code == x)]
  } else if (x %in% eic) {
    eic
  } else {
    stop("Unknown EIC code.")
  }
}







