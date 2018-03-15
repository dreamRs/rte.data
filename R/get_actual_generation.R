#'
#' #' @title Retrieve actual generation data
#' #'
#' #' @description Production data aggregated by sector and group
#' #'  (in MW) on an intraday basis, corresponding to net
#' #'  production injected into the network.
#' #'
#' #' @param resource Which ressource to use between \code{actual_generations_per_production_type}
#' #'  (production data aggregated by branch), \code{actual_generations_per_unit}
#' #'  (production data realised by group), \code{water_reserves}
#' #'  (hydraulic stock data) and \code{generation_mix_15min_time_scale}
#' #'  (production data realized from the global energy mix).
#' #' @param start_date Optional, starting date to filter results,
#' #' if used, \code{end_date} must be set as well.
#' #' @param end_date Optional, ending date to filter results.
#' #' @param token Token obtained with \code{\link{get_token}}.
#' #' @param raw Return output from \code{fromJSON}.
#' #'
#' #' @return a \code{data.table}.
#' #' @export
#' #'
#' #' @importFrom crul HttpClient proxy
#' #' @importFrom jsonlite fromJSON
#' #'
#' #' @examples
#' #' \dontrun{
#' #'
#' #' # First you need a token
#' #' id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' #' id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' #' token <- get_token(
#' #'   key = list(id_client = id_client, id_secret = id_secret)
#' #' )
#' #'
#' #' # Then you can retrieve consumption data
#' #' per_prod_type <- get_actual_generation(
#' #'   "actual_generations_per_production_type", token = token
#' #' )
#' #'
#' #' }
#' get_actual_generation2 <- function(resource = c("actual_generations_per_production_type", "actual_generations_per_unit",
#'                                                "water_reserves", "generation_mix_15min_time_scale"),
#'                             start_date = NULL, end_date = NULL, token = NULL, raw = FALSE) {
#'
#'   resource <- match.arg(resource)
#'   if (is.null(token)) {
#'     key <- get_key("actual_generation")
#'     if (is.null(key)) {
#'       stop(
#'         "Can't retrieve a key for this API, provide a token explicitly or use `set_key`.",
#'         call. = FALSE
#'       )
#'     }
#'     token <- get_token(key)
#'   }
#'
#'   if (!is.null(start_date)) {
#'     if (is.null(end_date))
#'       stop("If start_date is set, end_date must be passed as well.", call. = FALSE)
#'     start_date <- as.POSIXct(start_date)
#'     start_date <- format_datetime(start_date)
#'   }
#'   if (!is.null(end_date)) {
#'     end_date <- as.POSIXct(end_date)
#'     end_date <- format_datetime(end_date)
#'   }
#'   q <- list(
#'     start_date = start_date, end_date = end_date
#'   )
#'   q <- dropNulls(q)
#'
#'   cli <- crul::HttpClient$new(
#'     url = paste(get_url("url.open_api"), "actual_generation", "v1", resource, sep = "/"),
#'     headers = list(
#'       Host = "digital.iservices.rte-france.com",
#'       Authorization = paste(token$token_type, token$access_token)
#'     )
#'   )
#'   proxy <- get_proxy_info()
#'   if (!is.null(proxy$user) & !is.null(proxy$proxy_pwd)) {
#'     cli$proxies <- crul::proxy(
#'       url = proxy$url,
#'       user = proxy$user, pwd = proxy$proxy_pwd
#'     )
#'   }
#'   res <- cli$get(query = q)
#'   res$raise_for_status()
#'   txt <- res$parse("UTF-8")
#'   json <- jsonlite::fromJSON(txt)
#'   if (raw)
#'     return(json)
#'
#'   process_data(json)
#' }
