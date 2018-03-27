
#' @title Retrieve data from an open API
#'
#' @description Several APIs are available, create an application
#'  and attach APIs to it, this function allows
#'  to retrieve data from any API, you'll need
#'  its name and the name of the
#'  resource (informations available on the page of the
#'  API on https://data.rte-france.com).
#'
#' @param api Name of main API.
#' @param resource Resource to use in the API.
#' @param start_date Optional, starting date to filter results,
#' if used, \code{end_date} must be set as well.
#' @param end_date Optional, ending date to filter results.
#' @param q Additional parameters to query the API.
#' @param token Token obtained with \code{\link{get_token}}.
#' @param raw Return output from \code{fromJSON}.
#'
#' @return a \code{data.table}.
#' @export
#'
#' @importFrom crul HttpClient proxy
#' @importFrom jsonlite fromJSON
#' @importFrom data.table setattr
#'
#' @examples
#' \dontrun{
#'
#' set_key(
#'   api = "physical_flow",
#'   key = "BASE64_KEY"
#' )
#'
#' # Get physical flows between cross border country
#' physfl <- get_open_api(api = "physical_flow", resource = "physical_flows")
#'
#' }
get_open_api <- function(api, resource, start_date = NULL, end_date = NULL, q = list(), token = NULL, raw = FALSE) {

  if (is.null(token)) {
    key <- get_key(api)
    if (is.null(key)) {
      stop(
        "Can't retrieve a key for this API, provide a token explicitly or use `set_key`.",
        call. = FALSE
      )
    }
    token <- get_token(key)
  }

  if (!is.null(start_date)) {
    if (is.null(end_date))
      end_date <- Sys.Date()
    start_date <- as.POSIXct(format(start_date))
    start_date <- format_datetime(start_date)
  }
  if (!is.null(end_date)) {
    end_date <- as.POSIXct(format(end_date))
    end_date <- format_datetime(end_date)
  }
  q_ <- list(
    start_date = start_date, end_date = end_date
  )
  q <- c(q_, q)
  q <- dropNulls(q)

  url_api <- paste(get_url("url.open_api"), api, "v1", resource, sep = "/")
  cli <- crul::HttpClient$new(
    url = url_api,
    headers = list(
      Host = "digital.iservices.rte-france.com",
      Authorization = paste(token$token_type, token$access_token)
    )
  )
  proxy <- get_proxy_info()
  if (!is.null(proxy$user) & !is.null(proxy$proxy_pwd)) {
    cli$proxies <- crul::proxy(
      url = proxy$url,
      user = proxy$user, pwd = proxy$proxy_pwd
    )
  }
  api_time <- Sys.time()
  res <- cli$get(query = q)
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  json <- jsonlite::fromJSON(txt)
  if (raw)
    return(json)

  data <- process_data(json)
  setattr(data, "class", c(class(data), "rte.data.table"))
  setattr(data, "api.name", api)
  setattr(data, "api.resource", resource)
  setattr(data, "api.time", api_time)
  return(data)
}
