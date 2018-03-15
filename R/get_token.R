#' Get a token to access RTE data API
#'
#' @param key a base64 encoded string or a list containing
#' 'client_id' and 'client_secret'. To get those credentials
#' you need an account on \url{https://data.rte-france.com} and
#' to create an application for the concerned API.
#' @param user Username for proxy if needeed.
#' @param proxy_pwd Password for proxy if needeed.
#'
#' @return a list with the access token
#' @export
#'
#' @importFrom jsonlite fromJSON
#' @importFrom curl ie_get_proxy_for_url
#' @importFrom crul HttpClient proxy
#' @importFrom base64enc base64encode
#'
#' @examples
#' \dontrun{
#'
#' # To create a token you can use id_client and id_secret
#' id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
#' token <- get_token(
#'   key = list(id_client = id_client, id_secret = id_secret)
#' )
#'
#' # or the base64 encoded key
#' key <- "TGEgZGF0YXNjaWVuY2UgYXZlYyB1biBncmFuZCBS="
#' token <- get_token(key)
#'
#' }
get_token <- function(key, user = NULL, proxy_pwd = NULL) {
  if (is.list(key))
    key <- base64encode(charToRaw(paste(key$id_client, key$id_secret, sep = ":")))
  cli <- crul::HttpClient$new(
    url = get_url("url.auth"),
    headers = list(
      Authorization = paste("Basic", key)
    )
  )
  proxy <- get_proxy_info(user, proxy_pwd)
  if (!is.null(proxy$user) & !is.null(proxy$proxy_pwd)) {
    cli$proxies <- crul::proxy(
      url = curl::ie_get_proxy_for_url("https://httpbin.org/get"),
      user = proxy$user, pwd = proxy$proxy_pwd
    )
  }
  res <- try(cli$post(), silent = TRUE)
  if ("try-error" %in% class(res))
    stop(proxy_error(getOption("rte.data.url.base")))
  res$raise_for_status()
  txt <- res$parse("UTF-8")
  json <- jsonlite::fromJSON(txt)
  json
}
