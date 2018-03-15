#' Get proxy informations
#'
#' @param user Optionnal, username (NNI) for proxy.
#' @param password Optionnal, password for proxy.
#' @param url Optionnal, proxy url.
#'
#' @note You can set \code{PROXY_USR}, \code{PROXY_PWD} and \code{PROXY_URL}
#'  environment variable in your Renviron to automatically use this informations
#'  when you use other functions in this package. See \code{\link{set_proxy_info}}.
#'
#' @return a list with username, password and url for proxy
#' @export
#'
#' @importFrom curl ie_get_proxy_for_url
#'
#' @examples
#' \dontrun{
#'
#' # If you have set your proxy informations
#' # in you Renviron
#' # the list should contain them
#' get_proxy_info()
#'
#' }
get_proxy_info <- function(user = NULL, password = NULL, url = NULL) {
  res <- list()
  if (!is.null(user)) {
    res$usr <- user
  } else {
    res$usr <- Sys.getenv("PROXY_USR")
    if (res$usr == "")
      res$usr <- NULL
  }
  if (!is.null(password)) {
    res$pwd <- password
  } else {
    res$pwd <- Sys.getenv("PROXY_PWD")
    if (res$pwd == "")
      res$pwd <- NULL
  }
  if (!is.null(url)) {
    res$url <- url
  } else {
    res$url <- Sys.getenv("PROXY_URL")
    if (res$url == "")
      res$url <- curl::ie_get_proxy_for_url("https://httpbin.org/get")
  }
  return(res)
}





#' Set proxy credentials
#'
#'
#' @param user Username for proxy.
#' @param password Password for proxy.
#' @param url Proxy url, if not provided, n attempt is made to obtain the url.
#'
#' @note You'll need to restart your R session for change to be effective.
#'
#' @export
#'
#' @importFrom curl ie_get_proxy_for_url
#'
#' @examples
#' \dontrun{
#'
#' set_proxy_info("MYNNI", "MY_PASSWORD")
#' # restart your R session
#' # info should appear in the list
#' # returned by :
#' get_proxy_info()
#'
#' }
set_proxy_info <- function(user, password, url = NULL) {
  if (is.null(url))
    url <- curl::ie_get_proxy_for_url("https://httpbin.org/get")
  cat(
    paste(paste0("USR_PROXY=", user),
          paste0("PWD_PROXY=", password),
          paste0("PWD_URL=", url), sep = "\n"),
    file = file.path(path.expand("~/"), ".Renviron"),
    append = TRUE
  )
  message("Proxy info saved, please restart your R session")
}
