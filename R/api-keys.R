
#' Set a key for a specific API
#'
#' @param api Name of the API.
#' @param key a base64 encoded string or a list containing
#' 'client_id' and 'client_secret'. To get those credentials
#' you need an account on \url{https://data.rte-france.com} and
#' to create an application for the concerned API.
#'
#' @export
#'
#' @importFrom jsonlite base64_enc
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#'
#' set_key("consumption", "BASE64 KEY")
#'
#' # or
#' set_key("consumption", list(id_client = "XXX", id_secret = "XXX"))
#'
#' }
set_key <- function(api, key) {
  if (is.list(key))
    key <- jsonlite::base64_enc(charToRaw(paste(key$id_client, key$id_secret, sep = ":")))
  api_key <- glue::glue(
    "RTE_DATA_{api}={key}",
    api = toupper(api),
    key = key
  )
  api_key_l <- list(key)
  names(api_key_l) <- glue::glue("rte.data.api.{api}", api = tolower(api))
  cat(
    api_key, "\n",
    file = file.path(path.expand("~/"), ".Renviron"),
    append = TRUE
  )
  options(api_key_l)
  invisible()
}


get_key <- function(api) {
  key <- Sys.getenv(glue::glue("RTE_DATA_{api}", api = toupper(api)))
  if (key == "")
    key <- getOption(glue::glue("rte.data.api.{api}", api = tolower(api)))
  return(key)
}

