
# manipulate dates
parse_datetime <- function(x) {
  x <- gsub(pattern = "(\\d{2}):(\\d{2})$", replacement = "\\1\\2", x = x)
  as.POSIXct(x, format = "%FT%X%z")
}
format_datetime <- function(x) {
  x <- format(x, format = "%FT%X%z")
  gsub(pattern = "(\\d{2})(\\d{2})$", replacement = "\\1:\\2", x = x)
}


proxy_error <- function(url) {
  sprintf("Unable to access %s API, check yout internet connection or set proxy infos.", url)
}


# retrieve url from options
#' @importFrom glue glue
get_url <- function(url) {
  op <- options()
  op.rte.data <- op[grepl(pattern = "rte.data", x = names(op))]
  names(op.rte.data) <- gsub(pattern = "rte\\.data\\.", replacement = "", x = names(op.rte.data))
  if (!url %in% names(op.rte.data))
    stop("Invalid url")
  glue::glue(op.rte.data[[url]], .envir = as.environment(op.rte.data))
}



dropNulls <- function (x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}


