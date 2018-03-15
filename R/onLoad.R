# set global options
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rte.data <- list(
    rte.data.url.base = "https://digital.iservices.rte-france.com",
    rte.data.url.auth = "{url.base}/token/oauth/",
    rte.data.url.open_api = "{url.base}/open_api"
  )
  toset <- !(names(op.rte.data) %in% names(op))
  if(any(toset)) options(op.rte.data[toset])

  invisible()
}
