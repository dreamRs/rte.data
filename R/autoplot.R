
# cat("\u00c9coutez !",
#     "\nPuisqu\u2019on allume les \u00e9toiles,",
#     "\nc\u2019est qu\u2019elles sont \u00e0",
#     "\nquelqu\u2019un n\u00e9cessaires ?",
#     "\nC\u2019est que quelqu\u2019un d\u00e9sire",
#     "\nqu\u2019elles soient ?",
#     "\n\nVladimir Ma\u00efakovski")


#' @title Autplot method
#'
#' @description Quick \code{ggplot} to explore data
#'
#' @param object A \code{rte.data.table} obtained from the API.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 autoplot ggplot aes_ geom_line labs theme_minimal
#'  scale_color_brewer scale_x_datetime
#'
#' @examples
#' # todo
autoplot.rte.data.table <- function(object) {
  api_name <- attr(object, "api.name")
  if (is.null(api_name))
    stop("No autoplot defined for this API !", call. = FALSE)
  if (api_name == "consumption") {
    ggplot(data = object) +
      geom_line(aes_(x = ~start_date, y = ~value, color = ~type)) +
      labs(
        title = "French electricity consumption : forecast vs realised",
        subtitle = format(attr(object, "api.time"), "data for %Y-%m-%d, forecast at %Hh%M"),
        caption = "https://data.rte-france.com",
        x = NULL, y = "Electricity consumption (MW)"
      ) +
      scale_x_datetime(date_labels = "%H:%M") +
      scale_color_brewer(palette = "Set1") +
      theme_minimal()
  } else {
    stop("No autoplot defined for this API !", call. = FALSE)
  }
}
