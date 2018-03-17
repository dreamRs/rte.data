
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
#' @param object a \code{rte.data.table} obtained from the API.
#' @param ... other arguments passed to specific methods
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom ggplot2 autoplot ggplot aes_ geom_line labs theme_minimal
#'  scale_color_brewer scale_x_datetime geom_ribbon scale_fill_manual
#'  scale_color_manual waiver
#' @importFrom data.table copy data.table
#'
#' @examples
#' # todo
autoplot.rte.data.table <- function(object, ...) {
  api_name <- attr(object, "api.name")

  if (is.null(api_name))
    stop("No autoplot defined for this API !", call. = FALSE)

  if (api_name == "consumption") {

    dd <- difftime(max(object$start_date), min(object$start_date), units = "days")
    dd <- as.numeric(dd)
    if (dd < 1.1) {
      date_labels <- "%H:%M"
    } else {
      date_labels <- waiver()
    }
    ggplot(data = object) +
      geom_line(aes_(x = ~start_date, y = ~value, color = ~type)) +
      labs(
        title = "French electricity consumption : forecast vs realised",
        subtitle = format(attr(object, "api.time"), "data for %Y-%m-%d, forecast at %Hh%M"),
        caption = "https://data.rte-france.com",
        x = NULL, y = "Electricity consumption (MW)"
      ) +
      scale_x_datetime(date_labels = date_labels) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal()

  } else if (api_name == "physical_flow") {

    object <- copy(object)
    object[, type := ifelse(receiver_country_name == "France", "Imports", "Exports")]
    dd <- difftime(max(object$start_date), min(object$start_date), units = "days")
    dd <- as.numeric(dd)
    if (dd > 7) {
      object <- object[, list(value = sum(value)), by = list(start_date = as.Date(format(start_date)), type)]
    } else {
      object <- object[, list(value = sum(value)), by = list(start_date, type)]
    }

    ggplot(data = object) +
      geom_line(aes_(x = ~as.POSIXct(start_date), y = ~value, color = ~type), size = 1) +
      geom_ribbon(aes_(x = ~start_date, ymin = ~ymin, ymax = ~ymax, fill = ~fill, group = ~fill),
                  alpha = 0.3, data = function(data) {
        res <- data[, list(ymin = min(value), ymax = max(value), diff = diff(value)), by = start_date]
        res <- res[, fill := ifelse(diff > 0, "in favour", "against")]
        res <- res[, start_date := as.POSIXct(format(start_date))]
        data <- data[order(start_date)]
        x1 <- data$value[data$type == "Exports"]
        x2 <- data$value[data$type == "Imports"]
        above <- x1 > x2
        intp <- which(diff(above) != 0)
        x1s <- x1[intp + 1] - x1[intp]
        x2s <- x2[intp + 1] - x2[intp]
        xp <- intp + ((x2[intp] - x1[intp]) / (x1s - x2s))
        yp <- x1[intp] + (x1s * (xp - intp))
        if (length(xp) > 0) {
          addin <- data.table(
            start_date = rep(as.POSIXct(format(min(data$start_date))) + (xp - 1) * 60 * 60 * 24, each = 2),
            ymin = rep(yp, each = 2), ymax = rep(yp, each = 2),
            fill = rep(c("in favour", "against"), times = 2),
            diff = rep(0, each = 2)
          )
          res <- rbind(res, addin)
        }
        res[order(start_date, fill)]
      }) +
      labs(
        title = "Electricity balance for France",
        # subtitle = format(attr(object, "api.time"), "data for %Y-%m-%d, forecast at %Hh%M"),
        caption = "https://data.rte-france.com",
        x = NULL, y = "Physical flows (MW)"
      ) +
      scale_color_manual(values = c("firebrick", "goldenrod3"), name = "Flow") +
      scale_fill_manual(values = c("firebrick", "goldenrod3"), name = "Balance") +
      theme_minimal()

  } else {
    stop("No autoplot defined for this API !", call. = FALSE)
  }
}





#' rte.data exported operators and S3 methods
#'
#' The following functions are imported and then re-exported
#' from the rte.package package to avoid loading them.
#'
# @noRd
#' @importFrom ggplot2 autoplot
#' @name autoplot
#' @export
NULL


