
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
#'  scale_color_manual waiver geom_area theme guide_legend element_text
#' @importFrom data.table copy data.table %chin%
#'
#' @examples
#' # todo
autoplot.rte.data.table <- function(object, ...) {
  api_name <- attr(object, "api.name")
  api_resource <- attr(object, "api.resource")

  args <- list(...)

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
      theme_minimal() + theme(plot.title = element_text(face = "bold"))

  } else if (api_name == "physical_flow") {


    if (!is.null(args$by_country) && args$by_country) {
      plot_exchange(object)
    } else {
      plot_balance(object)
    }

  } else if (api_name == "actual_generation") {

    if (api_resource == "actual_generations_per_production_type") {

      api_time <- attr(object, "api.time")

      object <- copy(object)
      object <- object[production_type != "TOTAL"]
      if (!is.null(args$by_day) && args$by_day) {
        object <- object[, list(value = mean(value)), by = list(start_date = as.Date(start_date), production_type)]
      }
      object[production_type %chin% c("HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_WATER_RESERVOIR"), production_type := "HYDRO"]
      object <- object[, list(value = sum(value)), by = list(production_type, start_date)]
      object <- object[, group := factor(
        x = production_type, levels = c("BIOMASS", "FOSSIL_GAS", "FOSSIL_HARD_COAL", "FOSSIL_OIL",
                                        "HYDRO", "NUCLEAR", "SOLAR", "WASTE",
                                        "WIND_ONSHORE", "HYDRO_PUMPED_STORAGE")
      )]
      object <- object[, group := as.numeric(group)]



      ggplot(data = object) +
        geom_area(aes_(x = ~start_date, y = ~value, fill = ~production_type, group = ~group), position = "stack") +
        labs(
          title = "French electricity generation per production type",
          subtitle = paste("Poduced on", format(api_time, format = "%Y-%m-%d %H:%M")),
          y = "Production (in MW)", x = NULL
        ) +
        scale_fill_manual(
          values = c(
            "BIOMASS" = "#166a57",
            "FOSSIL_GAS" = "#f30a0a",
            "FOSSIL_HARD_COAL" = "#ac8c35",
            "FOSSIL_OIL" = "#8356a2",
            "HYDRO_PUMPED_STORAGE" = "#114774",
            "HYDRO" = "#2772b2",
            "NUCLEAR" = "#f8ca4c",
            "SOLAR" = "#f27406",
            "WASTE" = "#61380B",
            "WIND_ONSHORE" = "#74cdb9"
          ), guide = guide_legend(
            title = "Production type", title.position = "top", title.hjust = 0,
            nrow = 2, label.position = "bottom", keywidth = 6, keyheight = 0.5
          ),
          labels = capitalize
        ) +
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(face = "bold"))

    }

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







plot_balance <- function(object) {
  object <- copy(object)
  object[, type := ifelse(receiver_country_name == "France", "Imports", "Exports")]
  dd <- difftime(max(object$start_date), min(object$start_date), units = "days")
  dd <- as.numeric(dd)
  if (dd > 7) {
    object <- object[, list(value = sum(value)), by = list(start_date = as.Date(format(start_date)), type)]
    offset <- 24
  } else {
    object <- object[, list(value = sum(value)), by = list(start_date, type)]
    offset <- 1
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
                      start_date = rep(as.POSIXct(format(min(data$start_date))) + (xp - 1) * 60 * 60 * offset, each = 2),
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
    theme_minimal() + theme(plot.title = element_text(face = "bold"))
}



#' @importFrom data.table copy :=
#' @importFrom ggplot2 ggplot geom_col aes_ geom_hline
#'  scale_fill_manual scale_y_continuous labs
#'  theme_minimal coord_flip theme element_text
plot_exchange <- function(object) {

  object <- copy(object)

  range_dat <- paste(format(range(object$start_date), format = "%Y-%m-%d %H:%M"), collapse = " to ")
  range_dat <- paste("From", range_dat)
  object <- object[, list(value = sum(value)), by = list(sender_country_name, receiver_country_name)]
  object[, flow := ifelse(sender_country_name == "France", "Exports", "Imports")]
  object[flow == "Exports", value := value * -1L]
  object[, country := ifelse(flow == "Imports", sender_country_name, receiver_country_name)]
  object[, country := factor(x = country, levels = rev(sort(unique(country))))]
  object <- object[order(country)]

  ggplot(data = object) +
    geom_col(mapping = aes_(x = ~country, y = ~value, fill = ~flow)) +
    geom_hline(yintercept = 0, lty = "longdash", size = 1) +
    scale_fill_manual(values = c("firebrick", "goldenrod3"), name = "Flow") +
    scale_y_continuous(limits = c(-max(abs(object$value)), max(abs(object$value)))) +
    labs(
      title = "Exchanges with neighbouring countries",
      subtitle = range_dat,
      caption = "https://data.rte-france.com",
      x = NULL, y = "Physical flows (MW)"
    ) +
    theme_minimal() +
    coord_flip() +
    theme(plot.title = element_text(face = "bold"))
}


