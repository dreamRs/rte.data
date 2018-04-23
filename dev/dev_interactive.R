

#  ------------------------------------------------------------------------
#
# Title : Dev interactive viz
#    By : VP
#  Date : 2018-04-11
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )
library( data.table )
library( billboarder )




# Consumption -------------------------------------------------------------

short_term <- get_consumption("short_term", type = c("REALISED", "D-1"))
short_term

autoplot(short_term)

range_date <- function(x) {
  x <- unique(format(x, format = "%Y-%m-%d"))
  x <- sort(x)
  if (length(x) == 1) {
    return(x)
  } else {
    return(paste(range(x), collapse = " - "))
  }
}

billboarder(data = short_term) %>%
  bb_linechart(mapping = bbaes(x = start_date, y = value, group = type)) %>%
  bb_x_axis(tick = list(format = "%H:%M", fit = FALSE)) %>%
  bb_y_grid(show = TRUE) %>%
  bb_x_grid(show = TRUE) %>%
  bb_legend(position = "right") %>%
  bb_labs(
    title = "French electricity consumption : forecast vs realised",
    caption = "https://data.rte-france.com",
    y = "Electricity consumption (MW)"
  ) %>%
  bb_x_axis(label = list(text = range_date(short_term$start_date), position = "outer-center"))





# Physical flows ----------------------------------------------------------

# Balance ----

balance <- get_physical_flows(start_date = "2018-02-01", end_date = "2018-03-15")
autoplot(balance)

balance
balance[, type := ifelse(receiver_country_name == "France", "Imports", "Exports")]
balance <- balance[, list(value = sum(value)), by = list(start_date = as.Date(format(start_date)), type)]

billboarder(data = balance) %>%
  bb_linechart(mapping = bbaes(x = start_date, y = value, group = type)) %>%
  bb_x_axis(tick = list(fit = FALSE)) %>%
  bb_y_grid(show = TRUE) %>%
  bb_x_grid(show = TRUE) %>%
  bb_colors_manual(
    Imports = "#cd9b1d",
    Exports = "#b22222"
  ) %>%
  bb_labs(
    title = "Electricity balance for France",
    caption = "https://data.rte-france.com",
    y = "Physical flows (MW)"
  )


# area range
data <- copy(balance)
data[, type := ifelse(receiver_country_name == "France", "Imports", "Exports")]
dd <- difftime(max(data$start_date), min(data$start_date), units = "days")
dd <- as.numeric(dd)
if (dd > 7) {
  data <- data[, list(value = sum(value)), by = list(start_date = as.Date(format(start_date)), type)]
  offset <- 24
} else {
  data <- data[, list(value = sum(value)), by = list(start_date, type)]
  offset <- 1
}
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
res <- res[order(start_date, fill)]

billboarder(data = res) %>%
  bb_linechart(
    mapping = bbaes(x = start_date, y = ymin, ymin = ymin, ymax = ymax, group = fill),
    type = "area-line-range"
  ) %>%
  bb_colors_manual(
    "in favour" = "#cd9b1d",
    "against" = "#b22222",
    opacity = 0.4
  )
#
# str(test$x$bb_opts$data, max.level = 2)
# test




# Exchange ----

exchange <- get_physical_flows(start_date = "2018-02-01", end_date = "2018-03-15")
autoplot(exchange, by_country = TRUE)

exchange <- exchange[, list(value = sum(value)), by = list(sender_country_name, receiver_country_name)]
exchange[, flow := ifelse(sender_country_name == "France", "Exports", "Imports")]
exchange[flow == "Exports", value := value * -1L]
exchange[, country := ifelse(flow == "Imports", sender_country_name, receiver_country_name)]
exchange[, country := factor(x = country, levels = rev(sort(unique(country))))]
exchange <- exchange[order(country)]
exchange

range_val <- c(-max(abs(exchange$value)), max(abs(exchange$value)))

billboarder(data = exchange) %>%
  bb_barchart(
    mapping = bbaes(x = country, y = value, group = flow),
    stacked = TRUE, rotated = TRUE
  ) %>%
  bb_y_axis(
    tick = list(values = scales::pretty_breaks(4)(range_val)),
    min = range_val[1], max = range_val[2]
  ) %>%
  bb_y_grid(show = TRUE) %>%
  bb_x_grid(show = TRUE) %>%
  bb_legend(position = "right") %>%
  bb_y_grid(
    lines = list(
      list(value = 0, class = "zero")
    )
  ) %>%
  bb_add_style(y_grid = list(
    zero = list(line = "stroke: black; stroke-dasharray: 12 4; stroke-width: 2px;")
  )) %>%
  bb_colors_manual(
    Imports = "#cd9b1d",
    Exports = "#b22222"
  ) %>%
  bb_labs(
    title = "Exchanges with neighbouring countries",
    caption = "https://data.rte-france.com",
    y = "Physical flows (MW)"
  )
