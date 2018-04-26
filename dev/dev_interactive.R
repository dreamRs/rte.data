

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
library( leaflet )



# Consumption -------------------------------------------------------------

short_term <- get_consumption(
  resource = "short_term", type = c("REALISED", "D-1")
  , start_date = Sys.Date() - 7, end_date = Sys.Date()
)
short_term

autoplot(short_term)

range_date <- function(x) {
  x <- unique(format(x, format = "%Y-%m-%d"))
  x <- sort(x)
  if (length(x) == 1) {
    return(x)
  } else {
    return(NULL) # paste(range(x), collapse = " - ")
  }
}

format_date <- function(x) {
  dd <- difftime(
    time1 = max(x, na.rm = TRUE),
    time2 = min(x, na.rm = TRUE),
    units = "days"
  )
  dd <- as.numeric(dd)
  if (dd < 1.1) {
    return("%H:%M")
  } else {
    return("%Y-%m-%d %H:%M")
  }
}

billboarder(data = short_term) %>%
  bb_linechart(mapping = bbaes(x = start_date, y = value, group = type)) %>%
  bb_color(palette = c("#E41A1C", "#377EB8", "#4DAF4A",
                       "#984EA3", "#FF7F00", "#FFFF33",
                       "#A65628", "#F781BF")) %>%
  bb_x_axis(tick = list(
    format = format_date(short_term$start_date),
    fit = FALSE, multiline = TRUE, width = 72
  )) %>%
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
balance[type == "Imports", value := value * -1L]

billboarder(data = balance) %>%
  bb_linechart(mapping = bbaes(x = start_date, y = value, group = type), type = "area") %>%
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

data[, start_date := as.POSIXct(start_date)]

billboarder() %>%
  bb_linechart(
    data = res,
    mapping = bbaes(x = start_date, y = ymin, ymin = ymin, ymax = ymax, group = fill),
    type = "area-line-range"
  ) %>%
  bb_linechart(data = data, mapping = bbaes(x = start_date, y = value, group = type)) %>%
  bb_colors_manual(
    "in favour" = "#cd9b1d",
    "against" = "#b22222",
    "Imports" = "#cd9b1d",
    "Exports" = "#b22222",
    opacity = 0.4
  ) %>%
  bb_x_axis(tick = list(fit = FALSE)) %>%
  bb_y_grid(show = TRUE) %>%
  bb_x_grid(show = TRUE) %>%
  bb_labs(
    title = "Electricity balance for France",
    caption = "https://data.rte-france.com",
    y = "Physical flows (MW)"
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









# Actual generation -------------------------------------------------------

prod_type_30 <- get_actual_generation(
  resource = "actual_generations_per_production_type",
  start_date = Sys.Date() - 30, end_date = Sys.Date()
)

autoplot(prod_type_30, by_day = TRUE)



object <- copy(prod_type_30)
object <- object[production_type != "TOTAL"]
if (TRUE) {
  object <- object[, list(value = mean(value)), by = list(start_date = as.Date(start_date), production_type)]
}
object[production_type %chin% c("HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_WATER_RESERVOIR"), production_type := "HYDRO"]
object <- object[, list(value = round(sum(value))), by = list(production_type, start_date)]
object <- object[, group := factor(
  x = production_type, levels = c("BIOMASS", "FOSSIL_GAS", "FOSSIL_HARD_COAL", "FOSSIL_OIL",
                                  "HYDRO", "NUCLEAR", "SOLAR", "WASTE",
                                  "WIND_ONSHORE", "HYDRO_PUMPED_STORAGE")
)]
object[, group := as.numeric(group)]
object[production_type == "HYDRO_PUMPED_STORAGE", group := 0]
object[production_type == "FOSSIL_HARD_COAL", group := -1]
object <- object[order(group, start_date)]

# str(billboarder:::bbmapping(data = object, mapping = bbaes(x = start_date, y = value, group = production_type)))

billboarder(data = object) %>%
  bb_linechart(
    mapping = bbaes(x = start_date, y = value, group = production_type),
    type = "area-spline"
  ) %>%
  bb_data(
    order = NULL,
    groups = list(as.list(unique(object$production_type))),
    names = as.list(
      setNames(
        object = rte.data:::capitalize(unique(object$production_type)),
        nm = unique(object$production_type)
      )
    )
  ) %>%
  bb_tooltip(order = "") %>%
  bb_colors_manual(
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
  ) %>%
  bb_y_grid(show = TRUE) %>%
  bb_x_grid(show = TRUE) %>%
  bb_legend(
    position = "bottom",
    inset = list(anchor = "top-right"),
    equally = FALSE, usePoint = TRUE
  ) %>%
  bb_labs(
    title = "French electricity generation per production type",
    caption = "https://data.rte-france.com",
    y = "Production (in MW)"
  )








# Active production units -------------------------------------------------


active_units <- retrieve_active_units()

# data
object <- copy(active_units)

# date range
subtitle <- paste0(
  "From ", format(min(object$start_date, na.rm = TRUE)),
  " to ", format(max(object$end_date, na.rm = TRUE))
)

# munging
object[type %chin% c("HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_WATER_RESERVOIR"), type := "HYDRO"]
object <- object[, list(
  working = round(sum(prod_max, na.rm = TRUE) / sum(installed_capacity, na.rm = TRUE) * 100, 2),
  stopped = 100 - round(sum(prod_max, na.rm = TRUE) / sum(installed_capacity, na.rm = TRUE) * 100, 2),
  n = .N
), by = type]
object <- melt(data = object, id.vars = c("type", "n"), measure.vars = c("working", "stopped"))
object[, variable := factor(x = variable, levels = c("stopped", "working"))]
object <- object[is.finite(value)]
object[value < 0, value := 0]
object[value > 100, value := 100]
object[, type := rte.data:::capitalize(type)]

billboarder(data = object) %>%
  bb_barchart(
    mapping = bbaes(x = type, y = value, group = variable),
    stacked = TRUE, rotated = TRUE
  ) %>%
  bb_colors_manual(
    "stopped" = "firebrick",
    "working" = "forestgreen"
  ) %>%
  bb_y_grid(show = TRUE) %>%
  bb_y_axis(tick = list(format = suffix("%")), padding = 0) %>%
  bb_legend(usePoint = TRUE) %>%
  bb_labs(
    title = "Proportion of active units by branch",
    caption = "https://data.rte-france.com"
  )




# Map active production units ---------------------------------------------

active_units <- retrieve_active_units()

object <- copy(active_units)

# code eic <> location
code_eic_loc <- merge(
  x = code_eic, all.x = TRUE, all.y = FALSE,
  y = pplocations[, list(eic_parent = eic_code, lat, lon)]
)

# installed capacities <> location
object <- merge(
  x = object,
  y = unique(code_eic_loc[!is.na(lat), list(eic_parent, lat, lon)]),
  by = "eic_parent", all.x = FALSE, all.y = FALSE
)

# simplify type plant
object[, type := gsub(pattern = "_.*", replacement = "", x = type)]

# group by plant
object[, active := prod_max > 1]
object <- object[, list(name = first(name), active = sum(active) / .N, n = .N), by = list(type, lat, lon)]
object <- object[, name := gsub(pattern = "\\s\\d$", replacement = "", x = name)]

# categories
object[, active_cat := cut(
  x = active,
  breaks = c(-Inf, 0, 0.25, 0.5, 0.75, 1),
  labels = c("0%", "25%", "50%", "75%", "100%"),
  include.lowest = TRUE
)]



getColor <- function(x) {
  cols <- c(
    "0%" = "red",
    "25%" = "lightred",
    "50%"= "orange",
    "75%" = "lightgreen",
    "100%" = "green"
  )
  unname(cols[x])
}
getIcon <- function(x) {
  icons <- c(
    "NUCLEAR" = "ion-nuclear",
    "FOSSIL" = "ion-flame",
    "HYDRO" = "ion-waterdrop"
  )
  unname(icons[x])
}

icons <- awesomeIcons(
  icon = getIcon(object$type),
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(object$active_cat)
)


leaflet(object) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addAwesomeMarkers(
    lng = ~lon, lat = ~lat,
    icon = icons,
    label = ~lapply(paste0(
      "<b>", name, "</b>", "<br>",
      "Actine units: ", active_cat, " (number of units: ", n, ")"
    ), HTML)
  ) %>%
  addLegend(
    colors =  c("#d43e2a", "#ff8e7f", "#f69730", "#b9f770", "#72af26"),
    labels = c("0%", "25%", "50%", "75%", "100%"), title = "% active units", opacity = 1
  )






# Map Generation installed capacities -------------------------------------

inst_cap <- get_open_api(
  api = "generation_installed_capacities",
  resource = "capacities_per_production_unit"
)


object <- copy(inst_cap)

# code eic <> location
code_eic_loc <- merge(
  x = code_eic, all.x = TRUE, all.y = FALSE,
  y = pplocations[, list(eic_parent = eic_code, lat, lon)]
)

# installed capacities <> location
object <- merge(
  x = object,
  y = unique(code_eic_loc[!is.na(lat), list(code_eic = eic_parent, lat, lon)]),
  by = "code_eic", all.x = FALSE, all.y = FALSE
)

# simplify type plant
object[, type := gsub(pattern = "_.*", replacement = "", x = type)]

# group by type2 and localisation
object <- object[, list(
  installed_capacity = sum(installed_capacity),
  name = first(name)
), by = list(type, lat, lon)]

object[, type := rte.data:::capitalize(type)]


pal <- colorFactor(
  palette = c("#58D3F7", "#f8ca4c", "#f30a0a"),
  domain = c("Hydro", "Nuclear", "Fossil"),
  ordered = TRUE
)
leaflet(data = object) %>%
  # addTiles(urlTemplate = "https://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png") %>%
  # addProviderTiles(provider = providers$Hydda.Full, group = providers$Hydda.Full) %>%
  # addProviderTiles(provider = providers$OpenStreetMap.France, group = providers$OpenStreetMap.France) %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  # addLayersControl(
    # baseGroups = c("Hydda.Full", "OpenStreetMap.France"),
    # options = layersControlOptions(collapsed = TRUE)
  # ) %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    popup = ~paste0(
      "<b>", name, "</b>", "<br>",
      "Installed capacity: ", format(installed_capacity, big.mark = ",", digits = 0, format = "f"), "MW"
    ),
    radius = ~scales::rescale(installed_capacity, to = c(8, 20)),
    color = ~pal(type),
    stroke = FALSE,
    fillOpacity = 0.8
  ) %>%
  addLegend(
    pal = pal,
    values = ~type, title = "Sector",
    opacity = 0.9
  )
