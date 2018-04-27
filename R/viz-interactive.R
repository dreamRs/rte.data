
# interactive charts ----


#' @importFrom billboarder billboarder bb_linechart bbaes_string %>%
#'  bb_color bb_x_axis bb_y_grid bb_x_grid bb_legend bb_zoom bb_labs
viz_consumption <- function(object) {

  billboarder(data = object) %>%
    bb_linechart(mapping = bbaes_string(x = "start_date", y = "value", group = "type")) %>%
    bb_color(palette = c("#E41A1C", "#377EB8", "#4DAF4A",
                         "#984EA3", "#FF7F00", "#FFFF33",
                         "#A65628", "#F781BF")) %>%
    bb_x_axis(tick = list(
      format = format_date(object$start_date),
      fit = FALSE, multiline = TRUE, width = 72
    )) %>%
    bb_y_grid(show = TRUE) %>%
    bb_x_grid(show = TRUE) %>%
    bb_legend(position = "right") %>%
    bb_zoom(enabled = TRUE) %>%
    bb_labs(
      title = "French electricity consumption : forecast vs realised",
      caption = "https://data.rte-france.com",
      y = "Electricity consumption (MW)"
    ) %>%
    bb_x_axis(label = list(
      text = range_date(object$start_date),
      position = "outer-center"
    ))

}


#' @importFrom billboarder billboarder bb_linechart bbaes_string
#'  bb_x_axis bb_y_grid bb_x_grid bb_colors_manual bb_labs %>%
#' @importFrom data.table copy :=
viz_balance <- function(object) {

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
  object[type == "Imports", value := value * -1L]

  billboarder(data = object) %>%
    bb_linechart(mapping = bbaes_string(x = "start_date", y = "value", group = "type"), type = "area") %>%
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
}



#' @importFrom billboarder billboarder bb_barchart bbaes_string
#'  bb_y_axis bb_y_grid bb_x_grid bb_legend bb_add_style
#'  bb_colors_manual bb_labs %>%
#' @importFrom data.table copy
#' @importFrom scales pretty_breaks
viz_exchange <- function(object) {

  object <- copy(object)

  object <- object[, list(value = sum(value)), by = list(sender_country_name, receiver_country_name)]
  object[, flow := ifelse(sender_country_name == "France", "Exports", "Imports")]
  object[flow == "Exports", value := value * -1L]
  object[, country := ifelse(flow == "Imports", sender_country_name, receiver_country_name)]
  object[, country := factor(x = country, levels = rev(sort(unique(country))))]
  object <- object[order(country)]

  range_val <- c(-max(abs(object$value)), max(abs(object$value)))

  billboarder(data = object) %>%
    bb_barchart(
      mapping = bbaes_string(x = "country", y = "value", group = "flow"),
      stacked = TRUE, rotated = TRUE
    ) %>%
    bb_y_axis(
      tick = list(values = pretty_breaks(4)(range_val)),
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
}



#' @importFrom billboarder billboarder bb_linechart bbaes_string bb_data
#'  bb_tooltip bb_colors_manual bb_y_grid bb_x_grid bb_legend bb_labs %>%
#' @importFrom data.table copy
#' @importFrom stats setNames
viz_actual_generation <- function(object, by_day = NULL) {
  object <- copy(object)
  object <- object[production_type != "TOTAL"]
  if (!is.null(by_day) && by_day) {
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


  billboarder(data = object) %>%
    bb_linechart(
      mapping = bbaes_string(x = "start_date", y = "value", group = "production_type"),
      type = "area-spline"
    ) %>%
    bb_data(
      order = NULL,
      groups = list(as.list(unique(object$production_type))),
      names = as.list(
        setNames(
          object = capitalize(unique(object$production_type)),
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
    bb_x_axis(tick = list(
      format = format_date(object$start_date),
      fit = FALSE, multiline = TRUE, width = 72
    )) %>%
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
}


#' @importFrom billboarder billboarder bb_barchart bbaes_string %>%
#'  bb_colors_manual bb_y_grid bb_y_axis suffix bb_legend bb_labs
#' @importFrom data.table copy melt := %chin%
viz_p_active_units <- function(object) {
  # data
  object <- copy(object)

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
  object[, type := capitalize(type)]

  billboarder(data = object) %>%
    bb_barchart(
      mapping = bbaes_string(x = "type", y = "value", group = "variable"),
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
}


#' @importFrom data.table copy first
#' @importFrom leaflet colorFactor leaflet addProviderTiles
#'  addCircleMarkers addLegend
#' @importFrom scales rescale
imap_installed_capacities <- function(object) {

  object <- copy(object)

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
    name = gsub(pattern = "\\s\\d$", replacement = "", x = first(name))
  ), by = list(type, lat, lon)]

  object[, type := capitalize(type)]


  pal <- colorFactor(
    palette = c("#58D3F7", "#f8ca4c", "#f30a0a"),
    domain = c("Hydro", "Nuclear", "Fossil"),
    ordered = TRUE
  )
  leaflet(data = object) %>%
    # addTiles(urlTemplate = "https://{s}.tile.openstreetmap.fr/osmfr/{z}/{x}/{y}.png") %>%
    addProviderTiles(provider =  "CartoDB.Positron") %>%
    addCircleMarkers(
      lng = ~lon, lat = ~lat,
      popup = ~paste0(
        "<b>", name, "</b>", "<br>",
        "Installed capacity: ", format(installed_capacity, big.mark = ",", digits = 0, format = "f"), "MW"
      ),
      radius = ~rescale(installed_capacity, to = c(8, 20)),
      color = ~pal(type),
      stroke = FALSE,
      fillOpacity = 0.8
    ) %>%
    addLegend(
      pal = pal,
      values = ~type, title = "Sector",
      opacity = 0.9
    )

}



#' @importFrom data.table copy first :=
#' @importFrom leaflet awesomeIcons leaflet addProviderTiles addAwesomeMarkers icons addLegend
#' @importFrom htmltools  HTML
imap_p_active_units <- function(object) {

  object <- copy(object)

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
  object <- object[, list(
    name = first(name), active = sum(active) / .N, n = .N
  ), by = list(type, lat, lon)]
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
}




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

