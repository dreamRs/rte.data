
# ggplot2 plots ----


# Plot consumption forecast from `get_consumption`

#' @importFrom ggplot2 ggplot geom_line aes_ labs scale_x_datetime
#'  scale_color_brewer theme_minimal theme element_text waiver
plot_consumption <- function(object, date_labels = "%H:%M") {

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
}


# Plot Imports/Exports timeserie from `get_physical_flows`

#' @importFrom data.table data.table copy :=
#' @importFrom ggplot2 ggplot geom_line geom_ribbon aes_ labs
#'  scale_color_manual scale_fill_manual theme_minimal theme element_text
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



# Plot exchange with neighbouring countries from `get_physical_flows`

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
    geom_col(mapping = aes_(x = ~country, y = ~value/1e3, fill = ~flow)) +
    geom_hline(yintercept = 0, lty = "longdash", size = 1) +
    scale_fill_manual(values = c("firebrick", "goldenrod3"), name = "Flow") +
    scale_y_continuous(limits = c(-max(abs(object$value)), max(abs(object$value)))/1e3) +
    labs(
      title = "Exchanges with neighbouring countries",
      subtitle = range_dat,
      caption = "https://data.rte-france.com",
      x = NULL, y = "Physical flows (GW)"
    ) +
    theme_minimal() +
    coord_flip() +
    theme(plot.title = element_text(face = "bold"))
}



#' @importFrom data.table first copy
#' @importFrom ggplot2 ggplot geom_polygon aes_ geom_point
#'  scale_color_manual guide_legend scale_radius coord_equal
#'  theme_void theme element_rect element_text margin labs
#' @importFrom utils data
map_installed_capacities <- function(object) {

  object <- copy(object)

  # code eic <> location
  code_eic_loc <- merge(
    x = code_eic, all.x = TRUE, all.y = FALSE,
    y = pplocations[, list(eic_parent = eic_code, lat, lon, X, Y)]
  )

  # installed capacities <> location
  object <- merge(
    x = object,
    y = unique(code_eic_loc[!is.na(lat), list(code_eic = eic_parent, X, Y)]),
    by = "code_eic", all.x = FALSE, all.y = FALSE
  )

  # simplify type plant
  object[, type2 := gsub(pattern = "_.*", replacement = "", x = type)]

  # group by type2 and localisation
  object <- object[, list(
    installed_capacity = sum(installed_capacity),
    name = first(name)
  ), by = list(type2, X, Y)]

  ggplot() +
    geom_polygon(
      data = europe,
      mapping = aes_(x = ~long, y = ~lat, group = ~group),
      fill = "#5f799c", color = "#d7dee7"
      # fill = "grey98", color = "grey30"
    ) +
    geom_polygon(
      data = fra_dept,
      mapping = aes_(x = ~long, y = ~lat, group = ~group),
      fill = "#5f799c", color = "#d7dee7"
      # fill = "grey98", color = "grey30"
    ) +
    geom_point(
      data = object,
      mapping = aes_(x = ~X, y = ~Y, color = ~type2, size = ~installed_capacity),
      alpha = 1
    ) +
    scale_color_manual(
      values = c(
        "HYDRO" = "#58D3F7", #"#2772b2",
        "NUCLEAR" = "#f8ca4c",
        "FOSSIL" = "#f30a0a"
      ),
      labels = capitalize,
      name = "Sector",
      guide = guide_legend(override.aes = list(size = 4))
    ) +
    scale_radius(
      range = c(2, 10), name = "Capacity max.\n(in MW)"
      # , guide = guide_legend(override.aes = list(size = 1:5))
    ) +
    coord_equal(
      xlim = range(fra_dept$long) + abs(range(fra_dept$long)) * c(-0.05, 0.05),
      ylim = range(fra_dept$lat) + abs(range(fra_dept$long)) * c(-0.05, 0.05)
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "lightblue")
    ) +
    labs(
      title = "Generation Installed Capacities",
      subtitle = "Only production units of more than 1 MW are shown",
      fill = NULL, y = NULL, x = NULL, colour = NULL,
      caption = "https://data.rte-france.com"
    )
}


# Plot percentage of active production units from `retrieve_active_units`

#' @importFrom data.table copy melt :=
#' @importFrom ggplot2 ggplot geom_col aes geom_point geom_text coord_flip
#'  theme_minimal theme labs scale_fill_manual scale_colour_manual
#'  guide_legend scale_y_continuous scale_x_discrete margin
#' @importFrom scales percent
plot_p_active_units <- function(object) {
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

  # plot
  ggplot(data = object) +
    geom_col(
      mapping = aes(x = type, y = value, fill = variable),
      position = "fill", width = 0.5, alpha = 0.8, show.legend = FALSE
    ) +
    geom_point(
      mapping = aes(x = NA_character_, y = NA_real_, colour = variable),
      na.rm = TRUE
    ) +
    geom_text(
      # position = "fill",
      mapping = aes(
        label = paste(capitalize(type), paste0(round(value, 1), "% (n=", n, ")"), sep = " : "),
        x = type, y = 0
      ),
      hjust = 0, nudge_x = 0.5, size = 4.5,
      color = "black", data = object[variable == "working"]
    ) +
    coord_flip() + theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.subtitle = element_text(margin = margin(0, 0, 20, 0))
    ) +
    labs(
      title = "Proportion of active units by branch",
      subtitle = subtitle,
      fill = NULL, y = NULL, x = NULL, colour = NULL,
      caption = "https://data.rte-france.com"
    ) +
    scale_fill_manual(
      values = c("stopped" = "firebrick", "working" = "forestgreen")
    ) +
    scale_colour_manual(
      values = c("stopped" = "firebrick", "working" = "forestgreen"),
      guide = guide_legend(
        override.aes = list(shape = 16, size = 5), title.position = "top", label.position = "right",
        title.hjust = 0.5, label.hjust = 1, nrow = 1, byrow = TRUE, reverse = TRUE
      )
    ) +
    scale_y_continuous(labels = percent, expand = c(0.01, 0)) +
    scale_x_discrete(labels = NULL)
}



# Plot actual generation from `get_actual_generation`

#' @importFrom data.table copy := %chin%
#' @importFrom ggplot2 ggplot geom_area aes_ labs guide_legend
#'  scale_fill_manual theme_minimal theme element_text
plot_actual_generation <- function(object, by_day = NULL) {
  api_time <- attr(object, "api.time")

  object <- copy(object)
  object <- object[production_type != "TOTAL"]
  if (!is.null(by_day) && by_day) {
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
      y = "Production (in MW)", x = NULL,
      caption = "https://data.rte-france.com"
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



#' @importFrom data.table copy first
#' @importFrom ggplot2 ggplot geom_polygon aes_ geom_point
#'  scale_color_manual guide_legend scale_shape coord_equal
#'  theme_void theme element_rect labs
map_p_active_units <- function(object) {

  object <- copy(object)

  # code eic <> location
  code_eic_loc <- merge(
    x = code_eic, all.x = TRUE, all.y = FALSE,
    y = pplocations[, list(eic_parent = eic_code, lat, lon, X, Y)]
  )

  # installed capacities <> location
  object <- merge(
    x = object,
    y = unique(code_eic_loc[!is.na(lat), list(eic_parent, X, Y)]),
    by = "eic_parent", all.x = FALSE, all.y = FALSE
  )

  # simplify type plant
  object[, type2 := gsub(pattern = "_.*", replacement = "", x = type)]

  # group by plant
  object[, active := prod_max > 1]
  object <- object[, list(name = first(name), active = sum(active) / .N, n = .N), by = list(type2, X, Y)]
  object <- object[, name := gsub(pattern = "\\s\\d$", replacement = "", x = name)]

  # categories
  object[, active_cat := cut(
    x = active,
    breaks = c(-Inf, 0, 0.25, 0.5, 0.75, 1),
    labels = c("0%", "25%", "50%", "75%", "100%"),
    include.lowest = TRUE
  )]

  # Map
  ggplot() +
    geom_polygon(
      data = europe,
      mapping = aes_(x = ~long, y = ~lat, group = ~group),
      fill = "#5f799c", color = "#d7dee7"
      # fill = "grey98", color = "grey30"
    ) +
    geom_polygon(
      data = fra_dept,
      mapping = aes_(x = ~long, y = ~lat, group = ~group),
      fill = "#5f799c", color = "#d7dee7"
    ) +
    geom_point(
      data = object,
      mapping = aes_(x = ~X, y = ~Y, color = ~active_cat, shape = ~type2),
      alpha = 1, size = 5
    ) +
    scale_color_manual(
      values = c("0%" = "#E31A1C", "25%" = "#FB9A99", "50%"= "#FDBF6F", "75%" = "#B2DF8A", "100%" = "#33A02C"),
      drop = FALSE,
      guide = guide_legend(title = "% of active units", title.position = "top")
    ) +
    scale_shape(
      name = "Sector", labels = capitalize
    ) +
    coord_equal(
      xlim = range(fra_dept$long) + abs(range(fra_dept$long)) * c(-0.05, 0.05),
      ylim = range(fra_dept$lat) + abs(range(fra_dept$long)) * c(-0.05, 0.05)
    ) +
    theme_void() +
    theme(
      panel.background = element_rect(fill = "lightblue")
    ) +
    labs(
      title = "Active units",
      subtitle = "Only production units of more than 1 MW are shown",
      fill = NULL, y = NULL, x = NULL, colour = NULL,
      caption = "https://data.rte-france.com"
    )
}

