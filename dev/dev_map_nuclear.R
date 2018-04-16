

#  ------------------------------------------------------------------------
#
# Title : Map Nuclear Units
#    By : VP
#  Date : 2018-04-15
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )
library( ggrepel )
library( data.table )





# Datas -------------------------------------------------------------------

active_unit <- retrieve_active_units()
active_unit <- active_unit[type == "NUCLEAR"]
active_unit[, active := prod_max > 1]
active_unit <- active_unit[, list(name, prod_max, installed_capacity, active)]
active_unit[, name_site := gsub(pattern = "\\s\\d", replacement = "", x = name)]
active_unit[, name_site := gsub(pattern = "ST\\s", replacement = "SAINT ", x = name_site)]
active_unit


geo_nuc <- readRDS(file = "dev/geonuclear.rds")

active_unit <- merge(
  x = active_unit,
  y = geo_nuc[, list(name_site = name, latitude, longitude)],
  by = "name_site", all.x = TRUE
)





# Map data ----------------------------------------------------------------

library( rgdal )
library( sp )

# download.file(
#   url = "http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20140306-100m-shp.zip",
#   destfile = "dev/shapefiles/departements-20140306-100m-shp.zip"
# )
# unzip(
#   zipfile = "dev/shapefiles/departements-20140306-100m-shp.zip",
#   exdir = "dev/shapefiles/departements-20140306-100m-shp"
# )
# fra_dept <- readOGR(
#   dsn = "dev/shapefiles/departements-20140306-100m-shp",
#   layer = "departements-20140306-100m", stringsAsFactors = FALSE
# )
# fra_dept <- fra_dept[fra_dept@data$code_insee %in% sprintf("%02d", (1:95)[-20]), ]
# fra_dept <- fortify(fra_dept, region = "code_insee")
# saveRDS(fra_dept, file = "dev/fra_dept.rds")


fra_dept <- readRDS(file = "dev/fra_dept.rds")




# Map: work/stop + labels -------------------------------------------------


ggplot(fra_dept) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    fill = "grey99", color = "grey"
  ) +
  # geom_jitter(
  #   data = active_unit, width =  0.25, height =  0.25, size = 2,
  #   mapping = aes(x = longitude, y = latitude, color = active)
  # ) +
  geom_point(
    data = active_unit, size = 4,
    mapping = aes(x = longitude, y = latitude, color = active)
  ) +
  scale_color_manual(
    values = c("TRUE" = "forestgreen", "FALSE" = "firebrick"),
    labels = c("TRUE" = "Working", "FALSE" = "Stopped")
  ) +
  geom_label_repel(
    mapping = aes(
      x = longitude, y = latitude,
      label = paste0(rte.data:::capitalize(name_site), ": ", active, "/", n)
    ),
    label.size = 0.05, label.r = 0.3, label.padding = 0.2,
    data = active_unit[, list(active = sum(active), n = .N), by = list(name_site, longitude, latitude)]
  ) +
  theme_void() + coord_map(projection = "mercator")








# Map: % active units -----------------------------------------------------

active_unit_p <- active_unit[, list(active = sum(active) / .N, n = .N), by = list(name_site, longitude, latitude)]

ggplot(data = fra_dept) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    fill = "grey99", color = "grey"
  ) +
  geom_point(
    data = active_unit_p, size = 4,
    mapping = aes(x = longitude, y = latitude, color = active)
  ) +
  scale_color_distiller(
    palette = "RdYlGn", direction = 1,
    labels = scales::percent,
    limits = c(0, 1),
    guide = guide_colourbar(
      direction = "horizontal",
      title.position = 'top',
      title = "% of active units",
      barheight = 0.5,
      barwidth = 15
    )
  ) +
  theme_void() +
  theme(legend.position = "bottom") + coord_map(projection = "mercator")



# With categories
active_unit_p[, active_cat := cut(
  x = active,
  breaks = c(-Inf, 0, 0.25, 0.5, 0.75, 1),
  labels = c("0%", "25%", "50%", "75%", "100%"),
  include.lowest = TRUE
)]
active_unit_p

ggplot(data = fra_dept) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    # fill = "grey95", color = "grey50"
    fill = "grey30", color = "grey90"
  ) +
  geom_point(
    data = active_unit_p, size = 5,
    mapping = aes(x = longitude, y = latitude, color = active_cat)
  ) +
  scale_color_brewer(
    palette = "RdYlGn", direction = 1, drop = FALSE,
    guide = guide_legend(title = "% of active units", title.position = "top")
  ) +
  # scale_color_manual(
  #   values = c("0%" = "#A50026", "25%" = "#F46D43", "50%"= "#FDAE61", "75%" = "#66BD63", "100%" = "#006837"),
  #   drop = FALSE
  # ) +
  theme_void() +
  theme(legend.position = "bottom") + coord_map(projection = "mercator")

# colorRampPalette(c("#D7191C", "#FDAE61", "#1A9641"))(5)





# Map: Europe background --------------------------------------------------

europe <- readRDS(file = "dev/europe.rds")


ggplot(fra_dept) +
  geom_polygon(
    data = europe,
    mapping = aes(x = long, y = lat, group = group),
    # fill = "grey30", color = "grey"
    fill = "grey98", color = "grey30"
  ) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    # fill = "grey30", color = "grey"
    fill = "grey98", color = "grey30"
  ) +
  geom_point(
    data = active_unit_p, size = 5,
    mapping = aes(x = longitude, y = latitude, color = active_cat)
  ) +
  scale_color_brewer(
    palette = "RdYlGn", direction = 1, drop = FALSE,
    guide = guide_legend(title = "% of active units", title.position = "top")
  ) +
  coord_map(
    projection = "mercator",
    xlim = range(fra_dept$long) + abs(range(fra_dept$long)) * c(-0.05, 0.05),
    ylim = range(fra_dept$lat) + abs(range(fra_dept$long)) * c(-0.05, 0.05)
  ) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue"))



ggplot(fra_dept) +
  geom_polygon(
    data = europe,
    mapping = aes(x = long, y = lat, group = group),
    # fill = "grey30", color = "grey"
    fill = "grey98", color = "grey30"
  ) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group),
    # fill = "grey30", color = "grey"
    fill = "grey98", color = "grey30"
  ) +
  geom_point(
    data = active_unit_p, size = 5,
    mapping = aes(x = longitude, y = latitude, color = active)
  ) +
  scale_color_distiller(
    palette = "RdYlGn", direction = 1,
    labels = scales::percent,
    limits = c(0, 1),
    guide = guide_colourbar(
      direction = "horizontal",
      title.position = 'top',
      title = "% of active units",
      barheight = 0.5,
      barwidth = 15
    )
  ) +
  coord_map(
    projection = "mercator",
    xlim = range(fra_dept$long) + abs(range(fra_dept$long)) * c(-0.05, 0.05),
    ylim = range(fra_dept$lat) + abs(range(fra_dept$long)) * c(-0.05, 0.05)
  ) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue"), legend.position = "bottom")
