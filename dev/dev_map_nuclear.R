

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

download.file(
  url = "http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20140306-100m-shp.zip",
  destfile = "dev/shapefiles/departements-20140306-100m-shp.zip"
)
unzip(
  zipfile = "dev/shapefiles/departements-20140306-100m-shp.zip",
  exdir = "dev/shapefiles/departements-20140306-100m-shp"
)
fra_dept <- readOGR(
  dsn = "dev/shapefiles/departements-20140306-100m-shp",
  layer = "departements-20140306-100m", stringsAsFactors = FALSE
)
fra_dept <- fra_dept[fra_dept@data$code_insee %in% sprintf("%02d", (1:95)[-20]), ]
fra_dept <- fortify(fra_dept, region = "code_insee")


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


ggplot(mpg, aes(cyl, hwy)) + geom_jitter()
ggplot(mpg, aes(cyl, hwy)) + geom_jitter(width = 0.25)
