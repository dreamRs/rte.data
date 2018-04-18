

#  ------------------------------------------------------------------------
#
# Title : Map data
#    By : VP
#  Date : 2018-04-16
#
#  ------------------------------------------------------------------------


# Source data: http://www.naturalearthdata.com/downloads/110m-cultural-vectors/


# Packages ----------------------------------------------------------------

library( rgdal )
library( sp )
library( ggplot2 )



# Europe ------------------------------------------------------------------

# download.file(
#   url = "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries.zip",
#   destfile = "dev/shapefiles/ne_10m_admin_0_countries.zip"
# )
# unzip(
#   zipfile = "dev/shapefiles/ne_10m_admin_0_countries.zip",
#   exdir = "dev/shapefiles/ne_10m_admin_0_countries"
# )


world <- readOGR(
  dsn = "dev/shapefiles/ne_10m_admin_0_countries",
  layer = "ne_10m_admin_0_countries", stringsAsFactors = FALSE
)
plot(world)
str(world@data)


# Filtre europe
europe <- world[world@data$ADMIN %in% c("France", "Spain", "United Kingdom", "Belgium", "Italy", "Andorra",
                                        "Luxembourg", "Netherlands", "Switzerland", "Germany"), ]
plot(europe)

# Change projection
lam93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
europe <- spTransform(europe, lam93)

# Fortify to have data.frame
europe <- fortify(europe, region = "ADMIN")
saveRDS(europe, file = "dev/europe.rds")





# France departement ------------------------------------------------------

# download.file(
#   url = "http://osm13.openstreetmap.fr/~cquest/openfla/export/departements-20140306-100m-shp.zip",
#   destfile = "dev/shapefiles/departements-20140306-100m-shp.zip"
# )
# unzip(
#   zipfile = "dev/shapefiles/departements-20140306-100m-shp.zip",
#   exdir = "dev/shapefiles/departements-20140306-100m-shp"
# )
fra_dept <- readOGR(
  dsn = "dev/shapefiles/departements-20140306-100m-shp",
  layer = "departements-20140306-100m", stringsAsFactors = FALSE
)
fra_dept <- fra_dept[fra_dept@data$code_insee %in% sprintf("%02d", (1:95)[-20]), ]
lam93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
fra_dept <- spTransform(fra_dept, lam93)
fra_dept <- fortify(fra_dept, region = "code_insee")
saveRDS(fra_dept, file = "dev/fra_dept.rds")

# usethis::use_data(fra_dept, internal = TRUE)



# Use pckg ----------------------------------------------------------------

usethis::use_data(fra_dept, europe, overwrite = TRUE, internal = TRUE)



# Test map ----------------------------------------------------------------

library(ggplot2)
ggplot() +
  geom_polygon(
    data = europe,
    mapping = aes(x = long, y = lat, group = group),
    # fill = "grey30", color = "grey"
    fill = "grey98", color = "grey30"
  ) +
  geom_polygon(
    data = fra_dept,
    mapping = aes(x = long, y = lat, group = group),
    fill = "grey98", color = "grey30"
  ) +
  coord_equal(
    xlim = range(fra_dept$long) + abs(range(fra_dept$long)) * c(-0.05, 0.05),
    ylim = range(fra_dept$lat) + abs(range(fra_dept$long)) * c(-0.05, 0.05)
  ) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightblue"))

