

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
europe <- world[world@data$ADMIN %in% c("France", "Spain", "England", "Belgium", "Italy", "Andorra",
                                        "Luxembourg", "Netherlands", "Switzerland", "Germany"), ]
plot(europe)

europe <- fortify(europe, region = "ADMIN")
saveRDS(europe, file = "dev/europe.rds")

