

# Power plants coordinates ------------------------------------------------

library(data.table)
library(sp)

# via https://github.com/Open-Power-System-Data/conventional_power_plants
input_plant_locations <- fread(file = "data-raw/Open-Power-System-Data/input_plant_locations_FR.csv")

# Conversion en Lambert93
data_coord <- as.data.frame(input_plant_locations[!is.na(lon)])
# Variables definissant les coordonnees
coordinates(data_coord) <- c("lon", "lat")
# Definition de la projection
proj4string(data_coord) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# Conversion
res <- spTransform(x = data_coord, CRSobj = CRS("+proj=lcc +lat_1=49 +lat_2=44
                                                +lat_0=46.5 +lon_0=3 +x_0=700000
                                                +y_0=6600000 +ellps=GRS80 +towgs84=0,
                                                0,0,0,0,0,0 +units=m +no_defs"))
res <- as.data.table(res)
setnames(x = res, old = c("lon", "lat"), new = c("X", "Y"))

input_plant_locations <- merge(input_plant_locations, res)

# code_eic_loc <- merge(
#   x = code_eic, all.x = TRUE, all.y = FALSE,
#   y = input_plant_locations[, list(eic_parent = eic_code, lat, lon, X, Y)]
# )

# saveRDS(object = code_eic_loc, file = "dev/code_eic_loc.rds")

saveRDS(object = input_plant_locations, file = "dev/input_plant_locations.rds")


pplocations <- copy(input_plant_locations)

# usethis::use_data(pplocations, internal = FALSE, overwrite = TRUE)





