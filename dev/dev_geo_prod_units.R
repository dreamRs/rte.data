

#  ------------------------------------------------------------------------
#
# Title : Production units geoloc
#    By : VP
#  Date : 2018-04-15
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )
library( data.table )
# devtools::install_github(repo = 'rCarto/photon')
library( photon )
library( jsonlite )
library( rvest )



# Datas -------------------------------------------------------------------

# actual generation
prod_unit <- get_actual_generation("actual_generations_per_unit", start_date = Sys.Date() - 1)
str(prod_unit, max.level = 2)
prod_unit

dat <- retrieve_active_units()
dat



# Test TRICASTIN ----------------------------------------------------------

geocode("TRICASTIN 1", key = "place")

url <- "https://fr.wikipedia.org/w/api.php?action=query&list=search&srsearch=%s&prop=info&inprop=url&utf8=&format=json"
res <- fromJSON(txt = sprintf(url, gsub(x = "CENTRALE NUCLEAIRE DE BELLEVILLE", pattern = " ", replacement = "_")))
res

url <- "http://fr.dbpedia.org/data/%s.json"
name <- gsub(" ", "_", "Centrale nucléaire de Chinon")
res <- fromJSON(txt = sprintf(url, name))
res

ressources <- sprintf("http://fr.dbpedia.org/resource/%s", name)
latitude <- res[[ressources]][["http://fr.dbpedia.org/property/latitude"]]$value
longitude <- res[[ressources]][["http://fr.dbpedia.org/property/longitude"]]$value


get_loc <- function(x, pattern_title = NULL) {
  url_wiki <- "https://fr.wikipedia.org/w/api.php?action=query&list=search&srsearch=%s&prop=info&inprop=url&utf8=&format=json"
  res_wiki <- fromJSON(txt = sprintf(url_wiki, gsub(x = x, pattern = " ", replacement = "-")))
  if (is.null(pattern_title)) {
    title <- res_wiki$query$search$title[1]
  } else {
    title <- grep(pattern = pattern_title, x = res_wiki$query$search$title, value = TRUE)[1]
  }
  url_dbpedia <- "http://fr.dbpedia.org/data/%s.json"
  res_dbpedia <- fromJSON(txt = sprintf(url_dbpedia, gsub(" ", "_", title)))
  ressources <- sprintf("http://fr.dbpedia.org/resource/%s", gsub(" ", "_", title))
  latitude <- res_dbpedia[[ressources]][["http://fr.dbpedia.org/property/latitude"]]$value
  longitude <- res_dbpedia[[ressources]][["http://fr.dbpedia.org/property/longitude"]]$value
  list(title = title, latitude = latitude, longitude = longitude)
}

get_loc("TRICASTIN 1")


unique(prod_unit$name)
get_loc("CHINON")
get_loc("CHINON", "Centrale")

dat[type == "NUCLEAR", c(name)]
get_loc("BLAYAIS 3")


centrales <- dat[type == "NUCLEAR", c(name)]
centrales <- gsub(pattern = "\\s\\d", replacement = "", x = centrales)
centrales <- gsub(pattern = "ST\\s", replacement = "SAINT ", x = centrales)
centrales_loc <- lapply(
  X = unique(centrales),
  FUN = function(x) {
    loc <- try(get_loc(paste0("CENTRALE_NUCLEAIRE_DE_", x), "Centrale"), silent = TRUE)
    if ("try-error" %in% class(loc)) {
      list(name = x, title = NA_character_, latitude = NA_real_, longitude = NA_real_)
    } else {
      c(list(name = x), loc)
    }
  }
)
str(centrales_loc)
centrales_dt <- rbindlist(lapply(centrales_loc, as.data.table), fill = TRUE)
centrales_dt

saveRDS(centrales_dt, file = "dev/geonuclear.rds")


library(leaflet)
leaflet(centrales_dt) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude)



# Via scrapping -----------------------------------------------------------

url_centrales <- read_html("https://fr.wikipedia.org/wiki/Liste_des_r%C3%A9acteurs_nucl%C3%A9aires_en_France")
html_table(url_centrales, fill = TRUE)






# Fossil gas --------------------------------------------------------------

fossil_gas <-  dat[type == "FOSSIL_GAS", unique(name)]

get_loc("BOUCHAIN", "Centrale")
get_loc("MARTIGUES_PONTEAU", "Centrale")

library(ggmap)
geocode(location = "Centrale Thermique MARTIGUES PONTEAU")

geo_fossil_gas <- photon::geocode(paste("Centrale Thermique", gsub(pattern = "\\s\\d", replacement = "", x = fossil_gas)))
setDT(geo_fossil_gas)
geo_fossil_gas <- geo_fossil_gas[country == "France"]
geo_fossil_gas <- unique(geo_fossil_gas[, list(location, latitude = lat, longitude = lon)])
geo_fossil_gas <- unique(geo_fossil_gas, by = "location")

library(leaflet)
leaflet(geo_fossil_gas) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude)

library(leaflet)
leaflet(geo_fossil_gas[location == "Centrale Thermique FR-GA-MORANT1"]) %>%
  addTiles() %>%
  addMarkers(lng = ~longitude, lat = ~latitude)






# Via Open Power System Data ----------------------------------------------

input_plant_locations <- fread(input = "data-raw/input_plant_locations_FR.csv")

length(dat$name)
length(intersect(input_plant_locations$name, dat$name))
length(intersect(input_plant_locations$eic_code, dat$eic_parent))
length(intersect(input_plant_locations$eic_code, dat$eic_code))

dat_loc <- merge(
  x = dat, all.x = TRUE, all.y = FALSE,
  y = input_plant_locations[, list(eic_parent = eic_code, lat, lon)]
)

library(leaflet)
factpal <- colorFactor(RColorBrewer::brewer.pal(n = 8, name = "Dark2"), unique(dat_loc$type))
leaflet(dat_loc[!is.na(lat)]) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    fillColor = ~factpal(type),
    color = ~factpal(type),
    fillOpacity = 1,
    popup = ~type
  )


dat_loc[!is.na(lat), .N, by = type]


