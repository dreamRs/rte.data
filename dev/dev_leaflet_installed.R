

# Installed capacities with Leaflet ---------------------------------------


# Packages ----

library(rte.data)
library(data.table)



# Data ----

inst_cap <- get_open_api(
  api = "generation_installed_capacities",
  resource = "capacities_per_production_unit"
)

# code eic <> location
data(code_eic)
data(pplocations)
code_eic_loc <- merge(
  x = code_eic, all.x = TRUE, all.y = FALSE,
  y = pplocations[, list(eic_parent = eic_code, lat, lon, X, Y)]
)

# installed capacities <> location
inst_cap <- merge(
  x = inst_cap,
  y = unique(code_eic_loc[!is.na(lat), list(code_eic = eic_parent, lat, lon)]),
  by = "code_eic", all.x = FALSE, all.y = FALSE
)

# simplify type plant
inst_cap[, type2 := gsub(pattern = "_.*", replacement = "", x = type)]

# group by type2 and localisation
inst_cap <- inst_cap[, list(
  installed_capacity = sum(installed_capacity),
  name = first(name)
), by = list(type2, lat, lon)]

inst_cap[, name := rte.data:::capitalize(name)]
inst_cap[, type2 := rte.data:::capitalize(type2)]



# Map ----

library(leaflet)
pal <- colorFactor(
  palette = c("#58D3F7", "#f8ca4c", "#f30a0a"),
  domain = c("Hydro", "Nuclear", "Fossil"),
  ordered = TRUE
)
leaflet(data = inst_cap) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    popup = ~paste0(
      "<b>", name, "</b>", "<br>",
      "Installed capacity: ", format(installed_capacity, big.mark = ",", digits = 0, format = "f"), "MW"
    ),
    radius = ~scales::rescale(installed_capacity, to = c(8, 20)),
    color = ~pal(type2),
    stroke = FALSE,
    fillOpacity = 0.8
  ) %>%
  addLegend(
    pal = pal,
    values = ~type2, title = "Sector",
    opacity = 0.9
  )


