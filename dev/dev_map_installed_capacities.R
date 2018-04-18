

#  ------------------------------------------------------------------------
#
# Title : Dev map installed capacities
#    By : VP
#  Date : 2018-04-17
#
#  ------------------------------------------------------------------------




# Packages ----------------------------------------------------------------

library( rgdal )
library( sp )
library( ggplot2 )
library( rte.data )
library( data.table )




# Datas -------------------------------------------------------------------

gen_inst_unit <- get_open_api(
  api = "generation_installed_capacities",
  resource = "capacities_per_production_unit"
)
str(gen_inst_unit, max.level = 2)
gen_inst_unit

autoplot(gen_inst_unit)

code_eic_loc <- readRDS(file = "dev/code_eic_loc.rds")

# Merge location
gen_inst_unit <- merge(
  x = gen_inst_unit,
  y = unique(code_eic_loc[!is.na(lat), list(code_eic = eic_parent, X, Y)]),
  by = "code_eic", all.x = FALSE, all.y = FALSE
)

table(gen_inst_unit$type)


# Simplification type
gen_inst_unit[, type2 := gsub(pattern = "_.*", replacement = "", x = type)]

# group by type2 and localisation
gen_inst_unit2 <- gen_inst_unit[, list(
  installed_capacity = sum(installed_capacity),
  name = first(name)
), by = list(type2, X, Y)]

table(gen_inst_unit2$type2)


# map data
europe <- readRDS(file = "dev/europe.rds")
fra_dept <- readRDS(file = "dev/fra_dept.rds")



# Map ---------------------------------------------------------------------


ggplot() +
  geom_polygon(
    data = europe,
    mapping = aes(x = long, y = lat, group = group),
    fill = "#5f799c", color = "#d7dee7"
    # fill = "grey98", color = "grey30"
  ) +
  geom_polygon(
    data = fra_dept,
    mapping = aes(x = long, y = lat, group = group),
    fill = "#5f799c", color = "#d7dee7"
    # fill = "grey98", color = "grey30"
  ) +
  geom_point(
    data = gen_inst_unit2,
    mapping = aes(x = X, y = Y, color = type2, size = installed_capacity),
    alpha = 1
  ) +
  scale_color_manual(
    values = c(
      "HYDRO" = "#58D3F7", #"#2772b2",
      "NUCLEAR" = "#f8ca4c",
      "FOSSIL" = "#f30a0a"
    ),
    labels = rte.data:::capitalize,
    name = "Type",
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
    panel.background = element_rect(fill = "lightblue"),
    plot.title = element_text(margin = margin(10, 10, 20, 10))
  ) +
  labs(
    title = "Installed capacities by branch",
    # subtitle = subtitle,
    fill = NULL, y = NULL, x = NULL, colour = NULL,
    caption = "https://data.rte-france.com"
  )






