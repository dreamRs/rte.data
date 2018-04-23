

#  ------------------------------------------------------------------------
#
# Title : Actual generation vs Installed capacities
#    By : VP
#  Date : 2018-04-11
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )
library( data.table )


# API key -----------------------------------------------------------------

# set_key(
#   api = "actual_generation",
#   key = "BASE64KEY=="
# )
# set_key(
#   api = "generation_installed_capacities",
#   key = "BASE64KEY=="
# )



# Datas -------------------------------------------------------------------

# actual generation
prod_unit <- get_actual_generation("actual_generations_per_unit", start_date = Sys.Date() - 1)
str(prod_unit, max.level = 2)
prod_unit


# installed capacities
gen_inst_unit <- get_open_api(
  api = "generation_installed_capacities",
  resource = "capacities_per_production_unit", start_date = "2018-01-01"
)
str(gen_inst_unit, max.level = 2)
gen_inst_unit




# Matching unit -----------------------------------------------------------

# by name
uniqueN(gen_inst_unit$name) # 151
uniqueN(prod_unit$name) # 140
uniqueN(intersect(prod_unit$name, gen_inst_unit$name)) # 90

# by eic code
uniqueN(intersect(prod_unit$eic_code, gen_inst_unit$code_eic)) # 0...

prod_unit[name == "TRICASTIN 1", eic_code]
gen_inst_unit[name == "TRICASTIN 1", code_eic]

prod_unit[name == "TRICASTIN 2", eic_code]
gen_inst_unit[name == "TRICASTIN 2", code_eic]


# pas de match nom
sort(setdiff(prod_unit$name, gen_inst_unit$name))
prod_unit[name %like% "CHASTANG"]
gen_inst_unit[name %like% "CHASTANG"]


prod_unit[name %like% "BLENOD"]
gen_inst_unit[name %like% "BLENOD"]



# Correspondance eic code -------------------------------------------------

# https://clients.rte-france.com/servlets/CodeEICGServlet?type=W

table(substr(x = prod_unit$eic_code, start = 3, stop = 3))
table(substr(x = gen_inst_unit$code_eic, start = 3, stop = 3))

# download.file(url = "https://clients.rte-france.com/servlets/CodeEICGServlet?type=W", destfile = "dev/CodeEIC.xls", mode = "wb")
code_eic <- readxl::read_xls(path = "data-raw/CodeEIC.xls", skip = 2)
setDT(code_eic)
clean <- function(x) {
  x <- stringi::stri_trans_general(str = x, id = "Latin-ASCII")
  x <- tolower(x)
  gsub(pattern = "\\s+", replacement = "_", x = x)
}
setnames(
  x = code_eic, old = names(code_eic),
  new = clean(names(code_eic))
)
code_eic


gene_eic <- code_eic[fonction == "Generation Unit"]
gene_eic[, nom := stringi::stri_replace(str = nom_affiche, replacement = "", regex = "-G$")]
prod_eic <- code_eic[fonction == "Production Unit"]
prod_eic[, nom := stringi::stri_replace(str = nom_affiche, replacement = "", regex = "-P$")]

corresp_eic <- merge(
  x = gene_eic[, list(gene_eic = code_eic, name = nom, entity_name = nom_entite, eic_parent = eic_parent)],
  y = prod_eic[, list(prod_eic = code_eic, name = nom)],
  by = "name", all.x = TRUE, all.y = TRUE
)
corresp_eic

uniqueN(intersect(prod_unit$eic_code, corresp_eic$gene_eic))
uniqueN(intersect(gen_inst_unit$code_eic, corresp_eic$prod_eic))



# Generation parent -------------------------------------------------------

# eic_gen <- code_eic[fonction == "Generation Unit"]
# eic_gen

prodgen <- merge(
  x = prod_unit[, list(prod_max = max(value)), by = list(eic_code, name)],
  y = code_eic[, list(eic_code = code_eic, eic_parent)],
  by = "eic_code", all.x = TRUE, all.y = FALSE
)
prodgen
prodgen <- merge(
  x = prodgen,
  y = gen_inst_unit[, list(installed_capacity, eic_parent = code_eic, type)],
  by = "eic_parent", all.x = TRUE, all.y = FALSE
)
prodgen <- prodgen[!is.na(type)]
prodgen

# % prod/inst by type
prodgen[, list(
  p_prod = round(sum(prod_max, na.rm = TRUE) / sum(installed_capacity, na.rm = TRUE) * 100, 2)
), by = type]


# unit stoped
prodgen[, list(
  working = sum(prod_max > 1),
  stop = sum(prod_max <= 1),
  n_unit = .N
), by = type]








# Plots -------------------------------------------------------------------

library(ggplot2)

prodgen[type %chin% c("HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_WATER_RESERVOIR"), type := "HYDRO"]
percent_prod <- prodgen[, list(
  working = round(sum(prod_max, na.rm = TRUE) / sum(installed_capacity, na.rm = TRUE) * 100, 2),
  stopped = 100 - round(sum(prod_max, na.rm = TRUE) / sum(installed_capacity, na.rm = TRUE) * 100, 2),
  n= .N
), by = type]
percent_prod <- melt(data = percent_prod, id.vars = c("type", "n"), measure.vars = c("working", "stopped"))
percent_prod[, variable := factor(x = variable, levels = c("stopped", "working"))]
percent_prod <- percent_prod[is.finite(value)]
percent_prod[value < 0, value := 0]
percent_prod[value > 100, value := 100]

ggplot(data = percent_prod) +
  geom_col(mapping = aes(x = type, y = value, fill = variable), position = "fill", width = 0.5, show.legend = FALSE) +
  geom_point(mapping = aes(x = NA_character_, y = NA_real_, colour = variable), na.rm = TRUE) +
  geom_text(
    # position = "fill",
    mapping = aes(label = paste(rte.data:::capitalize(type), paste0(round(value, 1), "% (n=", n, ")"), sep = " : "), x = type, y = 0),
    hjust = 0, nudge_x = 0.5, size = 4.5,
    color = "black", data = percent_prod[variable == "working"]
  ) +
  coord_flip() + theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    title = "Proportion of active units by branch",
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
  scale_y_continuous(labels = scales::percent, expand = c(0.01, 0)) +
  scale_x_discrete(labels = NULL)




ggplot(data = percent_prod) +
  geom_col(mapping = aes(x = type, y = value, fill = variable), position = "fill") +
  geom_text(
    # position = "fill",
    mapping = aes(label = paste0(value, "%"), x = type, y = 0.1), #hjust = 1.1,
    color = "snow", data = percent_prod[variable == "working"]
  )









# Test fun ----------------------------------------------------------------

library(rte.data)
library(data.table)

dat <- retrieve_active_units()
dat

dat[name %chin% "TRICASTIN 1"]

autoplot(dat)








# Map ---------------------------------------------------------------------

library(rte.data)
library(data.table)
library(ggplot2)

fra_dept <- readRDS(file = "dev/fra_dept.rds")
europe <- readRDS(file = "dev/europe.rds")


dat <- retrieve_active_units()
dat

object <- copy(dat)


# add coordinates
data("code_eic")
data("pplocations")

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
    mapping = aes_(x = ~X, y = ~Y, color = ~active_cat, shape = ~type2),
    alpha = 1, size = 5
  ) +
  scale_color_manual(
    values = c("0%" = "#E31A1C", "25%" = "#FB9A99", "50%"= "#FDBF6F", "75%" = "#B2DF8A", "100%" = "#33A02C"),
    drop = FALSE,
    guide = guide_legend(title = "% of active units", title.position = "top")
  ) +
  scale_shape(
    name = "Sector", labels = rte.data:::capitalize
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
