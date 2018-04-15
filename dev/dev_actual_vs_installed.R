

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

