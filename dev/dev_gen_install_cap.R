

#  ------------------------------------------------------------------------
#
# Title : Generation Installed Capacities
#    By : VP
#  Date : 2018-03-27
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )
library( data.table )



# Funs --------------------------------------------------------------------

capitalize <- function(x) {
  lo <- substring(text = x, first = 2)
  up <- substring(text = x, first = 1, last = 1)
  up <- toupper(up)
  lo <- tolower(lo)
  lo <- gsub(pattern = "_", replacement = " ", x = lo)
  paste0(up, lo)
}



# API key -----------------------------------------------------------------

set_key(
  api = "generation_installed_capacities",
  key = "BASE64KEY=="
)




# Datas -------------------------------------------------------------------

gen_inst <- get_open_api(api = "generation_installed_capacities", resource = "capacities_cpc")
str(gen_inst)
gen_inst

# saveRDS(object = gen_inst, file = "dev/gen_inst.rds")


# par dep
gen_inst[department_code != "FR", list(value = sum(value, na.rm = TRUE)), by = list(department_code)][order(-value)]

# par dep et hydro
gen_inst[department_code != "FR" & production_type == "HYDRO",
         list(value = sum(value, na.rm = TRUE)),
         by = list(department_code)][order(-value)]

# type max par dep
gen_inst[department_code != "FR",
         .SD[which.max(value)],
         by = list(department_code)][order(-value)]





# capacities_per_production_unit ------------------------------------------


gen_inst_unit <- get_open_api(api = "generation_installed_capacities", resource = "capacities_per_production_unit", raw = FALSE)
str(gen_inst_unit, max.level = 2)
gen_inst_unit
table(gen_inst_unit$type)

# saveRDS(object = gen_inst_unit, file = "dev/gen_inst_unit.rds")




#  capacities_per_production_type -----------------------------------------

gen_inst_type <- get_open_api(api = "generation_installed_capacities", resource = "capacities_per_production_type", raw = FALSE)
str(gen_inst_type, max.level = 2)
gen_inst_type

# saveRDS(object = gen_inst_type, file = "dev/gen_inst_type.rds")
gen_inst_type <- readRDS("dev/gen_inst_type.rds")

gen_inst_type[type %chin% c("HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_WATER_RESERVOIR"), type := "HYDRO"]
gen_inst_type[type %chin% c("WIND_ONSHORE", "WIND_OFFSHORE"), type := "WIND"]
gen_inst_type <- gen_inst_type[, list(value = sum(value)), by = list(type)]
gen_inst_type <- gen_inst_type[order(value, decreasing = FALSE)]
gen_inst_type[, type := factor(type, levels = type, labels = capitalize(type))]
gen_inst_type

ggplot(data = gen_inst_type) +
  geom_col(aes(x = type, y = value)) +
  coord_flip() + theme_minimal() +
  labs(x = NULL)

ggplot(data = gen_inst_type) +
  geom_segment(aes(x = type, xend = type, y = 0, yend = value), color = "#666666") +
  geom_point(aes(x = type, y = value), color = "#112446", size = 5) +
  coord_flip() + theme_minimal() +
  labs(
    x = NULL, y = "In MW",
    title = "Installed capacity",
    subtitle = "per production type"
  )



