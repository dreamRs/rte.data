

#  ------------------------------------------------------------------------
#
# Title : Comsumption data
#    By : VP
#  Date : 2018-03-01
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )



# API key -----------------------------------------------------------------

# set_key(
#   api = "generation_installed_capacities",
#   key = "BASE64_KEY=="
# )
# set_key(
#   api = "actual_generation",
#   key = "BASE64_KEY=="
# )



# Active units ------------------------------------------------------------

active_units <- retrieve_active_units()
str(active_units)

autoplot(active_units)
autoplot(active_units, interactive = TRUE)


# draw on a map
autoplot(active_units, map = TRUE)
autoplot(active_units, map = TRUE, interactive = TRUE)



# Installed capacities ----------------------------------------------------

inst_cap <- get_open_api(
  api = "generation_installed_capacities",
  resource = "capacities_per_production_unit"
)

autoplot(inst_cap)
autoplot(inst_cap, interactive = TRUE)
