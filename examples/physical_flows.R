

#  ------------------------------------------------------------------------
#
# Title : Physical flows
#    By : VP
#  Date : 2018-03-01
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )



# API key -----------------------------------------------------------------

# set_key(
#   api = "physical_flow",
#   key = "BASE64_KEY=="
# )




# Physical flows ----------------------------------------------------------

physfl <- get_open_api(api = "physical_flow", resource = "physical_flows")
physfl

physfl_a <- physfl[, list(value = sum(value)), by = list(from = sender_country_name, to = receiver_country_name)]
physfl_a

physfl_a[, type := ifelse(to == "France", "Imports", "Exports")]
physfl_a[, country := ifelse(to == "France", from, to)]
physfl_a[type == "Imports", value := -value]
physfl_a

ggplot(data = physfl_a) +
  geom_col(aes(x = country, y = value, fill = type))


