

#  ------------------------------------------------------------------------
#
# Title : Physical flows
#    By : VP
#  Date : 2018-03-01
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )




# API key -----------------------------------------------------------------

# set_key(
#   api = "physical_flow",
#   key = "BASE64_KEY=="
# )




# Physical flows ----------------------------------------------------------


# commercial balance for France on electricity market
balance <- get_physical_flows(start_date = "2018-02-01", end_date = "2018-03-15")
autoplot(balance)


# Exchange by countries
autoplot(balance, by_country = TRUE)








