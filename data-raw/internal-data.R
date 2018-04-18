
# Internal data

source("data-raw/eic_code.R")
source("data-raw/map-data.R")
source("data-raw/plants-locations.R")

usethis::use_data(fra_dept, europe, pplocations, code_eic, overwrite = TRUE, internal = TRUE)

