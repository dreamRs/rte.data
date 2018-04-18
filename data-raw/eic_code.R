

# EIC code correspondance -------------------------------------------------

library("data.table")
library("readxl")


download.file(
  url = "https://clients.rte-france.com/servlets/CodeEICGServlet?type=W",
  destfile = "data-raw/CodeEIC.xls", mode = "wb"
)
code_eic <- read_xls(path = "data-raw/CodeEIC.xls", skip = 2)
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
# code_eic <- as.data.frame(code_eic)

usethis::use_data(code_eic, internal = FALSE, overwrite = TRUE)





