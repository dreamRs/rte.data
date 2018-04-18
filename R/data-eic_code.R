#' EIC code table
#'
#' This data comes from RTE, see :
#' \url{https://clients.rte-france.com}.
#'
#' @format A data.table with 158 rows and 6 variables:
#' \describe{
#' \item{code_eic}{EIC code}
#' \item{nom_affiche}{Id of the plant}
#' \item{nom_entite}{Name}
#' \item{eic_parent}{Parent EIC code }
#' \item{eic_responsible_party}{Eesponsible entity EIC code}
#' \item{pays}{Country}
#' \item{fonction}{Type of unit}
#' }
#' @examples
#' code_eic
"code_eic"
