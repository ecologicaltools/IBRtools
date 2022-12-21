#' 'IBRtools': Integrating Biomarker-Based Assessments and Radarchart Creation
#'
#' Several functions to calculate two important indexes (IBR (Integrated Biomarker Response) and IBRv2 (Integrated Biological Response version 2)), it also calculates the standardized values for enzyme activity for each index, and it has a graphing function to perform radarplots that make great data visualization for this type of data.
#'
#' It comes with 3 example datasets: enzact, enzact_coef and enzact2
#'
#' You can use them to practice or to see how the functions work so you can apply in your own datasets!
#'
#' To load them, just use the command: data(nameofdataset)
#'
#'
#' @section IBRtools functions:
#' There are 6 functions in this package, 3 for each index.
#'
#' IBR: ibr_index, ibr_std, ibr_chart
#'
#' IBRv2: ibrv2_index, ibrv2_bdi, ibrv2_chart
#'
#' @section Authors:
#'Author, Maintainer: Anna Carolina Resende
#'
#'\email{annac.resende@gmail.com}
#'
#'\url{https://orcid.org/0000-0003-0909-1255}
#'
#'Author: Diego Mauro Carneiro Pereira
#'
#'\email{diegom8135@gmail.com}
#'
#'\url{https://orcid.org/0000-0003-1554-3684}
#'
#' @references
#' Beliaeff, B., & Burgeot, T. (2002). Integrated biomarker response: A useful tool for ecological risk assessment. Environmental Toxicology and Chemistry, 21(6), 1316–1322.
#'
#' Devin, S., Burgeot, T., Giambérini, L., Minguez, L., & Pain-Devin, S. (2014). The integrated biomarker response revisited: Optimization to avoid misuse. Environmental Science and Pollution Research, 21(4), 2448–2454. https://doi.org/10.1007/s11356-013-2169-9
#'
#' Sanchez, W., Burgeot, T., & Porcher, J.-M. (2013). A novel “Integrated Biomarker Response” calculation based on reference deviation concept. Environmental Science and Pollution Research, 20(5), 2721–2725. https://doi.org/10.1007/s11356-012-1359-1
#'
#'
#' @docType package
#' @name IBRtools
#' @importFrom dplyr %>%
#' @importFrom dplyr select_if
#' @importFrom dplyr mutate_if
#' @importFrom dplyr group_by_if
#' @importFrom dplyr summarise_all
#' @importFrom dplyr group_by
#' @importFrom dplyr rename
#' @import tibble
#' @importFrom data.table data.table
#' @importFrom grDevices rgb
#' @importFrom stats sd
#' @importFrom utils tail
#' @importFrom tidyselect where

if(getRversion() >= "3.5.0")  utils::globalVariables(c("Frequency", "sites", "treatment"))
NULL


