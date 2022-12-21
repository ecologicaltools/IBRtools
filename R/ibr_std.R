#' Standardized values for IBR (Integrated Biomarker Response) index
#'
#' Returns a data frame with standardized values of IBR useful to do a radar chart. This data is fit for our function ibr_chart() from this package that creates this type of plot.
#'
#'
#' @param df A data frame containing values the enzymes activities with at least two levels of one independent variable.
#' @param z A data frame with the Z coefficient for each enzyme at each level. If not provided, all z values will be 1.
#'
#' @return Returns a dataframe with standardized values for each biomarker per treatment
#'
#' @section Warnings:
#'
#' You must have at least 3 biomarkers and a maximum of 9 biomarkers to perform this index correctly.
#'
#' @export
#'
#' @examples
#' data(enzact)
#' ibr_std(enzact, enzact_coef)
#'
#' @section References:
#'
#'Beliaeff, B., & Burgeot, T. (2002). Integrated biomarker response: A useful tool for ecological risk assessment. Environmental Toxicology and Chemistry, 21(6), 1316–1322.
#'
#'Devin, S., Burgeot, T., Giambérini, L., Minguez, L., & Pain-Devin, S. (2014). The integrated biomarker response revisited: Optimization to avoid misuse. Environmental Science and Pollution Research, 21(4), 2448–2454. https://doi.org/10.1007/s11356-013-2169-9
#'

ibr_std <- function(df, z) {
  df %>% dplyr::mutate_if(is.character, as.factor) %>% tidyr::unite("treatment", (where(is.factor))) %>% dplyr::mutate_if(is.character, as.factor) %>%  dplyr::group_by_if(is.factor) %>% dplyr::summarise_all(mean, na.rm = T, .groups = "drop") -> y
  x <- as.data.frame(dplyr::select_if(y, is.numeric))
  n <- dim(x)[1]
  one <- rep(1, n)
  x %>% dplyr::summarise_all(mean) -> med
  as.matrix(med) -> med
  diff <- x - one %*% med
  x %>% dplyr::summarise_all(sd) -> s
  as.matrix(s) -> s
  matrixsd <- one %*% s
  y1 <- diff/matrixsd
  if(missing(z)) {
    matrix(1, nrow = nrow(x), ncol = ncol(x)) -> z1}
  else {z1 <- as.matrix(dplyr::select_if(z, is.numeric))}
  z2 <- z1 * y1
  as.matrix(apply(z2, 2, FUN=min, na.rm= TRUE)) -> min
  t(min) -> min
  matrixmin <- one %*% min
  S <- as.data.frame(z2 + abs(matrixmin))
  as.matrix(dplyr::select_if(y, is.factor, as.character)) -> u
  rownames(S) <- u
  S %>% tibble::rownames_to_column(var = "group") -> S
  return(S)
}

