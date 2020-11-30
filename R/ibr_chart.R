#' IBR Radarchart
#'
#' Plots the IBR standardized values for the enzymes to be compared with one another.
#'
#'
#' @param df A data.frame that resulted from the function ibr_std
#' @inheritParams fmsb::radarchart
#'
#' @export
#'
#'
#' @examples
#'
#' data(enzact)
#' ibr_std(enzact) -> enzact_chart
#' ibr_chart(enzact_chart)
#'
#' @section References:
#'
#'Beliaeff, B., & Burgeot, T. (2002). Integrated biomarker response: A useful tool for ecological risk assessment. Environmental Toxicology and Chemistry, 21(6), 1316–1322.
#'
#'Devin, S., Burgeot, T., Giambérini, L., Minguez, L., & Pain-Devin, S. (2014). The integrated biomarker response revisited: Optimization to avoid misuse. Environmental Science and Pollution Research, 21(4), 2448–2454. https://doi.org/10.1007/s11356-013-2169-9
#'
#'  Minato Nakazawa (2019). fmsb: Functions for Medical Statistics Book with some Demographic Data. R package version 0.7.0. https://CRAN.R-project.org/package=fmsb
#'
#'

ibr_chart<- function(df, axistype, pcol, pfcol, plwd, plty, cglco, cglty, axislabcol, cglwd, caxislabels, ...) {
  df %>% remove_rownames %>% tibble::column_to_rownames(var = "group") -> df
  max(df)-> max
  min(df) -> min
  rbind(max,min,df) -> chart
  colors_border <- c( rgb(1,0.4,0.8,0.7), rgb(0,0.6,0.6,0.7) , rgb(0.4,0.4,0.6,0.7), rgb(0,0.4,0.4,0.7))
  colors_in <- c( rgb(1,0.4,0.8,0.25), rgb(0,0.6,0.6,0.25) , rgb(0.4,0.4,0.6,0.25), rgb(0,0.4,0.4,0.25) )
  if(missing(axistype)) {axistype = 1}
  if(missing(pcol)) {pcol = colors_border}
  if(missing(pfcol)) {pfcol = colors_in}
  if(missing(plwd)) {plwd = 2}
  if(missing(plty)) {plty = 1}
  if(missing(cglco)) {cglco = "grey"}
  if(missing(cglty)) {cglty = 1}
  if(missing(axislabcol)) {axislabcol = "black"}
  if(missing(cglwd)) {cglwd = 0.8}
  if(missing(caxislabels)) {caxislabels = 7}
  fmsb::radarchart(chart,
                   axistype = axistype,
                   pcol= pcol,
                   pfcol= pfcol,
                   plwd= plwd,
                   plty= plty,
                   cglcol= cglco,
                   cglty=1,
                   axislabcol= axislabcol,
                   caxislabels=seq(min(df),max(df), signif((max(df)/5), digits =2)),
                   cglwd=cglwd,
                   vlcex=0.8, ...
  )
  legend(x=1.2, y=-0.3, legend = rownames(chart[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=0.9, pt.cex=2)

}

