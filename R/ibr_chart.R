#' IBR Radarchart
#'
#' Plots the IBR standardized values for the enzymes to be compared with one another.
#'
#'
#' @param df A data.frame that resulted from the function ibr_std
#'
#' @param legend Default is NULL, when any other value is given the legend will not appear and you can manually create your own using the legend() function right after building your radarchart
#'
#' @return Returns a radarchart with the standardized values of biomarkers
#'
#' @inheritParams fmsb::radarchart
#'
#' @export
#' @section Tips:
#'
#' If you have any problems with the axis labels, you can do it manually by changing two params in the function: seg and caxislabel.
#' The caxislabel param has to be a sequence, such as: caxislabels = seq(-1,1.9,0.34), each number is: seq(minvalue, maxvalue, breakvalue)
#'
#' The seg param can be any number starting from 3, you can change it with: seg = 6.
#'
#' If you still can't solve the problem, try rounding your standardized values with the function round()
#'
#' @examples
#'
#' data(enzact)
#'
#' ibr_std(enzact) -> enzact_chart
#'
#' ibr_chart(enzact_chart, legend = FALSE)
#'
#' colorvector<- c(rgb(1,0.4,0.8,0.7), rgb(0,0.6,0.6,0.7) , rgb(0.4,0.4,0.6,0.7), rgb(0,0.4,0.4,0.7))
#'
#' legend(x=1.2, y=-0.3, enzact_chart$group, bty = "n", pch=20, col=colorvector, cex=0.9, pt.cex=2)
#'
#' @section References:
#'
#'Beliaeff, B., & Burgeot, T. (2002). Integrated biomarker response: A useful tool for ecological risk assessment. Environmental Toxicology and Chemistry, 21(6), 1316–1322.
#'
#'Devin, S., Burgeot, T., Giambérini, L., Minguez, L., & Pain-Devin, S. (2014). The integrated biomarker response revisited: Optimization to avoid misuse. Environmental Science and Pollution Research, 21(4), 2448–2454. https://doi.org/10.1007/s11356-013-2169-9
#'
#'  Minato Nakazawa (2022). fmsb: Functions for Medical Statistics Book with some Demographic Data. R package version 0.7.0. https://CRAN.R-project.org/package=fmsb
#'
#'

ibr_chart<- function(df, axistype, pcol, pfcol, plwd, plty, cglcol, cglty, axislabcol, cglwd, caxislabels, seg, legend = NULL, ...) {
  df %>% remove_rownames %>% tibble::column_to_rownames(var = "group") %>% round(digits = 1) -> df
  max(df)-> max
  min(df) -> min
  rbind(max,min,df) -> chart
  as.data.frame(chart)-> chart
  colors_border <- c(rgb(1,0.4,0.8,0.7), rgb(0,0.6,0.6,0.7) , rgb(0.4,0.4,0.6,0.7), rgb(0,0.4,0.4,0.7))
  if(missing(axistype)) {axistype = 1}
  if(missing(pcol)) {pcol = colors_border}
  if(missing(plwd)) {plwd = 2}
  if(missing(plty)) {plty = 1}
  if(missing(cglcol)) {cglcol = "grey"}
  if(missing(cglty)) {cglty = 1}
  if(missing(axislabcol)) {axislabcol = "black"}
  if(missing(cglwd)) {cglwd = 0.8}
  if(missing(caxislabels)) {caxislabels=seq(min(df),max(df), round((max(df)/5), digits =1))}
  if(missing(seg)) {seg =4}
  fmsb::radarchart(chart,
                   axistype = axistype,
                   pcol= pcol,
                   plwd= plwd,
                   plty= plty,
                   cglcol= cglcol,
                   cglty=1,
                   axislabcol= axislabcol,
                   caxislabels=caxislabels,
                   cglwd=cglwd,
                   centerzero=T,
                   vlcex=0.8,
                   seg = seg, ...)
  if(is.null(legend)) {legend = legend(x=1.2, y=-0.3, legend = rownames(chart[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=0.9, pt.cex=2)}

}
