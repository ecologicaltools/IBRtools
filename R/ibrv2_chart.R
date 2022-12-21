#' IBRv2 Radarchart
#'
#' Plots the IBRv2 standardized values for the enzymes to be compared with one another.
#'
#' @section Tips:
#'
#' For this index chart, it is necessary to plot each area/treatment with the reference value separately, therefore, make sure you subset the data.frame and perform this function for each subset.
#'
#' Also, you will find that the axis values are not written automatically, we suggest that our users do it manually because we couldn't yet provide an automated way to plot the zero in the correct position on the radarchart. This will be revised for next versions of this package. This can be done by changing values for seg, axistype = 1, and caxislabels.
#'
#' Check the examples for further insight.
#'
#'
#' @param df A data.frame that resulted from the function ibrv2_std
#'
#' @param legend Default is NULL, when any other value is given the legend will not appear and you can manually create your own using the legend() function right after building your radarchart
#'
#' @return Returns a radarchart with the standardized values of biomarkers in comparison to reference treatment
#'
#' @inheritParams fmsb::radarchart
#'
#' @export
#'
#' @examples
#' data(enzact2)
#'
#' ibrv2_bdi(enzact2) -> enzact2_std
#'
#' # subsetting to compare one area with the reference value
#'
#' enzact2_std[c(1,2),] -> sub1_enzact2_std
#'
#' ibrv2_chart(sub1_enzact2_std)
#'
#' ibrv2_chart(sub1_enzact2_std, seg =8, caxislabels = seq(-1,1.9,0.34), axistype = 1)
#'
#' @section References:
#'
#' Sanchez, W., Burgeot, T., & Porcher, J.-M. (2013). A novel “Integrated Biomarker Response” calculation based on reference deviation concept. Environmental Science and Pollution Research, 20(5), 2721–2725. https://doi.org/10.1007/s11356-012-1359-1
#'
#'  Minato Nakazawa (2022). fmsb: Functions for Medical Statistics Book with some Demographic Data. R package version 0.7.0. https://CRAN.R-project.org/package=fmsb
#'

ibrv2_chart<- function(df, axistype, pcol, pfcol, plwd, plty, cglcol, cglty, axislabcol, cglwd, caxislabels, seg, legend = NULL, ...) {
  df %>% remove_rownames %>% tibble::column_to_rownames(var = "group") %>% round(digits = 1) -> df
  max(df)-> max
  min(df) -> min
  rbind(max,min,df) -> chart
  as.data.frame(chart)-> chart
  colors_border <- c("#000000", "#CC0000")
  if(missing(axistype)) {axistype = 0}
  if(missing(pcol)) {pcol = colors_border}
  if(missing(plwd)) {plwd = 2}
  if(missing(plty)) {plty = c(2,1)}
  if(missing(cglcol)) {cglcol = "grey"}
  if(missing(cglty)) {cglty = 1}
  if(missing(axislabcol)) {axislabcol = "black"}
  if(missing(cglwd)) {cglwd = 0.8}
  if(missing(caxislabels)) {caxislabels = seq(min, max+0.5, ((max)/5))}
  if(missing(seg)) {seg =8}
  fmsb::radarchart(chart,
                   axistype = axistype,
                   pcol= pcol,
                   plwd= plwd,
                   plty= plty,
                   cglcol= cglcol,
                   cglty=1,
                   axislabcol= axislabcol,
                   caxislabels= caxislabels,
                   cglwd=cglwd,
                   vlcex=0.8,
                   centerzero=T,
                   seg = seg,
                   ...
  )
  legend(x=1.2, y=-0.3, legend = rownames(chart[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "black", cex=0.9, pt.cex=2)
}
