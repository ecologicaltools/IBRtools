#' IBRv2 (Integrated Biological Responses version 2)
#'
#' This function calculates de IBRv2 index proposed by Sanchez et al. 2013.
#'
#' Returns a dataframe with the IBRv2 values for each area/treatment in your input data.
#'
#' Please cite this package when you use it!
#'
#' @param df A data frame containing values the enzymes activities with a reference value on the first rows.
#'
#' @return Returns a dataframe with IBRv2 values in comparison to reference treatment
#'
#' @section Warnings:
#'
#' You must have at least 3 biomarkers and a maximum of 15 biomarkers to perform this index correctly.
#'
#' If you have more than one independent variable, it is necessary to separate the data.frame in subsets so there's only one reference value for each level.
#'
#' @export
#'
#' @examples
#' data(enzact2)
#'
#' ibrv2_index(enzact2)
#'
#' @section References:
#'
#' Sanchez, W., Burgeot, T., & Porcher, J.-M. (2013). A novel “Integrated Biomarker Response” calculation based on reference deviation concept. Environmental Science and Pollution Research, 20(5), 2721–2725. https://doi.org/10.1007/s11356-012-1359-1
#'
#'


ibrv2_index <- function(df) {
  df %>% dplyr::mutate_if(is.character, as.factor) %>%  tidyr::unite("sites", (where(is.factor))) -> df
  z=df
  z$Frequency=rep(1, each=nrow(z))
  z=data.table::data.table(z)
  x=z[,list(a=sum(Frequency)),by=sites][,2]
  my.list = vector("list", nlevels(as.factor(df$sites)))
  for (i in 1:nlevels(as.factor(df$sites))) {
    A=rep(i, each=x$a[i])
    my.list[[i]]=A
  }
  df1 <- data.frame(matrix(unlist(my.list), byrow=T))
  dff=cbind(df1,df)
  dff$matrix.unlist.my.list...byrow...T. <- as.factor(dff$matrix.unlist.my.list...byrow...T.)
  dff %>% dplyr::mutate_if(is.character, as.factor) %>% tidyr::unite("treatment", (where(is.factor))) %>% dplyr::mutate_if(is.character, as.factor) %>%  dplyr::group_by_if(is.factor) %>% dplyr::summarise_all(mean, na.rm = T) -> x
  x1=x[,-1]
  my.list <- vector("list", nrow(x1))
  for(i in 1:nrow(x1)){
   y <- log10(x1[i,]/x1[1,])
   my.list[[i]] = y
  }
  yi = matrix(ncol=ncol(x))
  yi <- rbind(yi[-1,], do.call(rbind, my.list))
  n <- dim(yi)[1]
  one <- rep(1, n)
  yi %>% dplyr::summarise_all(mean) -> med
  as.matrix(med) -> med
  diff <- yi - one %*% med
  yi %>% dplyr::summarise_all(sd) -> s
  as.matrix(s) -> s
  matrixsd <- one %*% s
  zi <- diff/matrixsd
  my.list2 <- vector("list", nrow(zi))
  for(i in 1:nrow(zi)){
    a <- (zi[i,]-zi[1,])
    my.list2[[i]] = a
  }
  ai = matrix(ncol=ncol(x))
  ai <- rbind(ai[-1,], do.call(rbind, my.list2))
  indexvalue <- apply(abs(ai), 1, sum)
  xvar <-  as.matrix(x[,1])
  as.data.frame(cbind(xvar, indexvalue)) -> indexf
  indexf %>% tidyr::separate(treatment, into = c("num", "group"), sep = "_", extra = "drop") -> indexf1
  indexf1[,-1] -> indexf1
  indexf1$indexvalue <- as.numeric(indexvalue)
  return(indexf1)

}

