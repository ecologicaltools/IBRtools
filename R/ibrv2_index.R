#' IBRv2 (Integrated Biological Responses version 2)
#'
#' This function calculates de IBRv2 index purposed by Sanchez et al. 2013.
#' 
#' Returns a dataframe with the IBRv2 values for each area/treatment in your input data.
#' 
#' Please cite this package when you use it!
#' 
#' @param df A data frame containing values the enzymes activities with a reference value on the first rows.
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


ibrv2_index <- function(df, na.rm = TRUE, ...) { 
  df %>% dplyr::mutate_if(is.character, as.factor) %>% tidyr::unite("treatment", (where(is.factor))) %>% dplyr::mutate_if(is.character, as.factor) %>%  dplyr::group_by_if(is.factor) %>% dplyr::summarise_all(mean, na.rm = T) -> x
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
  indexf$indexvalue <- as.numeric(indexvalue)
  return(indexf)
  
}




