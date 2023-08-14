#' IBR (Integrated Biomarker Response) index
#'
#' This function calculates de Integrated Biomarker Response index proposed by Beliaeff and Burgeot, 2002 and revisited by Devin et al. 2014.
#'
#' Gives a list of two dataframes, the first data frame is IBR_total and it can be used for statistical analysis (such as ANOVA, t-test) to verify the differences between each level of the independent variable(s). The second data frame is IBR_mean_sd, it gives the mean values and standard deviation for each independent variable, which is usually presented next to a radarplot.
#'
#'
#' @param df The data frame output of the function ibr_std of this package.
#'
#' @return Returns a list with two dataframes, the IBR index value per treatment and its mean and standard deviation
#'
#' @section Warnings:
#'
#' You must have at least 3 biomarkers and a maximum of 9 biomarkers to perform this index correctly.
#'
#' @export
#'
#' @examples
#' data(enzact)
#'
#' outstd<- ibr_std(enzact)
#'
#' ibr_index(outstd)
#'
#'
#'
#' @section References:
#'
#'Beliaeff, B., & Burgeot, T. (2002). Integrated biomarker response: A useful tool for ecological risk assessment. Environmental Toxicology and Chemistry, 21(6), 1316–1322.
#'
#'Devin, S., Burgeot, T., Giambérini, L., Minguez, L., & Pain-Devin, S. (2014). The integrated biomarker response revisited: Optimization to avoid misuse. Environmental Science and Pollution Research, 21(4), 2448–2454. https://doi.org/10.1007/s11356-013-2169-9
#'


ibr_index <- function(df) {
  df %>% remove_rownames %>% tibble::column_to_rownames(var="group")-> S
  t(S) -> ts
  as.data.frame(ts) -> ts
  ts %>%  tibble::rownames_to_column( var = "trat" ) -> ts
  result <- gtools::permutations(n = nrow(ts), r = nrow(ts), repeats.allowed = F)
  shift.to.minimum <- function(row) {
    binhf::shift(row, which(row==min(row))-1, dir='left')
  }
  result <- unique(t(apply(result, 1, shift.to.minimum)))
  dat3 <- do.call(rbind, lapply((1:nrow(result)), function(x){
    ts_temp <- ts[result[x, ],]
    return(ts_temp)
  }))
  ts2=dat3
  x=ts2[,-1]
  my.list <- vector("list", nrow(x))
  for (i in 1:(nrow(x))){
    if(i != nrow(ts2)){
      k <- nrow(ts)
      x[i,]*(x[i+1,])*(sin((2*pi)/k)/2) -> value
      my.list[[i]] = value
    } else {value=tail(x,n=1)*(x[1,])*(sin((2*pi)/k)/2)
    my.list[[i]] = value
    }
  }
  solution = matrix(ncol=ncol(x))
  solution <- rbind(solution[-1,], do.call(rbind, my.list))
  solution$group=rep(make.names(letters[1:(nrow(solution)/nrow(ts))],unique=T), each=nrow(ts))
  ibr <- solution %>% dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::group_by_if(is.factor) %>% dplyr::summarise_all(sum)
  ibr[,-1] -> ibr
  ibr_final <- ibr %>% tidyr::gather("treatment") %>%  dplyr::mutate_if(is.character, as.factor)
  as.data.frame(ibr_final)-> ibr_final
  ibr_mean <- ibr_final %>% dplyr::group_by(treatment) %>%  dplyr::summarise_all(mean)
  ibr_mean <- dplyr::rename(ibr_mean, `ibr_mean` = `value`)
  ibr_sd <- ibr_final %>% group_by(treatment) %>%  summarise_all(sd)
  ibr_sd <- dplyr::rename(ibr_sd, `ibr_sd` = `value`)
  ibr_sd[,2] -> ibrsd
  cbind(ibr_mean, ibrsd) -> final_ibr
  return(list(IBR_total = ibr_final, IBR_mean_sd = final_ibr))
}
