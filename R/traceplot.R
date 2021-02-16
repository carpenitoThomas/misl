#' Creates a traceplot using the imputations from MISL
#'
#' @param imputations Results of the MISL imputations
#'
#' @return
#' @export
#'
#' @examples
traceplot <- function(imputations){
  long_dataframe <- data.frame()
  for(mdatasets in imputations){
    long_dataframe <- rbind(long_dataframe, mdatasets$trace)
  }

  long_dataframe %>%
    dplyr::mutate(m = as.factor(m)) %>%
    dplyr::filter(!is.na(value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = iteration, y = value, group = m, color= m)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(variable ~ statistic , scales = "free", ncol = 2) +
    ggplot2::theme_bw()
}


