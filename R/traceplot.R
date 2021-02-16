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
    mutate(m = as.factor(m)) %>%
    filter(!is.na(value)) %>%
    ggplot(aes(x = iteration, y = value, group = m, color= m)) +
    geom_line() +
    geom_point() +
    facet_wrap(variable ~ statistic , scales = "free", ncol = 2) +
    theme_bw()
}


