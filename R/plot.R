#' Plot the trace lines of the MISL algorithm
#'
#' Creates a traceplot using the imputations from MISL to view convergence. Of importance, these plots to not display categorical variables (yet).
#'
#' @param imputations Results of the MISL imputations
#'
#' @return
#' @export
#'
#' @examples
#' # First generate the imputations
#' misl_imp <- misl(nhanes, m = 1)
#'
#' # Then you can generate the associated traceplot to investiga
#' plot(misl_imp)
plot <- function(imputations){
  long_dataframe <- data.frame()
  for(mdatasets in imputations){
    long_dataframe <- rbind(long_dataframe, mdatasets$trace)
  }

  long_dataframe <- long_dataframe %>%
    filter(complete.cases(.))

  num_pages <- ceiling(length(unique(long_dataframe$variable)) / 3)

  for(page_num in seq_len(num_pages)){
      print(
        long_dataframe %>%
          dplyr::mutate(m = as.factor(m)) %>%
          dplyr::filter(!is.na(value)) %>%
            ggplot2::ggplot(ggplot2::aes(x = iteration, y = value, group = m, color= m)) +
            ggplot2::geom_line() +
            ggplot2::geom_point() +
            ggforce::facet_wrap_paginate(variable ~ statistic , scales = "free", ncol = 2, nrow = 3, page = page_num) +
            ggplot2::theme_bw()
      )
    }
}

