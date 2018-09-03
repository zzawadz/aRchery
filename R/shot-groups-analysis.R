#' Summarise data for functional boxplot.
#'
#' @param data 
#' @param n 
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' 
#' data(archeryData)
#' dtMatrix12 <- fnc_by_n_shots(archeryData, 12)
#' 
fnc_by_n_shots <- function(data, n = 12) {

  dt <- data %>% 
    seplyr::group_by_se("Date") %>% 
    seplyr::arrange_se(c("Date", "End"))
  
  add_idx <- function(x) {
    nn <- ceiling(length(x) / n)
    sort(rep(seq_len(nn), n))
  }
  
  dt <- dt %>% dplyr::mutate(Groups = add_idx(Date))
  
  dt <- dt %>% 
    seplyr::group_by_se(c("Date", "Groups")) %>%
    dplyr::summarise(Scores = sum(RawScore))
  fnc_data2matrix(dt)  
}

fnc_data2matrix <- function(dt) {
  dtMatrix <- dt %>% seplyr::spread_se(key = "Groups", value = "Scores")
  dtMatrix <- dtMatrix %>% dplyr::ungroup() %>%
    seplyr::select_se("-Date") %>% as.matrix()
  dtMatrix
}

#' Create an functional boxplot for showing the distribution of results in single training session.
#'
#' @param dtMatrix data matrix. It should be a result of the \code{fnc_by_n_shots} or similar function.
#' @param bands a vector with bands.
#'
#' @export
#'
#' @examples
#' 
#' data(archeryData)
#' 
#' dtMatrix12 <- fnc_by_n_shots(archeryData, 12)
#' plot_fnc_boxplot(dtMatrix12)
#' 
#' dtMatrix6 <- fnc_by_n_shots(archeryData, 6)
#' plot_fnc_boxplot(dtMatrix6)
#' 
#' dtMatrix3 <- fnc_by_n_shots(archeryData, 3)
#' plot_fnc_boxplot(dtMatrix3)
#' 
plot_fnc_boxplot <- function(dtMatrix, bands = c(0, 0.5, 0.75, 1)) {
  DepthProc::fncBoxPlot(dtMatrix, bands = bands)
}
