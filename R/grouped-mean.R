#' Create list with data grouped by N-shots for a given day
#'
#' @param data archery data for only one day 
#' @param n size of the group
#'
#' @return
#' @export
#' 
ar_make_mean_grouped_data <- function(data, n = 6) {
  
  if(length(unique(data[["Date"]])) != 1) {
    "Only one day can be selected!"
  }
  
  gdt <- add_group_by_n(data, n = n)
  
  summaries <- gdt %>% 
    group_by(Target, Groups) %>% 
    summarise(x = mean(x), y = mean(y))
  
  splData <- split_by(gdt, "Groups")
  rawPoints <- splData[["data"]]
  
  list(summaries = summaries, rawPoints = rawPoints)
}
