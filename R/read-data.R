#' Read multiple files containing archery data.
#'
#' @param files a vector of csv to read.
#'
#' @return data.frame
#' 
#' @export
#' 
read_archery_files <- function(files) {
  allFiles <- lapply(files, function(x) readr::read_delim(x, delim = ";"))
  allData  <- dplyr::bind_rows(allFiles)
  allData  <- seplyr::arrange_se(allData, c("Date", "End"))
  allData
}
