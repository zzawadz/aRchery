#' Read multiple files containing archery data.
#'
#' @param files a vector of csv to read.
#'
#' @return data.frame
#' 
#' @export
#' 
read_archery_files <- function(files) {
  allFiles <- suppressMessages(lapply(files, function(x) readr::read_delim(x, delim = ";")))
  allData  <- dplyr::bind_rows(allFiles)
  allData  <- seplyr::arrange_se(allData, c("Date", "End"))
  
  points <- allData[["Points"]]
  
  points <- dplyr::case_when(points == "X" ~ "10",
                   points == "M" ~ "0",
                   TRUE ~ points)
  points <- as.numeric(points)
  allData[["RawScore"]] <- points
  allData
}
