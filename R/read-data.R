#' Read multiple files containing archery data.
#'
#' @param files a vector of csv to read.
#'
#' @return data.frame
#' 
#' @export
#' 
read_archery_files <- function(files) {
  
  
  
  allFiles <- suppressWarnings(suppressMessages(lapply(files, function(x) 
    {
      firstLine <- readLines(x, n = 1)
      sep <- if(length(strsplit(firstLine, split = ",")[[1]]) > 2) "," else ";"
      
      res <- readr::read_delim(x, delim = sep)
      date <- res[["Date"]]
      
      date <- lubridate::ymd(date)
      if(all(is.na(date))) {
        date <- lubridate::dmy(res[["Date"]])
      }
      res[["Date"]] <- date
      res
    }
  )))
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
