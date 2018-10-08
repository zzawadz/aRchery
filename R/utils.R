split_by <- function(data, vars) {
  data <- seplyr::group_by_se(data, vars)
  splData <- tidyr::nest(data)
  splData
}

# get names for variables based on the split
get_name_for_splitted <- function(splData) {
  apply(splData[, -ncol(splData)], 1, function(x)
    paste(unlist(x), collapse = " - "))
}

get_target_type <- function(data) {
  
  target <- unique(data[["Target"]])
  if(length(target) != 1) stop("Multiple targets in the data.")
  
  ARCHERY_TARGETS[[ARCHERY_MAP[[target]]]]
}

select_by_n_days <- function(
  data, n = 7, startDate = c("last", "first"), backwards = TRUE,
  fromLast = TRUE, includeEdge = TRUE) {
  
  days <- select_by_n_days_inner(
    data[["Date"]], startDate = startDate, 
    backwards = backwards, n = n,
    fromLast = fromLast, includeEdge = includeEdge)
  
  data[data[["Date"]] %in% days,]
}

select_by_n_days_inner <- function(
  datesAll, startDate = c("last", "first"), backwards = TRUE, n = 7,
  fromLast = TRUE, includeEdge = TRUE) {
  
  startDate <- as.character(startDate[1])
  datesAll  <- unique(datesAll)
  
  startDate <- if(startDate == "last") tail(datesAll, 1) else
  if(startDate == "first") head(datesAll, 1) else
    as.Date(startDate)
  
  diffDays <- as.numeric(diff(range(datesAll)))
  
  op <- if(backwards) `-` else `+`
  
  times <- ceiling(diffDays / n)
  
  finalDays <- op(startDate, (0:times * n))
  if(backwards) {
    finalDays <- c(datesAll[1], finalDays)
  } else {
    finalDays <- c(datesAll[length(datesAll)], finalDays)
  }
  
  finalDays <- sort(unique(finalDays))
  finalDays
}


#####
add_group_by_n <- function(data, n = 12) {
  
  dt <- data %>% 
    seplyr::group_by_se("Date") %>% 
    seplyr::arrange_se(c("Date", "End"))
  
  add_idx <- function(x) {
    nn <- ceiling(length(x) / n)
    sort(rep(seq_len(nn), n))
  }
  
  dt <- dt %>% dplyr::mutate(Groups = add_idx(Date))
  dt
}
