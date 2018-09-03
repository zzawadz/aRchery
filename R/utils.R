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