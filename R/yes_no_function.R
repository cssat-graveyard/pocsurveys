#' Function for dealing with yes and no data
#'
#' @param data
#' @param pretty
#' @param useNa
#' @param aggregate
#'
#' @return what does this return?
#'
#' @export

yes_no_function <- function(data, pretty = TRUE, useNA = "ifany", aggregate = TRUE){
  if(any(class(data) == "factor")){
    data <- as.character(data)
  }
  if(pretty == TRUE){
    if(!any(str_detect(data, "Yes|No"), na.rm = TRUE)){
      data <- ifelse(data == 0, "No", data)
      data <- ifelse(data == 1, "Yes", data)
    }
    data <- ifelse(is.na(data), "No Response", data)
    data <- factor(data, levels = c("Yes", "No", "No Response"))
  }
  if(pretty == FALSE){
    if(any(str_detect(data, "Yes|No"), na.rm = TRUE)){
      data <- ifelse(data == "No", 0, data)
      data <- ifelse(data == "Yes", 1, data)
    }
  }
  if(aggregate == TRUE){
    table(data, useNA = useNA)
  }else{
    return(data)
  }
}
