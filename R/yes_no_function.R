#'
#'
#'
#'

yes_no_function <- function(data, pretty = TRUE, useNA = "ifany", aggregate = TRUE){

  if(pretty == TRUE){

    if(any(class(dat1$cwtap_education) == "factor")){
      data <- as.character(data)
    }

    if(any(str_detect(data, "Yes|No"), na.rm = TRUE) == FALSE){
      data <- ifelse(data == 0, "No", data)
      data <- ifelse(data == 1, "Yes", data)
    }
    data <- ifelse(is.na(data), "No Response", data)
    data <- factor(data, levels = c("Yes", "No", "No Response"))
  }

  if(aggregate == TRUE){
    table(data, useNA = useNA)
  }else{
    return(data)
  }

}
