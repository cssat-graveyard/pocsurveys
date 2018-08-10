#' Function for dealing with dropdown data
#'
#' @param data
#'
#' @return what does this return?
#'
#' @export

dropdown_function <- function(data) {

  data <- gather(data, item, response)

  freq <- data %>%
    group_by(item) %>%
    count_by(item)

  colnames(data) <- c("choice", "frequency")
  return(data)
}
