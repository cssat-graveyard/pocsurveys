dropdown_function <- function(data) {

  data <- gather(data, item, response)

  freq <- data %>%
    group_by(item) %>%
    count_by(item)

  colnames(data) <- c("choice", "frequency")
  return(data)
}
