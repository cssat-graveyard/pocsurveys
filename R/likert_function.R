#' Function for dealing with likert data
#'
#' @param data add info
#' @param aggregate add info
#' @param metadata add info
#'
#' @return what does this return?
#'
#' @export

likert_function <- function(data, aggregate = TRUE, metadata = NULL){

  data <- gather(data, item, response)

  if(aggregate == TRUE){

    data <- data %>%
      group_by(item) %>%
      count(response) %>%
      ungroup() %>%
      spread(response, n, fill = 0)

    if(!'Strongly Agree' %in% names(data)) {
      data <- mutate(data, `Strongly Agree` = 0)
    }
    if(!'Agree' %in% names(data)) {
      data <- mutate(data, Agree = 0)
    }
    if(!'Neither agree nor disagree' %in% names(data)) {
      data <- mutate(data, `Neither agree nor disagree` = 0)
    }
    if(!'Disagree' %in% names(data)) {
      data <- mutate(data, Disagree = 0)
    }
    if(!'Strongly Disagree' %in% names(data)) {
      data <- mutate(data, `Strongly Disagree` = 0)
    }

    data <- select(data, item, `Strongly Disagree`, Disagree, `Neither agree nor disagree`, Agree, `Strongly Agree`)

  } else {
    data <- data %>%
      mutate(response = factor(response,
                               levels = c("Strongly Disagree", "Disagree", "Neither agree nor disagree", "Agree", "Strongly Agree"))) %>%
      arrange(item, response)
  }

  if(!is.null(metadata)){

    metadata %>%
      dplyr::select(field_name, field_label) %>%
      right_join(data, by = c("field_name" = "item")) %>%
      dplyr::select(-field_name) %>%
      rename(item = field_label)

  } else {
    return(data)
  }
}
