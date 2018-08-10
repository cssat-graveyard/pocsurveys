#' Function for dealing with multiple choice data
#'
#' @param data
#' @param label
#' @param aggregate
#'
#' @return what does this return?
#'
#' @export

multiple_choice <- function(data, label = TRUE, aggregate = TRUE) {

  labels_list <- list()

  for(i in 1:ncol(data)) {

    labels_list[[i]] <- attributes(data[[i]])$redcapLabels

  }

  levels_list <- list()

  for(i in 1:ncol(data)) {

    levels_list[[i]] <- attributes(data[[i]])$redcapLevels

  }

  labels_list_row <- as_data_frame(do.call(rbind, labels_list))
  levels_list_col <- as_data_frame(do.call(cbind, levels_list))

  response_tx <- attributes(data[[1]])$redcapLabels
  response_cd <- attributes(data[[1]])$redcapLevels
  response_tbl <- as_data_frame(cbind(response_tx, response_cd))

  # make data long
  data <- gather(data, item, response_text) %>%
    left_join(response_tbl, by = c("response_text" = "response_tx")) %>%
    rename(response_code = response_cd)

  if(label == TRUE) {

    string_check <- unite(labels_list_row, choices, 1:ncol(labels_list_row), sep = ", ")

    if(length(unique(string_check$choices)) > 1) {

      stop("Error: reponse labels must all be the same")

    }

    response <- response_tx

    count_data <- data %>%
      group_by(item) %>%
      count(response_text) %>%
      ungroup() %>%
      spread(response_text, n, fill = 0)

  } else {

    length_check <- gather(levels_list_col, item, response) %>%
      group_by(item) %>%
      mutate(max_responses = max(response))

    if(length(unique(length_check$max_responses)) > 1) {

      stop("Error: number of reponse levels must all be the same")

      }

    response <- as.character(response_cd)

    count_data <- data %>%
      group_by(item) %>%
      count(response_code) %>%
      ungroup() %>%
      spread(response_code, n, fill = 0)

  }

  # loop to add in dropped responses
  for(i in 1:length(response)) {

    if(!response[i] %in% names(count_data)) {
      count_data[response[i]] <- 0

    }
  }

  if("<NA>" %in% names(count_data)) {
    count_data <- dplyr::select(count_data, response[1:length(response)], `<NA>`)

  } else {

    count_data <- dplyr::select(count_data, response[1:length(response)])

  }

  if(aggregate == TRUE) {

    # for an aggregate, counts of each response choice are given
    count_data

  } else {

     data %>%
      mutate(response_text = factor(response_text, levels = c(response_tx)),
             response_code = factor(response_code, levels = c(response_cd)))

  }

}
