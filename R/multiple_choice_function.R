#' Function for dealing with multiple choice data
#'
#' @param data add info
#' @param label add info
#' @param aggregate add info
#'
#' @return what does this return?
#'
#' @export

# function: label = TRUE for response labels; label = FALSE for response numbers
multiple_choice_function <- function(data, label = TRUE, aggregate = TRUE) {

  labels_list <- list()

  # extract redcap labels of response options
  for(i in 1:ncol(data)) {

    labels_list[[i]] <- attributes(data[[i]])$redcapLabels

  }

  levels_list <- list()

  # extract redcap levels (numbers) of response options
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

    # check to ensure that response labels are all the same across items
    string_check <- unite(labels_list_row, choices, 1:ncol(labels_list_row), sep = ", ")

    if(length(unique(string_check$choices)) > 1) {

      stop("Error: response labels must all be the same")

    }

    response <- response_tx

    count_data <- data %>%
      group_by(item) %>%
      count(response_text) %>%
      ungroup() %>%
      spread(response_text, n, fill = 0)

  } else {

    # check to ensure that the number of response levels are all the same across items
    length_check <- gather(levels_list_col, item, response) %>%
      group_by(item) %>%
      mutate(max_responses = max(response))

    if(length(unique(length_check$max_responses)) > 1) {

      stop("Error: number of response levels must all be the same")

    }

    response <- as.character(response_cd)

    count_data <- data %>%
      group_by(item) %>%
      count(response_code) %>%
      ungroup() %>%
      spread(response_code, n, fill = 0)

  }

  # loop to add back in dropped/unused responses
  for(i in 1:length(response)) {

    if(!response[i] %in% names(count_data)) {
      count_data[response[i]] <- 0

    }
  }

  # subset data
  if("<NA>" %in% names(count_data)) {
    count_data <- dplyr::select(count_data, response[1:length(response)], `<NA>`)

  } else {

    count_data <- dplyr::select(count_data, response[1:length(response)])

  }

  if(aggregate == TRUE) {

    # for an aggregate, counts of each response choice are given
    count_data

  } else {

    # for raw data, long data is provided
    data %>%
      mutate(response_text = factor(response_text, levels = c(response_tx)),
             response_code = factor(response_code, levels = c(response_cd)))

  }

}
