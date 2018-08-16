#' Function for dealing with checkbox data
#'
#' @param data add info
#' @param aggregate add info
#'
#' @return what does this return?
#'
#' @export

checkbox_function <- function(data, aggregate = TRUE) {

  labels <- list()

  for(i in 1:ncol(data)) {

    labels[[i]] <- attributes(data[[i]])$label

  }

  labels_df <- as_data_frame(do.call(rbind, labels)) %>%
    transmute(label = gsub(".*[0-9]+\\.[[:space:]]", "", V1),
              number = row_number())

  n_col <- ncol(data)

  data <- gather(data, name, response) %>%
    mutate(response = case_when(response == "Checked" ~ 1,
                                response == "Unchecked" ~ 0,
                                TRUE ~ NA_real_)) %>%
    separate(name, c("field_name", "number"), sep = "___") %>%
    mutate(number = as.integer(number)) %>%
    left_join(labels_df, by = "number")

  if(aggregate == TRUE) {

    data %>%
      group_by(number, label) %>%
      summarise(response = sum(response, na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::select(-number)

  } else {

    data %>%
      dplyr::select(label, response) %>%
      mutate(label = factor(label))

  }
}
