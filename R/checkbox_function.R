#' Function for dealing with checkbox data
#'
#' @param data add info
#' @param aggregate add info
#' @param metadata add info
#'
#' @return what does this return?
#'
#' @export

checkbox_function <- function(data, aggregate = TRUE, metadata = NULL) {

  n_col <- ncol(data)

  data <- gather(data, name, response) %>%
    mutate(response = case_when(response == "Checked" ~ 1,
                                response == "Unchecked" ~ 0,
                                TRUE ~ NA_real_)) %>%
    separate(name, c("field_name", "number"), sep = "___")

  if(aggregate == TRUE){

    data <- data %>%
      group_by(field_name, number) %>%
      summarise(response = sum(response, na.rm = TRUE)) %>%
      ungroup()

  } else {
    data <- data %>%
      mutate(number = factor(number))
  }

  if(!is.null(metadata)) {

    var <- data$field_name[1]

    metadata <- filter(metadata, field_name == var) %>%
      dplyr::select(field_name, select_choices_or_calculations) %>%
      separate(select_choices_or_calculations, as.character(c(1:n_col)), sep = " [|] ") %>%
      gather(number, item, `1`:`6`) %>%
      dplyr::select(-number) %>%
      separate(item, c("number", "item"), sep = ", ")

    data %>%
      left_join(metadata, by = c("field_name", "number")) %>%
      dplyr::select(item, response)
  } else {
    return(data)
  }
}
