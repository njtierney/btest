add_n_miss <- function(data, ..., label = "n_miss"){
  if (missing(...)) {
    purrrlyr::by_row(.d = data,
                     ..f = function(x) n_miss(x),
                     .collate = "row",
                     .to = paste0(label,"_all"))
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    prop_selected_data <- purrrlyr::by_row(.d = selected_data,
                                           ..f = function(x) n_miss(x),
                                           .collate = "row",
                                           .to =  paste0(label,"_vars"))

    # add only the variables prop_miss function, not the whole data.frame...
    prop_selected_data_cut <- prop_selected_data %>%
      dplyr::select(!!as.name(paste0(label,"_vars")))

    dplyr::bind_cols(data, prop_selected_data_cut) %>% dplyr::as_tibble()
  } # close else loop

}
