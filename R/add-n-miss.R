add_n_miss <- function(data, ..., label = "n_miss"){
  if (missing(...)) {
    data[[paste0(label, "_all")]] <- rowSums(is.na(data))
  } else {

    quo_vars <- rlang::quos(...)

    selected_data <- dplyr::select(data, !!!quo_vars)

    data[[paste0(label, "_vars")]] <- rowSums(is.na(selected_data))
  } # close else loop

  data
}
