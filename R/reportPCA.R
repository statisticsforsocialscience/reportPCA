####
#
# This file provides the functions to generate a GT-Table from a PCA
#


reportPCATable <- function(mypca,
                           table_title = "Principal Component Analysis",
                           table_subtitle = "",
                           alpha_table = NULL) {

  # some assertions
  if(mypca$fn != "principal") warning("This only works with PCA")


  my_factors <- mypca$factors

  my_loadings <- mypca$loadings %>% unclass()

  my_names <- colnames(my_loadings)

  my_eigenvalues <- data.frame( t(mypca$values[1:my_factors]) )
  names(my_eigenvalues) <- my_names

  my_eigens <- data.frame(rowname = "Eigenvalue", stringsAsFactors = F) %>% dplyr::bind_cols(my_eigenvalues)

  my_commu <- mypca$communality %>%
    tibble::enframe(name = "rowname", value = "Communality") %>%
    dplyr::arrange(Communality)

  my_var <- mypca$Vaccounted %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::filter(rowname == "Proportion Var")




  # Columns that are numbers
  number_selector <- (1:(my_factors + 2))[-1]

  # Columns that are RC-Loadings
  rc_selector <- 2:(my_factors + 1)

  my_decimals <- 3

  my_table_data <- my_loadings %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::bind_rows(my_eigens) %>%
    dplyr::bind_rows(my_var) %>%
    dplyr::bind_rows(alpha_table) %>%
    dplyr::left_join(my_commu, by = "rowname")


  my_row_count <- nrow(my_table_data)

  # location rows where statistical data begins
  stats_row_numbers <- (my_row_count-4):my_row_count


  tab <- my_table_data %>%
    gt() %>%
    tab_header(
      title = table_title,
      subtitle = table_subtitle
    ) %>%
    tab_spanner(
      label = "Rotated Components", columns = rc_selector
    ) %>%
    fmt_missing(
      columns = TRUE,
      missing_text = " "
    ) %>%
    fmt_number(
      columns = number_selector,
      decimals = my_decimals
    ) %>%
    fmt_percent(columns = number_selector,
                rows = starts_with("Pro"),
                decimals = 0) %>%
    tab_row_group(
      group = "Loadings",
      rows = 1:(my_row_count-5)
    ) %>%
    tab_row_group(
      group = "Statistics",
      rows = stats_row_numbers
    )



}
