format_lmm_html <- function(lmm_results,
                            show_CI = TRUE,
                            collapse_CI = FALSE,
                            show_SE = TRUE,
                            round_digits = NULL,
                            table_title = NULL,
                            table_subtitle = NULL,
                            rename_columns = NULL,
                            show_in_viewer = TRUE) {

  # Check if input is a list (multiple models) or a single model
  if (!is.list(lmm_results) || !all(sapply(lmm_results, is.list))) {
    lmm_results <- list("Model" = lmm_results)  # Wrap single model result into a list
  }

  # Prepare tables for each model
  formatted_tables <- lapply(names(lmm_results), function(model_name) {

    model_results <- lmm_results[[model_name]]

    # Extract fixed effects table
    fixed_df <- model_results$`Fixed Effect`

    # Optionally drop CI columns if not requested
    if (!show_CI) {
      fixed_df <- fixed_df %>% select(-c(CI.low, CI.upp), everything())
    }

    # Optionally drop Standard Error column if not requested
    if (!show_SE) {
      fixed_df <- fixed_df %>% select(-`Std. Err`, everything())
    }

    # Format coefficients with CI in parentheses
    if (show_CI&collapse_CI) {
      fixed_df <- fixed_df %>%
        mutate(CI = paste0(
          round(CI.low, round_digits),
          "–",
          round(CI.upp, round_digits)
        )) %>%
        select(-CI.low, -CI.upp, everything())
    }

    # Round numeric values if requested
    if (!is.null(round_digits)) {
      fixed_df <- fixed_df %>%
        mutate(across(where(is.numeric), ~ round(., round_digits)))
    }

    # Bold significant p-values
    fixed_df$`p-value` <- cell_spec(fixed_df$`p-value`,
                                    format = "html",
                                    bold = fixed_df$`p-value` < 0.05)
    fixed_df$`adjusted p-value` <- cell_spec(fixed_df$`adjusted p-value`,
                                             format = "html",
                                             bold = fixed_df$`adjusted p-value` < 0.05)

    # Rename columns if requested
    if (!is.null(rename_columns)) {
      names(fixed_df) <- rename_columns
    }

    # Extract Random Effects table
    random_df <- model_results$`Random Effect`

    # Extract model statistics
    total_N <- model_results$`Fixed Effect`$Total_N[1]

    # Combine Fixed & Random Effects into a kableExtra table
    table_html <- kable(fixed_df, format = "html", row.names = TRUE, escape = FALSE,
                        caption = paste("Fixed Effects for", model_name)) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

    # Format random effects section
    random_table <- kable(random_df, format = "html", row.names = FALSE, escape = FALSE) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

    # Add Random Effects Header
    random_table <- paste0("<hr><hr><h4>Random Effects for ", model_name, "</h4><hr><hr>", random_table)

    # Combine sections
    combined_table <- paste0("<h3>", model_name, "</h3>",
                             table_html, "<br>",
                             random_table, "<br><hr>")

    return(combined_table)
  })

  # Combine tables for multiple models or a single model
  final_html <- paste0("<h2>", table_title, "</h2>",
                       if (!is.null(table_subtitle)) paste0("<h3>", table_subtitle, "</h3>") else "",
                       paste(formatted_tables, collapse = ""))

  # Save to a temporary HTML file if Viewer display is requested
  if (show_in_viewer) {
    temp_file <- tempfile(fileext = ".html")
    writeLines(final_html, temp_file)
    htmltools::html_print(htmltools::HTML(readLines(temp_file)))  # Display in Viewer
  }

  return(final_html)
}



format_lmm_html(
  xxx,
  show_CI = TRUE,
  show_SE = FALSE,
  round_digits = 3,
  show_in_viewer = T,
  table_title = "Differential Abundance Between Groups Over Time",
  table_subtitle = "Linear Mixed Model Results"
)
jjj
tab_model()
