#' Create Rmd files and folder structure needed, in the current project folder.
#'
#' This function is called automatically by RStudio after the user chooses the
#' "My Custom Project" template.
#'
#' @param filename name of the new analysis file, it will be used also as title.
#' @param ... Additional arguments passed by RStudio (not usually needed)
#' @return Invisible TRUE
#' @export

MBU_new_analysis <- function(file_name) {
  # Ensure required packages are available
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required but not installed. Install it with install.packages('fs').")
  }

  if (grepl("^A-Za-zα-ωΑ-Ω0-9_\\s.", file_name) || grepl("\\s", file_name)) {
    # Replace white spaces with underscores
    filename <- gsub("\\s+", "_", file_name)

    # Replace special characters and periods with hyphens
    filename <- gsub("[^A-Za-zα-ωΑ-Ω0-9_\\s.]", "-", filename)

    # Set title to the original value for display
    title <- file_name
  } else {
    filename <- file_name
    title <- file_name
  }

  # Read project configuration
  config <- yaml::read_yaml(here::here("project_config.yml"))
  project_id <- config$project_id
  project_name <- config$project_name

  # Define paths
  base_dir <- here::here()  # Assumes the function is called within an RStudio project
  analysis_dir <- file.path(base_dir, "2.analysis")
  results_dir <- file.path(base_dir, "4.results_files", filename)

  # Locate the skeleton file dynamically
  template_path <- file.path(base::find.package("MosaBiomeUtils"), "rmarkdown", "templates", "report", "skeleton", "skeleton.Rmd")

  if (!fs::file_exists(template_path)) {
    stop("Template file not found at: ", template_path)
  }

  rmd_file <- file.path(analysis_dir, paste0(filename, ".Rmd"))

  # Define subfolders within the results directory
  results_subfolders <- c("4.1.plots", "4.2.tables", "4.3.data_objects")

  # Create directories if they don't exist
  if (!fs::dir_exists(analysis_dir)) fs::dir_create(analysis_dir)
  if (!fs::dir_exists(results_dir)) fs::dir_create(results_dir)

  for (subfolder in results_subfolders) {
    subfolder_path <- file.path(results_dir, subfolder)
    if (!fs::dir_exists(subfolder_path)) fs::dir_create(subfolder_path)
  }

  # Create the Rmd file if it doesn't exist
  if (!fs::file_exists(rmd_file)) {
    # Read the template
    template_content <- readLines(template_path)

    # Modify relevant fields in the template
    updated_content <- gsub("Analysis_title", title, template_content) # Update title
    updated_content <- gsub("Project_id", project_id, updated_content) # Update project_id
    updated_content <- gsub("Project_name", project_name, updated_content) # Update project_name

    # Update the knit function to direct output to the specific folder
    updated_content <- gsub(
      pattern = "config\\$output_dir, envir=new.env\\(\\)",
      replacement = paste0("file.path(config$output_dir, '", filename, "'), envir=new.env()"),
      updated_content
    )

    # Write the updated content to the new Rmd file
    writeLines(updated_content, rmd_file)
    message("Created Rmd file: ", rmd_file)
  } else {
    message("Rmd file already exists: ", rmd_file)
  }

  # Inform the user about the results directory
  message("Created/checked folder structure in: ", results_dir)
}
