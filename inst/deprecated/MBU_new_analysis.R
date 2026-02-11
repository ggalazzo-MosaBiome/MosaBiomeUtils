create_new_analysis <- function(filename) {
  # Ensure required packages are available
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required but not installed. Install it with install.packages('fs').")
  }

  # Read project configuration
  config <- yaml::read_yaml(here::here("project_config.yml"))
  project_id <- config$project_id
  project_name <- config$project_name

  # Define paths
  base_dir <- here::here()  # Assumes the function is called within an RStudio project
  analysis_dir <- file.path(base_dir, "2.analysis")
  results_dir <- file.path(base_dir, "4.results_files", filename)
  template_path <- system.file("rmarkdown/templates/report/skeleton/skeleton.rmd", package = "YourPackageName") # Replace with your package name
  rmd_file <- file.path(analysis_dir, paste0(filename, ".Rmd"))

  # Define subfolders within the results directory
  results_subfolders <- c("plots", "tables", "data_objects")

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
    if (!fs::file_exists(template_path)) {
      stop("Template file not found at: ", template_path)
    }
    template_content <- readLines(template_path)

    # Modify relevant fields in the template
    updated_content <- gsub("Project Name", filename, template_content) # Update title
    updated_content <- gsub("Project_id", project_id, updated_content) # Update project_id
    updated_content <- gsub("Project_name", project_name, updated_content) # Update project_name

    # Update the knit function to direct output to the specific folder
    updated_content <- gsub(
      "config$output_dir, envir=new.env()",
      paste0("file.path(config$output_dir, '", filename, "'), envir=new.env()"),
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
