#' Create a New Quarto Analysis File
#'
#' @description
#' Creates a new Quarto (.qmd) analysis file from the MosaBiome template and
#' automatically sets up the corresponding output folder structure in the
#' results directory. This function must be called from within a MosaBiome
#' project directory (created by \code{\link{MBU_create_new_project}}).
#'
#' @param file_name Character string. The name for the new analysis. This will be
#'   used as both the filename and the report title. The function automatically
#'   sanitizes the input:
#'   \itemize{
#'     \item Spaces are replaced with underscores
#'     \item Special characters are replaced with hyphens
#'     \item The original name is preserved as the report title
#'   }
#'   Examples: \code{"descriptive_stats"}, \code{"alpha diversity"} (becomes
#'   \code{alpha_diversity.qmd}), \code{"Analysis #1"} (becomes \code{Analysis_-1.qmd}).
#'
#' @return Invisible character string containing the path to the created .qmd file.
#'   The function is primarily called for its side effects (file and directory creation).
#'
#' @details
#' \strong{What This Function Creates:}
#'
#' \enumerate{
#'   \item A new Quarto file at \code{2.analysis/<file_name>.qmd}
#'   \item An output directory at \code{4.results_files/<file_name>/}
#'   \item Subdirectories for organizing outputs:
#'     \itemize{
#'       \item \code{4.1.plots/} - For plot images (PNG, SVG)
#'       \item \code{4.2.tables/} - For table outputs
#'       \item \code{4.3.data_objects/} - For R data objects (.rds files)
#'     }
#' }
#'
#' \strong{Template Customization:}
#'
#' The generated .qmd file is pre-populated with:
#' \itemize{
#'   \item Project ID and name from \code{project_config.yml}
#'   \item Author name from \code{getOption("MosaBiomeUtils.author")}
#'   \item Current date
#'   \item Standard Quarto HTML output configuration
#'   \item Starter sections for Introduction, Data, Methods, Results, and Conclusions
#' }
#'
#' \strong{Prerequisites:}
#'
#' This function requires:
#' \itemize{
#'   \item A valid MosaBiome project directory (with \code{project_config.yml})
#'   \item The \pkg{fs} package for file operations
#'   \item The working directory set to the project root (or use \code{here::here()})
#' }
#'
#' \strong{File Naming:}
#'
#' If a file with the same name already exists, the function will not overwrite
#' it. Instead, it will display a message indicating the file already exists.
#' This prevents accidental loss of work.
#'
#' @section Workflow:
#'
#' Typical usage within a project:
#'
#' \preformatted{
#' # 1. Navigate to your project (or open the .Rproj file)
#' setwd("~/projects/my_project")
#'
#' # 2. Create a new analysis
#' MBU_new_analysis("exploratory_analysis")
#'
#' # 3. Edit the file in 2.analysis/exploratory_analysis.qmd
#'
#' # 4. When ready, render the report
#' MBU_render_report()
#' }
#'
#' @examples
#' \dontrun{
#' # Simple analysis name
#' MBU_new_analysis("descriptive_statistics")
#' # Creates: 2.analysis/descriptive_statistics.qmd
#'
#' # Name with spaces (automatically sanitized)
#' MBU_new_analysis("alpha diversity analysis")
#' # Creates: 2.analysis/alpha_diversity_analysis.qmd
#'
#' # Multiple analyses in one project
#' MBU_new_analysis("01_data_cleaning")
#' MBU_new_analysis("02_exploratory_analysis")
#' MBU_new_analysis("03_statistical_models")
#' MBU_new_analysis("04_figures_for_publication")
#' }
#'
#' @seealso
#' \code{\link{MBU_create_new_project}} to create a new project first
#'
#' \code{\link{MBU_render_report}} to render the analysis to HTML
#'
#' \code{\link{set_author_name}} to configure the author name used in templates
#'
#' @family project management
#' @export

MBU_new_analysis <- function(file_name) {
  # Ensure required packages are available
  if (!requireNamespace("fs", quietly = TRUE)) {
    stop("The 'fs' package is required but not installed. Install it with install.packages('fs').")
  }

  # Sanitize filename
  if (grepl("[^A-Za-z0-9_\\s.]", file_name) || grepl("\\s", file_name)) {
    # Replace white spaces with underscores
    filename <- gsub("\\s+", "_", file_name)
    # Replace special characters and periods with hyphens
    filename <- gsub("[^A-Za-z0-9_\\s.]", "-", filename)
    # Set title to the original value for display
    title <- file_name
  } else {
    filename <- file_name
    title <- file_name
  }

  # Read project configuration
  config_path <- here::here("project_config.yml")
  if (!file.exists(config_path)) {
    stop(
      "project_config.yml not found.\n",
      "Are you in a MosaBiome project directory?\n",
      "Use MBU_create_new_project() to create a new project first."
    )
  }

  config <- yaml::read_yaml(config_path)
  project_id <- config$project_id %||% ""
  project_name <- config$project_name %||% ""

  # Get author name from options
  author_name <- getOption("MosaBiomeUtils.author", "Unknown Author")

  # Define paths
  base_dir <- here::here()
  analysis_dir <- file.path(base_dir, "2.analysis")
  results_dir <- file.path(base_dir, "4.results_files", filename)

  # Locate the Quarto skeleton file
  template_path <- file.path(
    base::find.package("MosaBiomeUtils"),
    "quarto", "templates", "report", "skeleton", "skeleton.qmd"
  )

  if (!fs::file_exists(template_path)) {
    stop("Quarto template file not found at: ", template_path)
  }

  qmd_file <- file.path(analysis_dir, paste0(filename, ".qmd"))

  # Define subfolders within the results directory
  results_subfolders <- c("4.1.plots", "4.2.tables", "4.3.data_objects")

  # Create directories if they don't exist
  if (!fs::dir_exists(analysis_dir)) fs::dir_create(analysis_dir)
  if (!fs::dir_exists(results_dir)) fs::dir_create(results_dir)

  for (subfolder in results_subfolders) {
    subfolder_path <- file.path(results_dir, subfolder)
    if (!fs::dir_exists(subfolder_path)) fs::dir_create(subfolder_path)
  }

  # Create the Quarto file if it doesn't exist
  if (!fs::file_exists(qmd_file)) {
    # Read the template
    template_content <- readLines(template_path)

    # Modify relevant fields in the template
    updated_content <- gsub("Analysis_title", title, template_content)
    updated_content <- gsub("Project_id", project_id, updated_content)
    updated_content <- gsub("Project_name", project_name, updated_content)
    updated_content <- gsub("Author_name", author_name, updated_content)

    # Write the updated content to the new Quarto file
    writeLines(updated_content, qmd_file)
    message("Created Quarto file: ", qmd_file)
  } else {
    message("Quarto file already exists: ", qmd_file)
  }

  # Inform the user about the results directory and next steps

  message("Created/checked folder structure in: ", results_dir)
  message("\nTo render this report, use: MBU_render_report()")

  invisible(qmd_file)
}

# Null coalescing operator (if not already defined)
`%||%` <- function(x, y) if (is.null(x)) y else x
