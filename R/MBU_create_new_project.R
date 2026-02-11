#' Create MosaBiome Project Structure
#'
#' @description
#' Creates a standardized project folder structure for MosaBiome statistical
#' analyses. This function establishes a consistent, organized directory layout
#' that separates data, analysis code, supporting scripts, and output files.
#'
#' The function can be called directly from R or automatically invoked by
#' RStudio/Positron when using the MosaBiome project template.
#'
#' @param path Character string. The absolute or relative path where the project
#'   directory will be created. The directory will be created if it doesn't exist.
#'   Example: \code{"~/projects/my_analysis"} or \code{"/home/user/projects/study_001"}.
#'
#' @param project_id Character string. A unique identifier for the project, typically
#'   following an organizational naming convention. This ID is stored in the
#'   \code{project_config.yml} file and embedded in report metadata.
#'   Examples: \code{"PRJ-2024-001"}, \code{"STUDY-ABC-123"}, \code{"CLIENT-Q1-2024"}.
#'
#' @param project_name Character string. A human-readable, descriptive name for the
#'   project. This appears in the project README and report headers.
#'   Examples: \code{"Microbiome Diversity Analysis"}, \code{"Q1 Sales Report"}.
#'
#' @param ... Additional arguments passed by RStudio when using the project template.
#'   These are typically not used directly but ensure compatibility with the
#'   RStudio New Project wizard.
#'
#' @return Invisible \code{TRUE} on successful project creation.
#'
#' @details
#' \strong{Folder Structure Created:}
#'
#' The function creates the following standardized directory structure:
#'
#' \preformatted{
#' project/
#' |-- 1.data/
#' |   |-- 1.1.data_analysis_ready/
#' |   |-- readme.txt
#' |-- 2.analysis/
#' |   |-- readme.txt
#' |-- 3.supporting_code/
#' |   |-- readme.txt
#' |-- 4.results_files/
#' |   |-- readme.txt
#' |-- .header.html
#' |-- project_config.yml
#' |-- project.Rproj
#' |-- readme.txt
#' }
#'
#' \strong{Directory Purposes:}
#'
#' \describe{
#'   \item{\code{1.data/}}{Raw data files. Never modify original data here.}
#'   \item{\code{1.data/1.1.data_analysis_ready/}}{Cleaned, processed data ready for analysis.}
#'   \item{\code{2.analysis/}}{Quarto (.qmd) analysis files created by \code{\link{MBU_new_analysis}}.}
#'   \item{\code{3.supporting_code/}}{Helper R scripts, utility functions, and code not in main analyses.}
#'   \item{\code{4.results_files/}}{Rendered reports and outputs, organized by analysis name.}
#' }
#'
#' \strong{Configuration Files:}
#'
#' \describe{
#'   \item{\code{project_config.yml}}{YAML configuration storing project_id, project_name, and output_dir.
#'     This file is read by \code{\link{MBU_new_analysis}} and \code{\link{MBU_render_report}}.}
#'   \item{\code{project.Rproj}}{RStudio/Positron project file with recommended settings.}
#'   \item{\code{.header.html}}{HTML header template for reports (optional customization).}
#'   \item{\code{readme.txt}}{Project overview with structure documentation and quick start guide.}
#' }
#'
#' \strong{Author Information:}
#'
#' The author name is retrieved from \code{getOption("MosaBiomeUtils.author")}.
#' If not set, it defaults to "Unknown Author". Use \code{\link{set_author_name}}
#' to configure your name, which will be saved to your \code{.Rprofile}.
#'
#' @section Workflow:
#'
#' Typical workflow after creating a project:
#'
#' \enumerate{
#'   \item Open the \code{.Rproj} file in RStudio/Positron
#'   \item Create a new analysis: \code{MBU_new_analysis("my_first_analysis")}
#'   \item Edit the generated \code{.qmd} file in \code{2.analysis/}
#'   \item Render the report: \code{MBU_render_report()}
#'   \item Find output in \code{4.results_files/my_first_analysis/}
#' }
#'
#' @section RStudio/Positron Template:
#'
#' This function is registered as an RStudio project template. Users can create
#' new MosaBiome projects via:
#'
#' \emph{File > New Project > New Directory > MosaBiome Utility Functions}
#'
#' The template will prompt for project_id and project_name in the GUI.
#'
#' @examples
#' \dontrun{
#' # Create a new project with full details
#' MBU_create_new_project(
#'   path = "~/projects/microbiome_study",
#'   project_id = "MB-2024-001",
#'   project_name = "Gut Microbiome Diversity Analysis"
#' )
#'
#' # Create a project in the current directory
#' MBU_create_new_project(
#'   path = "./new_analysis",
#'   project_id = "QUICK-001",
#'   project_name = "Quick Analysis"
#' )
#'
#' # After creation, navigate to the project and start working
#' setwd("~/projects/microbiome_study")
#' MBU_new_analysis("alpha_diversity")
#' }
#'
#' @seealso
#' \code{\link{MBU_new_analysis}} to create analysis files within a project
#'
#' \code{\link{MBU_render_report}} to render Quarto reports
#'
#' \code{\link{set_author_name}} to configure your author name
#'
#' @family project management
#' @export
MBU_create_new_project <- function(path, project_id, project_name, ...) {
  # Retrieve the author name
  author_name <- getOption("MosaBiomeUtils.author", "Unknown Author")



  if (missing(project_id)) {
    cat("Argument 'project_id' is missing. Would you like to enter the id?\n")
    x <- as.numeric(readline(prompt = "Type project_id: "))
    if (is.na(project_id)) {
      project_id=""
      cat("project_id set to '' ")
    }
  }
  cat("project_id is:", project_id, "\n")

  if (missing(project_name)) {
    cat("Argument 'project_name' is missing. Would you like to enter the id?\n")
    x <- as.numeric(readline(prompt = "Type project_name: "))
    if (is.na(project_name)) {
      project_name=""
      cat("project_name set to '' ")
    }
  }
  cat("project_name is:", project_name, "\n")


  # Define subfolders
  subfolders <- c(
    "1.data",
    "1.data/1.1.data_analysis_ready",
    "2.analysis",
    "3.supporting_code",
    "4.results_files"
  )

  # Create subfolders and their readme
  for (sf in subfolders) {
    dir.create(file.path(path, sf), showWarnings = FALSE, recursive = TRUE)
    file.create(file.path(path, sf, "readme.txt"))
  }

  # Create R project file using folder name
  project_name_actual <- basename(path)
  rproj_file <- file.path(path, paste0(project_name_actual, ".Rproj"))

  # Create main files
  config_file <- file.path(path, "project_config.yml")
  file.create(config_file)
  file.create(file.path(path, "readme.txt"))
  file.create(file.path(path, ".header.html"))

  # Write header file
  writeLines(
    c(
      '<hr/>',
      '  <h3 style="text-align:center;color: gray;font-size:14px;">',
      '    <b>MosaBiome</b> Statistical Analysis Report',
      '  </h3>',
      '<hr/>'
    ),
    con = file.path(path, ".header.html")
  )

  # Write the Rproj file
  writeLines(
    c(
      "Version: 1.0",
      "",
      "RestoreWorkspace: Default",
      "SaveWorkspace: Default",
      "AlwaysSaveHistory: Default",
      "",
      "EnableCodeIndexing: Yes",
      "UseSpacesForTab: Yes",
      "NumSpacesForTab: 2",
      "Encoding: UTF-8",
      "",
      "RnwWeave: knitr",
      "LaTeX: pdfLaTeX"
    ),
    con = rproj_file
  )

  # Write project configuration (YAML)
  yaml::write_yaml(
    list(
      project_id = project_id,
      project_name = project_name,
      output_dir = "4.results_files"
    ),
    file = config_file
  )

  # Write a detailed README file
  readme_lines <- c(
    paste("Project Name:", project_name),
    paste("Project ID:", project_id),
    paste("Author:", author_name),
    paste("Date Created:", Sys.Date()),
    "",
    "Folder Structure:",
    "project/",
    "    |-- 1.data/: folder containing raw and processed data files.",
    "        |-- 1.1.data_analysis_ready/: folder containing cleaned data ready for analysis.",
    "    |-- 2.analysis/: folder containing Quarto (.qmd) files of the analysis.",
    "    |-- 3.supporting_code/: folder containing supplemental R scripts.",
    "    |-- 4.results_files/: folder containing rendered reports and outputs.",
    "        |-- <analysis_name>/: subfolder for each analysis containing:",
    "            |-- 4.1.plots/: PNG/SVG plots from the analysis.",
    "            |-- 4.2.tables/: table outputs.",
    "            |-- 4.3.data_objects/: R data objects (.rds files).",
    "    |-- project.Rproj: R project file.",
    "    |-- project_config.yml: YAML file containing project configuration.",
    "    |-- readme.txt: this file.",
    "",
    "Quick Start:",
    "    1. Create a new analysis: MBU_new_analysis('my_analysis')",
    "    2. Edit the .qmd file in 2.analysis/",
    "    3. Render the report: MBU_render_report()"
  )

  writeLines(readme_lines, con = file.path(path, "readme.txt"))

  # Optionally add a message
  message("Custom project structure created at: ", path)

  invisible(TRUE)
}
