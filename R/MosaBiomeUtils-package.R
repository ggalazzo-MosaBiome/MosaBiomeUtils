#' MosaBiomeUtils: Standardized Project Structure and Workflow for Statistical Analysis
#'
#' @description
#' The MosaBiomeUtils package provides a comprehensive toolkit for standardizing
#' project structure, workflow, and reporting for statistical analyses within
#' the MosaBiome organization. It ensures consistency across projects, simplifies
#' report generation, and promotes reproducible research practices.
#'
#' @section Main Features:
#'
#' \strong{Project Management:}
#' \itemize{
#'   \item \code{\link{MBU_create_new_project}}: Create standardized project folder structures
#'   \item \code{\link{MBU_new_analysis}}: Generate new Quarto analysis files from templates
#'   \item \code{\link{MBU_render_report}}: Render Quarto reports to designated output directories
#' }
#'
#' \strong{Author Management:}
#' \itemize{
#'   \item \code{\link{set_author_name}}: Set or update the author name used in report templates
#'   \item Author name is automatically saved to \code{.Rprofile} for persistence
#' }
#'
#' \strong{Code Formatting:}
#' \itemize{
#'   \item \code{\link{MB_style_report}}: Apply custom MosaBiome code styling
#'   \item \code{\link{report_custom_transformers}}: Custom styler transformers
#' }
#'
#' \strong{Text Manipulation Addins:}
#' \itemize{
#'   \item \code{\link{wrapInQuote}}: Wrap vector elements in quotes
#'   \item \code{\link{underlineHtml}}: Add HTML underline tags to selected text
#' }
#'
#' @section Getting Started:
#'
#' \strong{1. Install and load the package:}
#' \preformatted{
#' # Install from GitHub (if applicable)
#' # devtools::install_github("MosaBiome/MosaBiomeUtils")
#'
#' library(MosaBiomeUtils)
#' }
#'
#' On first load, you will be prompted to enter your name, which will be used
#' as the author in all report templates.
#'
#' \strong{2. Create a new project:}
#' \preformatted{
#' MBU_create_new_project(
#'   path = "~/projects/my_analysis",
#'   project_id = "PRJ-2024-001",
#'   project_name = "My Statistical Analysis"
#' )
#' }
#'
#' \strong{3. Create a new analysis:}
#' \preformatted{
#' # Navigate to your project directory first
#' setwd("~/projects/my_analysis")
#'
#' # Create a new analysis file
#' MBU_new_analysis("descriptive_statistics")
#' }
#'
#' \strong{4. Render your report:}
#' \preformatted{
#' # Open the .qmd file, edit it, then render:
#' MBU_render_report()
#'
#' # Or specify a file directly:
#' MBU_render_report("2.analysis/descriptive_statistics.qmd")
#' }
#'
#' @section Project Structure:
#'
#' MosaBiomeUtils creates and expects the following folder structure:
#'
#' \preformatted{
#' project/
#' |-- 1.data/
#' |   |-- 1.1.data_analysis_ready/
#' |-- 2.analysis/
#' |   |-- my_analysis.qmd
#' |-- 3.supporting_code/
#' |-- 4.results_files/
#' |   |-- my_analysis/
#' |       |-- my_analysis.html
#' |       |-- 4.1.plots/
#' |       |-- 4.2.tables/
#' |       |-- 4.3.data_objects/
#' |-- project_config.yml
#' |-- project.Rproj
#' |-- readme.txt
#' }
#'
#' @section Configuration:
#'
#' The \code{project_config.yml} file stores project-level settings:
#'
#' \preformatted{
#' project_id: "PRJ-2024-001"
#' project_name: "My Statistical Analysis"
#' output_dir: "4.results_files"
#' }
#'
#' @section IDE Integration:
#'
#' MosaBiomeUtils provides RStudio/Positron addins for common tasks:
#'
#' \itemize{
#'   \item \strong{Render Quarto Report}: Render the current .qmd file
#'   \item \strong{MosaBiome Style Report}: Apply code formatting
#'   \item \strong{Wrap in quotes}: Add quotes to vector elements
#'   \item \strong{Underline for HTML}: Add HTML underline tags
#' }
#'
#' Access these via the Addins menu or assign keyboard shortcuts.
#'
#' @section Dependencies:
#'
#' MosaBiomeUtils requires the following packages:
#' \itemize{
#'   \item \pkg{fs}: File system operations
#'   \item \pkg{here}: Project-relative paths
#'   \item \pkg{yaml}: YAML file handling
#'   \item \pkg{rstudioapi}: IDE integration
#'   \item \pkg{quarto}: Quarto document rendering
#' }
#'
#' Optional packages for additional features:
#' \itemize{
#'   \item \pkg{styler}: Code formatting (for \code{MB_style_report})
#'   \item \pkg{rmarkdown}: Legacy RMarkdown support
#' }
#'
#' @section Author:
#'
#' MosaBiome Team
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item Quarto documentation: \url{https://quarto.org/}
#'   \item Report issues: \url{https://github.com/MosaBiome/MosaBiomeUtils/issues}
#' }
#'
#' @keywords internal
"_PACKAGE"
