#' Render Quarto Report to Project Output Directory
#'
#' @description
#' Renders a Quarto document (.qmd) to the project's configured output directory,
#' maintaining a clean separation between source files and rendered outputs.
#' This is the primary function for generating reports in MosaBiome projects.
#'
#' The function is designed to work seamlessly with Positron and RStudio IDEs.
#' When called without arguments, it automatically detects and renders the
#' currently open .qmd file.
#'
#' @param file Character string or \code{NULL}. Path to the \code{.qmd} file to render.
#'   \itemize{
#'     \item If \code{NULL} (default): Attempts to detect the currently active file
#'       in RStudio/Positron using \code{rstudioapi::getSourceEditorContext()}.
#'     \item If a relative path: Will be resolved relative to the project root
#'       using \code{here::here()}.
#'     \item If an absolute path: Used directly.
#'   }
#'   Examples: \code{NULL}, \code{"2.analysis/my_analysis.qmd"},
#'   \code{"~/projects/study/2.analysis/report.qmd"}.
#'
#' @param output_format Character string. The output format for rendering.
#'   \describe{
#'     \item{\code{"html"}}{(Default) HTML document with interactive features.}
#'     \item{\code{"pdf"}}{PDF document (requires LaTeX installation).}
#'     \item{\code{"docx"}}{Microsoft Word document.}
#'     \item{\code{"all"}}{Render all formats defined in the document's YAML header.}
#'   }
#'
#' @param open_result Logical. Whether to automatically open the rendered file
#'   after successful rendering. Defaults to \code{TRUE}.
#'   \itemize{
#'     \item For HTML: Opens in RStudio Viewer pane or system browser.
#'     \item For PDF/DOCX: Opens with system default application (when \code{TRUE}).
#'   }
#'
#' @return Invisible character string containing the path to the rendered output file.
#'   This allows for programmatic use, e.g., \code{output_path <- MBU_render_report()}.
#'
#' @details
#' \strong{How It Works:}
#'
#' Quarto does not natively support rendering to a directory different from
#' the source file location. This function works around this limitation by:
#'
#' \enumerate{
#'   \item Reading \code{project_config.yml} to determine the output directory
#'   \item Creating the output directory structure if it doesn't exist
#'   \item Copying the .qmd file to the output directory

#'   \item Rendering the document in the output directory
#'   \item Cleaning up the temporary .qmd copy
#' }
#'
#' \strong{Output Location:}
#'
#' Reports are rendered to:
#' \preformatted{
#' <output_dir>/<analysis_name>/<analysis_name>.<format>
#' }
#'
#' For example, rendering \code{2.analysis/descriptive_stats.qmd} produces:
#' \preformatted{
#' 4.results_files/descriptive_stats/descriptive_stats.html
#' }
#'
#' \strong{Directory Structure:}
#'
#' The function ensures the following subdirectories exist in the output folder:
#' \itemize{
#'   \item \code{4.1.plots/} - For saving plot images
#'   \item \code{4.2.tables/} - For saving table outputs
#'   \item \code{4.3.data_objects/} - For saving R data objects
#' }
#'
#' \strong{IDE Integration:}
#'
#' This function is also available as an RStudio/Positron addin:
#' \emph{Addins > Render Quarto Report}
#'
#' You can assign a keyboard shortcut for quick access via:
#' \emph{Tools > Modify Keyboard Shortcuts > Search "Render Quarto"}
#'
#' \strong{Error Handling:}
#'
#' The function provides informative error messages for common issues:
#' \itemize{
#'   \item File not found or not a .qmd file
#'   \item Not in a MosaBiome project directory (missing \code{project_config.yml})
#'   \item Quarto rendering errors (passed through from \code{quarto::quarto_render})
#' }
#'
#' @section Prerequisites:
#'
#' \itemize{
#'   \item Quarto must be installed on the system (\url{https://quarto.org/docs/get-started/})
#'   \item The \pkg{quarto} R package
#'   \item A valid MosaBiome project with \code{project_config.yml}
#'   \item For PDF output: A LaTeX distribution (e.g., TinyTeX, MiKTeX)
#' }
#'
#' @examples
#' \dontrun{
#' # Most common usage: render the currently open file
#' # (run this while editing a .qmd file in Positron/RStudio)
#' MBU_render_report()
#'
#' # Render a specific file
#' MBU_render_report("2.analysis/descriptive_statistics.qmd")
#'
#' # Render to PDF format
#' MBU_render_report("2.analysis/final_report.qmd", output_format = "pdf")
#'
#' # Render to Word document
#' MBU_render_report(output_format = "docx")
#'
#' # Render without opening the result
#' MBU_render_report(open_result = FALSE)
#'
#' # Capture the output path for further processing
#' output_path <- MBU_render_report("2.analysis/report.qmd")
#' message("Report saved to: ", output_path)
#'
#' # Render all formats defined in the document
#' MBU_render_report(output_format = "all")
#' }
#'
#' @seealso
#' \code{\link{MBU_new_analysis}} to create new analysis files
#'
#' \code{\link{MBU_create_new_project}} to set up a new project
#'
#' \code{\link[quarto]{quarto_render}} for underlying Quarto rendering
#'
#' @family project management
#' @export
MBU_render_report <- function(file = NULL, output_format = "html", open_result = TRUE) {
  # Auto-detect current file if not provided
  if (is.null(file)) {
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::hasFun("getSourceEditorContext")) {
      context <- rstudioapi::getSourceEditorContext()
      file <- context$path
      if (is.null(file) || file == "") {
        stop(
          "Could not detect current file. Please either:\n",
          "  1. Save your file first, or\n",
          "  2. Provide the file path: MBU_render_report('2.analysis/my_analysis.qmd')"
        )
      }
    } else {
      stop(
        "Could not detect current file (rstudioapi not available).\n",
        "Please provide the file path: MBU_render_report('2.analysis/my_analysis.qmd')"
      )
    }
  }

  # Validate file exists and is a .qmd file
  if (!file.exists(file)) {
    # Try relative to project root
    file <- here::here(file)
  }

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  if (!grepl("\\.qmd$", file, ignore.case = TRUE)) {
    stop("File must be a Quarto document (.qmd): ", file)
  }

  # Read project configuration
  config_path <- here::here("project_config.yml")
  if (!file.exists(config_path)) {
    stop(
      "project_config.yml not found in project root.\n",
      "Are you in a MosaBiome project directory?"
    )
  }

  config <- yaml::read_yaml(config_path)
  base_output_dir <- config$output_dir

  if (is.null(base_output_dir)) {
    base_output_dir <- "4.results_files"
    message("Note: output_dir not specified in project_config.yml, using default: ", base_output_dir)
  }

  # Determine output directory based on filename
  file_basename <- tools::file_path_sans_ext(basename(file))
  output_dir <- here::here(base_output_dir, file_basename)

  # Create output directory if needed
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }

  # Create subdirectories for plots, tables, and data objects
  subdirs <- c("4.1.plots", "4.2.tables", "4.3.data_objects")
  for (subdir in subdirs) {
    subdir_path <- file.path(output_dir, subdir)
    if (!dir.exists(subdir_path)) {
      dir.create(subdir_path, recursive = TRUE)
    }
  }

  # Copy .qmd to output directory for rendering
  temp_qmd <- file.path(output_dir, basename(file))
  file.copy(file, temp_qmd, overwrite = TRUE)

  # Render the document
  message("Rendering: ", basename(file))
  message("Output directory: ", output_dir)

  tryCatch({
    quarto::quarto_render(
      input = temp_qmd,
      output_format = output_format
    )

    # Determine the output file path
    output_ext <- switch(output_format,
      "html" = ".html",
      "pdf" = ".pdf",
      "docx" = ".docx",
      ".html"
    )
    output_file <- file.path(output_dir, paste0(file_basename, output_ext))

    message("Successfully rendered: ", output_file)

    # Open result if requested
    if (open_result && output_format == "html" && file.exists(output_file)) {
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::hasFun("viewer")) {
        rstudioapi::viewer(output_file)
      } else {
        utils::browseURL(output_file)
      }
    }

    invisible(output_file)

  }, error = function(e) {
    stop("Rendering failed: ", e$message)
  }, finally = {
    # Clean up: remove the temporary .qmd copy
    if (file.exists(temp_qmd)) {
      file.remove(temp_qmd)
    }
  })
}
