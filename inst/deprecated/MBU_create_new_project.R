#' Create the custom project structure
#'
#' This function is called automatically by RStudio after the user chooses the
#' "My Custom Project" template.
#'
#' @param path The path to the newly created project directory
#' @param ... Additional arguments passed by RStudio (not usually needed)
#' @return Invisible TRUE
#' @export
MBU_create_new_project <- function(path, ...) {

  # Retrieve the author name
  author_name <- getOption("MosaBiomeUtils.author", "Unknown Author")


  # Define subfolders
  subfolders <- c(
    "1.data",
    "1.data/1.1.data_analysis_ready",
    "2.analysis", "3.supporting_code",
    "4.results_files"
    )

  # Create subfolders and their readme
  for (sf in subfolders) {
    dir.create(file.path(path, sf), showWarnings = FALSE, recursive = TRUE)
    file.create(file.path(path, sf, "readme.txt"))
  }

  # Create R project file using folder name
  project_name <- basename(path)
  rproj_file <- file.path(path, paste0(project_name, ".Rproj"))

  # Create main files
  file.create(file.path(path, "project_config.yml"))
  file.create(file.path(path, "_output.yml"))
  file.create(file.path(path, "readme.txt"))

  # write the Rproj file
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

  # # Create config files needed to knit the mrd properly
  # writeLines(c('output_dir: "4.docs"'),
  #            con = file.path(path, "project_config.yml"))
  # writeLines(c("output: ",
  #              "   html_document:",
  #              "   toc: true",
  #              "   toc_depth: 4",
  #              "   toc_float: true",
  #              "   code_folding: hide",
  #              "  rmdformats::downcute:",
  #              "   self_contained: true",
  #              "   thumbnails: false",
  #              "   lightbox: true",
  #              "   gallery: false",
  #              "   highlight: tango",
  #              "  pdf_document:",
  #              "   toc: true",
  #              "   toc_depth: 4"
  # ),
  # con = file.path(path, "_output.yml"))

   readme_lines <- c(
    "Folder Structure:",
    "Statistical Analysis (name)/",
    "    |-- 2.analysis/: folder containing R markdown (Rmd) files of the analysis performed according to the analysis plan document.",
    "    |-- 3.supporting_code/: folder containing supplemental code not meant to be included in the Rmd files.",
    "    |-- 3.processed_data/: *MAYBE TO REMOVE OR MAKE IT OPTIONAL* folder containing files and/or R objects generated during the analysis process.",
    "    |-- 4.results_files/: folder containing the knitted reports in PDF or HTML format generated from the Rmd files.",
    "        |-- 4.1.plots/: folder containing JPG\\PNG\\SVG of the plots included in the report files.",
    "        |-- 4.2.tables/: folder containing JPG\\PDF of the tables included in the report files.",
    "        |-- 4.3.data_objects/: folder containing data-frames as R-objects used to perform analyses and generate plots and tables.",
    "    |-- renv/: folder, used by the 'renv' package to store info about the library used and their version.",
    "    |-- project.Rproj: R project file.",
    "    |-- project_config.yml: YAML file containing config instruction for RMarkdown.",
    "    |-- renv.lock: file used by renv to store libraries info. ",
    "    |-- readme.txt: this file."
  )

  writeLines(c(
      paste("Project Name:", project_name),
      paste("Author:", author_name),
      paste("Date Created:", Sys.Date()),
    readme_lines), con = file.path(path, "readme.txt"))

  # Optionally add a message
  message("Custom project structure created at: ", path)

  invisible(TRUE)
}
