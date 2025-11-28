#' Create the custom project structure
#'
#' This function is called automatically by RStudio after the user chooses the
#' "My Custom Project" template.
#'
#' @param path The path to the newly created project directory
#' @param project_id The project unique identifier
#' @param project_name Name of the project
#' @param ... Additional arguments passed by RStudio (not usually needed)
#' @return Invisible TRUE
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
    )
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
    "Statistical Analysis (name)/",
    "    |-- 2.analysis/: folder containing R markdown (Rmd) files of the analysis performed according to the analysis plan document.",
    "    |-- 3.supporting_code/: folder containing supplemental code not meant to be included in the Rmd files.",
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

  writeLines(readme_lines, con = file.path(path, "readme.txt"))

  # Optionally add a message
  message("Custom project structure created at: ", path)

  invisible(TRUE)
}
