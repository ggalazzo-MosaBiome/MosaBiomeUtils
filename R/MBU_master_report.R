#' Generate Master Report for MosaBiome Project
#'
#' @description
#' Creates a master report (index page) that provides an overview of all analyses
#' in a MosaBiome project, with links to individual HTML reports. The master report
#' includes a project introduction, organized navigation to all sub-reports, and
#' metadata about each analysis.
#'
#' This function scans the project's output directory for rendered HTML reports,
#' extracts metadata from each, and generates a navigable index page with
#' categorized links and optional descriptions.
#'
#' @param study_description Character string or NULL. A description of the study/project
#'   that will appear at the top of the master report. If NULL (default), a placeholder
#'   text is used that can be edited in the generated .qmd file.
#'   Example: "This study investigates the gut microbiome composition in patients with..."
#'
#' @param report_groups Named list or NULL. Custom groupings for organizing reports.
#'   Each list element should be a character vector of report folder names (or patterns)
#'   to include in that group. If NULL (default), reports are grouped automatically
#'   based on naming conventions (e.g., prefix patterns like "1.A", "1.B", etc.).
#'   Example:
#'   \preformatted{
#'   list(
#'     "Fecal Microbiota" = c("1.A.1-Fecal", "1.B.1-Fecal", "1.C.1-Fecal"),
#'     "Vaginal Microbiota" = c("1.A.2-Vaginal", "1.B.2-Vaginal"),
#'     "Additional Analyses" = c("2.A", "2.B")
#'   )
#'   }
#'
#' @param output_name Character string. The name for the master report file and folder.
#'   Defaults to \code{"0.Statistical_Analysis_Report"}. The leading "0." ensures it
#'   appears first in directory listings.
#'
#' @param render Logical. Whether to automatically render the generated .qmd file
#'   to HTML after creation. Defaults to \code{TRUE}.
#'
#' @param open_result Logical. Whether to open the rendered HTML in the viewer
#'   after rendering. Defaults to \code{TRUE}. Only applies when \code{render = TRUE}.
#'
#' @param include_metadata Logical. Whether to extract and display metadata
#'   (title, author, date) from individual reports. Defaults to \code{TRUE}.
#'   This provides richer information but requires parsing each HTML file.
#'
#' @param exclude_patterns Character vector. Folder name patterns to exclude from
#'   the report listing. Defaults to \code{c("^0\\.", "Presentation", "Support")}.
#'   The master report folder itself is always excluded.
#'
#' @return Invisible character string containing the path to the rendered HTML file
#'   (if \code{render = TRUE}) or the generated .qmd file (if \code{render = FALSE}).
#'
#' @details
#' \strong{How It Works:}
#'
#' \enumerate{
#'   \item Scans \code{4.results_files/} for subdirectories containing HTML reports
#'   \item Extracts metadata (title, date, author) from each HTML file
#'   \item Groups reports by category (automatic or user-defined)
#'   \item Generates a Quarto document with the master report content
#'   \item Optionally renders to HTML
#' }
#'
#' \strong{Automatic Grouping:}
#'
#' When \code{report_groups = NULL}, reports are grouped based on their prefix patterns:
#' \itemize{
#'   \item Numeric prefixes (e.g., "1.", "2.") create top-level groups
#'   \item Letter suffixes (e.g., ".A.", ".B.") create subgroups
#'   \item Reports without clear prefixes go into "Other Analyses"
#' }
#'
#' \strong{Output Location:}
#'
#' The master report is created at:
#' \preformatted{
#' 4.results_files/0.Statistical_Analysis_Report/0.Statistical_Analysis_Report.html
#' }
#'
#' \strong{UI/UX Features:}
#'
#' The generated master report includes:
#' \itemize{
#'   \item Responsive card-based layout for report links
#'   \item Report metadata display (title, date, author)
#'   \item Floating table of contents for navigation
#'   \item MosaBiome branded footer
#'   \item Dark/light theme toggle
#' }
#'
#' @section Improvements Over Manual Reports:
#'
#' This function provides several advantages over manually creating master reports:
#' \itemize{
#'   \item \strong{Automatic Discovery}: Finds all HTML reports in the project
#'   \item \strong{Metadata Extraction}: Pulls titles and dates from reports
#'   \item \strong{Consistent Styling}: Uses MosaBiome branded templates
#'   \item \strong{Easy Updates}: Re-run to add newly created reports
#'   \item \strong{Flexible Grouping}: Organize reports as needed
#' }
#'
#' @section Future Development Ideas:
#'
#' Potential enhancements for future versions:
#' \itemize{
#'   \item Search/filter functionality for projects with many reports
#'   \item Thumbnail previews of key figures from each report
#'   \item Report status indicators (draft, final, archived)
#'   \item Changelog/version tracking
#'   \item Export to PDF with all sub-reports concatenated
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage - auto-discover and group reports
#' MBU_master_report()
#'
#' # Provide a custom study description
#' MBU_master_report(
#'   study_description = "This study investigates the relationship between
#'     gut microbiome composition and metabolic markers in Type 2 Diabetes patients."
#' )
#'
#' # Define custom report groupings
#' MBU_master_report(
#'   study_description = "Microbiome diversity analysis across sample types.",
#'   report_groups = list(
#'     "Gut Microbiota" = c("Fecal", "Intestinal"),
#'     "Oral Microbiota" = c("Saliva", "Oral"),
#'     "Correlations" = c("Correlation", "Association")
#'   )
#' )
#'
#' # Generate without rendering (for manual editing first)
#' MBU_master_report(render = FALSE)
#' # Edit the .qmd file, then render manually:
#' MBU_render_report("4.results_files/0.Statistical_Analysis_Report/0.Statistical_Analysis_Report.qmd")
#'
#' # Exclude certain folders
#' MBU_master_report(exclude_patterns = c("^0\\.", "Presentation", "draft", "test"))
#' }
#'
#' @seealso
#' \code{\link{MBU_render_report}} for rendering individual reports
#'
#' \code{\link{MBU_new_analysis}} for creating new analysis files
#'
#' \code{\link{MBU_create_new_project}} for setting up new projects
#'
#' @family project management
#' @export
MBU_master_report <- function(
    study_description = NULL,
    report_groups = NULL,
    output_name = "0.Statistical_Analysis_Report",
    render = TRUE,
    open_result = TRUE,
    include_metadata = TRUE,
    exclude_patterns = c("^0\\.", "Presentation", "Support")
) {

  # Validate we're in a MosaBiome project

config_path <- here::here("project_config.yml")
  if (!file.exists(config_path)) {
    stop(
      "project_config.yml not found in project root.\n",
      "Are you in a MosaBiome project directory?\n",
      "Use MBU_create_new_project() to create a new project first."
    )
  }

  # Read project configuration
  config <- yaml::read_yaml(config_path)
  project_id <- config$project_id %||% "Unknown Project"
  project_name <- config$project_name %||% "Statistical Analysis"
  base_output_dir <- config$output_dir %||% "4.results_files"

  # Get author name
  author_name <- getOption("MosaBiomeUtils.author", "Unknown Author")

  # Define output paths
  output_folder <- here::here(base_output_dir, output_name)
  qmd_file <- file.path(output_folder, paste0(output_name, ".qmd"))
  html_file <- file.path(output_folder, paste0(output_name, ".html"))

  # Create output directory if needed
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
    message("Created output directory: ", output_folder)
  }

  # Scan for HTML reports
  results_path <- here::here(base_output_dir)
  report_dirs <- list.dirs(results_path, recursive = FALSE, full.names = FALSE)

  # Exclude the master report folder and other patterns
  exclude_patterns <- c(exclude_patterns, paste0("^", output_name, "$"))
  for (pattern in exclude_patterns) {
    report_dirs <- report_dirs[!grepl(pattern, report_dirs, ignore.case = TRUE)]
  }

  # Find HTML files in each directory
  reports <- lapply(report_dirs, function(dir_name) {
    dir_path <- file.path(results_path, dir_name)
    html_files <- list.files(dir_path, pattern = "\\.html$", full.names = TRUE)

    if (length(html_files) == 0) {
      return(NULL)
    }

    # Use the first HTML file (usually matches directory name)
    html_file <- html_files[1]

    # Extract metadata if requested
    metadata <- list(
      folder = dir_name,
      html_path = html_file,
      relative_path = paste0("../", dir_name, "/", basename(html_file)),
      title = gsub("_", " ", gsub("-", " - ", dir_name)),
      date = NA_character_,
      author = NA_character_
    )

    if (include_metadata) {
      metadata <- extract_report_metadata(html_file, metadata)
    }

    return(metadata)
  })

  # Remove NULL entries (directories without HTML files)
  reports <- Filter(Negate(is.null), reports)

  if (length(reports) == 0) {
    stop(
      "No HTML reports found in ", results_path, "\n",
      "Create and render some analyses first using MBU_new_analysis() and MBU_render_report()"
    )
  }

  # Group reports
  if (is.null(report_groups)) {
    grouped_reports <- auto_group_reports(reports)
  } else {
    grouped_reports <- custom_group_reports(reports, report_groups)
  }

  # Set default study description
  if (is.null(study_description)) {
    study_description <- paste0(
      "The results are divided into sections, each addressing a specific research question. ",
      "The findings for each section can be accessed by clicking on the corresponding research question.\n\n",
      "If the figures are too small, try:\n\n",
      "1. Right click on the figure.\n",
      "2. Select \"open in a new tab\".\n",
      "3. In the new tab it will be also possible to zoom in the figure."
    )
  }

  # Generate Quarto content
  qmd_content <- generate_master_qmd(
    project_name = project_name,
    project_id = project_id,
    author_name = author_name,
    study_description = study_description,
    grouped_reports = grouped_reports,
    include_metadata = include_metadata
  )

  # Write the Quarto file
  writeLines(qmd_content, qmd_file)
  message("Created master report Quarto file: ", qmd_file)

  # Render if requested
  if (render) {
    message("Rendering master report...")

    tryCatch({
      quarto::quarto_render(input = qmd_file, output_format = "html")

      message("Successfully rendered: ", html_file)

      # Open result if requested
      if (open_result && file.exists(html_file)) {
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::hasFun("viewer")) {
          rstudioapi::viewer(html_file)
        } else {
          utils::browseURL(html_file)
        }
      }

      invisible(html_file)
    }, error = function(e) {
      warning("Rendering failed: ", e$message, "\n",
              "The .qmd file was created at: ", qmd_file, "\n",
              "You can render it manually after fixing any issues.")
      invisible(qmd_file)
    })
  } else {
    message("\nTo render this report, use:")
    message("  quarto::quarto_render('", qmd_file, "')")
    invisible(qmd_file)
  }
}


#' Extract metadata from an HTML report file
#'
#' @param html_path Path to the HTML file
#' @param metadata List with default metadata values
#' @return Updated metadata list
#' @keywords internal
extract_report_metadata <- function(html_path, metadata) {
  tryCatch({
    # Read first part of HTML file (metadata is usually in the header)
    con <- file(html_path, "r")
    lines <- readLines(con, n = 100, warn = FALSE)
    close(con)
    html_head <- paste(lines, collapse = "\n")

    # Extract title
    title_match <- regmatches(
      html_head,
      regexpr('<h1[^>]*class="[^"]*title[^"]*"[^>]*>([^<]+)</h1>', html_head, perl = TRUE)
    )
    if (length(title_match) > 0) {
      metadata$title <- gsub('<[^>]+>', '', title_match[1])
      metadata$title <- trimws(metadata$title)
    }

    # Extract date
    date_match <- regmatches(
      html_head,
      regexpr('<h4[^>]*class="[^"]*date[^"]*"[^>]*>([^<]+)</h4>', html_head, perl = TRUE)
    )
    if (length(date_match) > 0) {
      metadata$date <- gsub('<[^>]+>', '', date_match[1])
      metadata$date <- trimws(metadata$date)
    }

    # Extract author
    author_match <- regmatches(
      html_head,
      regexpr('<h4[^>]*class="[^"]*author[^"]*"[^>]*>([^<]+)</h4>', html_head, perl = TRUE)
    )
    if (length(author_match) > 0) {
      metadata$author <- gsub('<[^>]+>', '', author_match[1])
      metadata$author <- trimws(metadata$author)
    }
  }, error = function(e) {
    # Silently fail and use defaults
  })

  return(metadata)
}


#' Automatically group reports based on naming conventions
#'
#' @param reports List of report metadata
#' @return Named list of grouped reports
#' @keywords internal
auto_group_reports <- function(reports) {
  grouped <- list()

  for (report in reports) {
    folder <- report$folder

    # Try to extract group from naming convention
    # Patterns like "1.A.1-Fecal-...", "1.B.2-Vaginal-...", "2.A-..."
    group_name <- "Other Analyses"

    # Check for common grouping patterns
    if (grepl("Fecal|Gut|Intestinal|Stool", folder, ignore.case = TRUE)) {
      group_name <- "Fecal/Gut Microbiota"
    } else if (grepl("Vaginal|Cervical", folder, ignore.case = TRUE)) {
      group_name <- "Vaginal Microbiota"
    } else if (grepl("Oral|Saliva", folder, ignore.case = TRUE)) {
      group_name <- "Oral Microbiota"
    } else if (grepl("Skin|Dermal", folder, ignore.case = TRUE)) {
      group_name <- "Skin Microbiota"
    } else if (grepl("Bacterial|16S", folder, ignore.case = TRUE)) {
      group_name <- "Bacterial Community"
    } else if (grepl("Fungal|ITS|Mycobiome", folder, ignore.case = TRUE)) {
      group_name <- "Fungal Community"
    } else if (grepl("Correlation|Association", folder, ignore.case = TRUE)) {
      group_name <- "Correlation Analyses"
    } else if (grepl("Diversity|Alpha|Beta", folder, ignore.case = TRUE)) {
      group_name <- "Diversity Analyses"
    } else if (grepl("Differential|Abundance", folder, ignore.case = TRUE)) {
      group_name <- "Differential Abundance"
    } else if (grepl("Composition|Overall", folder, ignore.case = TRUE)) {
      group_name <- "Composition Analyses"
    } else if (grepl("Extra|Additional|Supplementary|Addendum", folder, ignore.case = TRUE)) {
      group_name <- "Additional Analyses"
    }

    # Add to appropriate group
    if (!group_name %in% names(grouped)) {
      grouped[[group_name]] <- list()
    }
    grouped[[group_name]] <- c(grouped[[group_name]], list(report))
  }

  # Sort groups (keep "Other" at the end)
  group_order <- names(grouped)
  if ("Other Analyses" %in% group_order) {
    group_order <- c(setdiff(group_order, "Other Analyses"), "Other Analyses")
  }
  if ("Additional Analyses" %in% group_order) {
    group_order <- c(setdiff(group_order, "Additional Analyses"), "Additional Analyses")
  }

  return(grouped[group_order])
}


#' Group reports based on user-defined categories
#'
#' @param reports List of report metadata
#' @param report_groups Named list of patterns for each group
#' @return Named list of grouped reports
#' @keywords internal
custom_group_reports <- function(reports, report_groups) {
  grouped <- list()
  assigned <- character(0)

  # Assign reports to user-defined groups
  for (group_name in names(report_groups)) {
    patterns <- report_groups[[group_name]]
    grouped[[group_name]] <- list()

    for (report in reports) {
      if (report$folder %in% assigned) next

      for (pattern in patterns) {
        if (grepl(pattern, report$folder, ignore.case = TRUE)) {
          grouped[[group_name]] <- c(grouped[[group_name]], list(report))
          assigned <- c(assigned, report$folder)
          break
        }
      }
    }
  }

  # Add unassigned reports to "Other"
  unassigned <- Filter(function(r) !r$folder %in% assigned, reports)
  if (length(unassigned) > 0) {
    grouped[["Other Analyses"]] <- unassigned
  }

  # Remove empty groups
  grouped <- Filter(function(g) length(g) > 0, grouped)

  return(grouped)
}


#' Generate Quarto document content for master report
#'
#' @param project_name Project name from config
#' @param project_id Project ID from config
#' @param author_name Author name
#' @param study_description Study description text
#' @param grouped_reports Named list of grouped reports
#' @param include_metadata Whether to include metadata in report cards
#' @return Character vector of Quarto document lines
#' @keywords internal
generate_master_qmd <- function(
    project_name,
    project_id,
    author_name,
    study_description,
    grouped_reports,
    include_metadata
) {
  # YAML header
  yaml_header <- c(
    "---",
    paste0('title: "', project_name, ' - Statistical Analysis Report"'),
    paste0('subtitle: "Project: ', project_id, '"'),
    paste0('author: "', author_name, '"'),
    "date: today",
    'date-format: "DD-MMM-YYYY"',
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 3",
    "    toc-location: left",
    "    toc-expand: 2",
    "    toc-title: 'Contents'",
    "    code-fold: true",
    "    code-tools: false",
    "    theme:",
    "      light: cosmo",
    "      dark: darkly",
    "    self-contained: true",
    "    smooth-scroll: true",
    "    link-external-icon: true",
    "    link-external-newwindow: true",
    "    css: |",
    "      .report-card {",
    "        border: 1px solid #ddd;",
    "        border-radius: 8px;",
    "        padding: 15px;",
    "        margin: 10px 0;",
    "        transition: box-shadow 0.3s ease;",
    "        background: var(--bs-body-bg);",
    "      }",
    "      .report-card:hover {",
    "        box-shadow: 0 4px 12px rgba(0,0,0,0.15);",
    "      }",
    "      .report-card a {",
    "        text-decoration: none;",
    "        font-size: 1.1em;",
    "        font-weight: 500;",
    "      }",
    "      .report-meta {",
    "        font-size: 0.85em;",
    "        color: #666;",
    "        margin-top: 5px;",
    "      }",
    "      .section-header {",
    "        border-bottom: 2px solid #007bff;",
    "        padding-bottom: 5px;",
    "        margin-bottom: 15px;",
    "      }",
    "      .footer-branding {",
    "        text-align: center;",
    "        color: gray;",
    "        font-size: 14px;",
    "        margin-top: 40px;",
    "        padding: 20px;",
    "        border-top: 1px solid #ddd;",
    "      }",
    "---",
    ""
  )

  # Study Goal section
  study_section <- c(
    "## Study Goal {.section-header}",
    "",
    study_description,
    "",
    ""
  )

  # Generate report sections
  report_sections <- character(0)

  report_sections <- c(
    report_sections,
    "## Analysis Results {.section-header}",
    ""
  )

  for (group_name in names(grouped_reports)) {
    group_reports <- grouped_reports[[group_name]]

    report_sections <- c(
      report_sections,
      paste0("### ", group_name),
      "",
      "::: {.report-list}",
      ""
    )

    for (report in group_reports) {
      # Create report card
      card_lines <- c(
        "::: {.report-card}",
        paste0("[", report$title, "](", report$relative_path, ")"),
        ""
      )

      if (include_metadata) {
        meta_parts <- character(0)
        if (!is.na(report$date) && nchar(report$date) > 0) {
          meta_parts <- c(meta_parts, paste0("**Date:** ", report$date))
        }
        if (!is.na(report$author) && nchar(report$author) > 0) {
          meta_parts <- c(meta_parts, paste0("**Author:** ", report$author))
        }
        if (length(meta_parts) > 0) {
          card_lines <- c(
            card_lines,
            "::: {.report-meta}",
            paste(meta_parts, collapse = " | "),
            ":::",
            ""
          )
        }
      }

      card_lines <- c(card_lines, ":::", "")
      report_sections <- c(report_sections, card_lines)
    }

    report_sections <- c(report_sections, ":::", "")
  }

  # Footer
  footer <- c(
    "",
    "::: {.footer-branding}",
    "---",
    "",
    "**MosaBiome** - Maastricht Microbiome Service Facility",
    "",
    "---",
    ":::",
    ""
  )

  # Combine all sections
  c(yaml_header, study_section, report_sections, footer)
}


#' Add a Report Link to the Master Report
#'
#' @description
#' Adds a link to an HTML report in the master report's "Custom Reports" section.
#' This function can auto-detect the currently active .qmd file in RStudio/Positron
#' and find its corresponding rendered HTML report, or accept a direct path to an
#' HTML file.
#'
#' This is useful for adding reports that were not automatically discovered by
#' \code{\link{MBU_master_report}}, or for adding custom links with specific
#' display text.
#'
#' @param link_text Character string. The text to display for the link in the
#'   master report. This should be a descriptive title for the report.
#'   Example: \code{"1.A - Overall Microbiota Composition Analysis"}
#'
#' @param file Character string or NULL. Path to the source .qmd file or the
#'   rendered .html file. The function accepts either:
#'   \itemize{
#'     \item \code{NULL} (default): Auto-detects the currently active file in
#'       RStudio/Positron and finds its rendered HTML in \code{4.results_files/}.
#'     \item Path to a \code{.qmd} file: Finds the corresponding HTML in the
#'       results directory.
#'     \item Path to an \code{.html} file: Uses the HTML file directly.
#'   }
#'
#' @param section Character string. The section name under which to add the link.
#'   Defaults to \code{"Custom Reports"}. If the section doesn't exist in the
#'   master report, it will be created.
#'
#' @param master_report_name Character string. The name of the master report folder
#'   and file. Defaults to \code{"0.Statistical_Analysis_Report"}.
#'
#' @param render Logical. Whether to re-render the master report after adding the
#'   link. Defaults to \code{TRUE}.
#'
#' @param open_result Logical. Whether to open the re-rendered master report in
#'   the viewer. Defaults to \code{TRUE}. Only applies when \code{render = TRUE}.
#'
#' @return Invisible character string containing the path to the master report
#'   HTML file (if rendered) or .qmd file.
#'
#' @details
#' \strong{How It Works:}
#'
#' \enumerate{
#'   \item Detects or validates the target HTML report
#'   \item Reads the existing master report .qmd file
#'   \item Adds a new report card entry with the specified link text
#'   \item Inserts it under the specified section (creates section if needed)
#'   \item Optionally re-renders the master report
#' }
#'
#' \strong{Auto-Detection:}
#'
#' When \code{file = NULL}, the function:
#' \enumerate{
#'   \item Gets the currently active file from RStudio/Positron
#'   \item Extracts the base name (e.g., "my_analysis" from "my_analysis.qmd")
#'   \item Looks for the HTML in \code{4.results_files/my_analysis/my_analysis.html}
#' }
#'
#' \strong{HTML Code Generated:}
#'
#' The function generates a report card in the same format as \code{MBU_master_report()}:
#' \preformatted{
#' ::: \{.report-card\}
#' [Link Text](../folder_name/file.html)
#' :::
#' }
#'
#' @examples
#' \dontrun{
#' # Add the currently open report with custom text
#' MBU_add_to_master_report("1.A - Gut Microbiota Composition Between Groups")
#'
#' # Add a specific report file
#' MBU_add_to_master_report(
#'   link_text = "Supplementary Analysis - Extended Results",
#'   file = "4.results_files/supplementary_analysis/supplementary_analysis.html"
#' )
#'
#' # Add to a specific section
#' MBU_add_to_master_report(
#'   link_text = "Correlation Analysis",
#'   section = "Correlation Analyses"
#' )
#'
#' # Add without re-rendering (for batch additions)
#' MBU_add_to_master_report("Report 1", render = FALSE)
#' MBU_add_to_master_report("Report 2", file = "path/to/report2.html", render = FALSE)
#' MBU_add_to_master_report("Report 3", file = "path/to/report3.html", render = TRUE)
#' }
#'
#' @seealso
#' \code{\link{MBU_master_report}} to generate the master report
#'
#' \code{\link{MBU_render_report}} to render individual reports
#'
#' @family project management
#' @export
MBU_add_to_master_report <- function(
    link_text,
    file = NULL,
    section = "Custom Reports",
    master_report_name = "0.Statistical_Analysis_Report",
    render = TRUE,
    open_result = TRUE
) {

  # Validate link_text

if (missing(link_text) || is.null(link_text) || !nzchar(link_text)) {
    stop("link_text is required. Please provide descriptive text for the report link.")
  }

  # Validate we're in a MosaBiome project
  config_path <- here::here("project_config.yml")
  if (!file.exists(config_path)) {
    stop(
      "project_config.yml not found in project root.\n",
      "Are you in a MosaBiome project directory?"
    )
  }

  # Read project configuration
  config <- yaml::read_yaml(config_path)
  base_output_dir <- config$output_dir %||% "4.results_files"

  # Resolve the HTML file path
  html_path <- resolve_html_report_path(file, base_output_dir)

  # Get the master report .qmd file path
  master_qmd_path <- here::here(base_output_dir, master_report_name,
                                 paste0(master_report_name, ".qmd"))
  master_html_path <- here::here(base_output_dir, master_report_name,
                                  paste0(master_report_name, ".html"))

  # Check if master report exists
  if (!file.exists(master_qmd_path)) {
    stop(
      "Master report not found at: ", master_qmd_path, "\n",
      "Run MBU_master_report() first to create the master report."
    )
  }

  # Calculate relative path from master report to the HTML file
  html_folder <- basename(dirname(html_path))
  html_filename <- basename(html_path)
  relative_path <- paste0("../", html_folder, "/", html_filename)

  # Generate the report card HTML/Markdown
  report_card <- c(
    "",
    "::: {.report-card}",
    paste0("[", link_text, "](", relative_path, ")"),
    ":::",
    ""
  )

  # Read the existing master report
  master_content <- readLines(master_qmd_path)

  # Find or create the section
  section_header <- paste0("### ", section)
  section_index <- grep(paste0("^### ", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", section), "$"),
                        master_content)

  if (length(section_index) == 0) {
    # Section doesn't exist - add it before the footer
    footer_index <- grep("^::: \\{\\.footer-branding\\}", master_content)

    if (length(footer_index) == 0) {
      # No footer found, append at the end
      insert_position <- length(master_content)
    } else {
      # Insert before footer
      insert_position <- footer_index[1] - 1
    }

    # Create new section with the report card
    new_section <- c(
      "",
      section_header,
      "",
      "::: {.report-list}",
      report_card,
      ":::",
      ""
    )

    master_content <- c(
      master_content[1:insert_position],
      new_section,
      master_content[(insert_position + 1):length(master_content)]
    )

    message("Created new section '", section, "' in master report")
  } else {
    # Section exists - find the end of the section's report-list div
    # Look for the closing ::: after the section header
    section_start <- section_index[1]

    # Find the ::: {.report-list} after this section
    report_list_start <- NULL
    for (i in (section_start + 1):min(section_start + 10, length(master_content))) {
      if (grepl("^::: \\{\\.report-list\\}", master_content[i])) {
        report_list_start <- i
        break
      }
    }

    if (is.null(report_list_start)) {
      # No report-list found, create one
      insert_position <- section_start + 1
      new_content <- c(
        "",
        "::: {.report-list}",
        report_card,
        ":::",
        ""
      )
      master_content <- c(
        master_content[1:insert_position],
        new_content,
        master_content[(insert_position + 1):length(master_content)]
      )
    } else {
      # Find the closing ::: for this report-list
      # Count nested divs to find the matching close
      nesting <- 1
      close_index <- NULL
      for (i in (report_list_start + 1):length(master_content)) {
        if (grepl("^:::\\s*$", master_content[i])) {
          nesting <- nesting - 1
          if (nesting == 0) {
            close_index <- i
            break
          }
        } else if (grepl("^:::", master_content[i])) {
          nesting <- nesting + 1
        }
      }

      if (!is.null(close_index)) {
        # Insert before the closing :::
        master_content <- c(
          master_content[1:(close_index - 1)],
          report_card,
          master_content[close_index:length(master_content)]
        )
      }
    }

    message("Added report to section '", section, "'")
  }

  # Write the updated master report
  writeLines(master_content, master_qmd_path)
  message("Updated master report: ", master_qmd_path)

  # Render if requested
  if (render) {
    message("Re-rendering master report...")

    tryCatch({
      quarto::quarto_render(input = master_qmd_path, output_format = "html")

      message("Successfully rendered: ", master_html_path)

      # Open result if requested
      if (open_result && file.exists(master_html_path)) {
        if (requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::hasFun("viewer")) {
          rstudioapi::viewer(master_html_path)
        } else {
          utils::browseURL(master_html_path)
        }
      }

      invisible(master_html_path)
    }, error = function(e) {
      warning("Rendering failed: ", e$message, "\n",
              "The .qmd file was updated at: ", master_qmd_path)
      invisible(master_qmd_path)
    })
  } else {
    message("\nTo render the master report, run:")
    message("  quarto::quarto_render('", master_qmd_path, "')")
    invisible(master_qmd_path)
  }
}


#' Resolve the path to an HTML report
#'
#' @description
#' Internal function that resolves the HTML report path from either:
#' - Auto-detection of the current active file
#' - A provided .qmd file path
#' - A provided .html file path
#'
#' @param file Character string or NULL. The file path or NULL for auto-detection.
#' @param base_output_dir Character string. The base output directory.
#' @return Character string with the full path to the HTML file.
#' @keywords internal
resolve_html_report_path <- function(file, base_output_dir) {

  if (is.null(file)) {
    # Auto-detect current file
    if (!requireNamespace("rstudioapi", quietly = TRUE) ||
        !rstudioapi::hasFun("getSourceEditorContext")) {
      stop(
        "Could not detect current file (rstudioapi not available).\n",
        "Please provide the file path explicitly."
      )
    }

    context <- rstudioapi::getSourceEditorContext()
    file <- context$path

    if (is.null(file) || file == "") {
      stop(
        "Could not detect current file. Please either:\n",
        "  1. Save your file first, or\n",
        "  2. Provide the file path: MBU_add_to_master_report('Link Text', file = 'path/to/file')"
      )
    }
  }

  # Determine if it's a .qmd or .html file
  if (grepl("\\.qmd$", file, ignore.case = TRUE)) {
    # It's a .qmd file - find the corresponding HTML
    file_basename <- tools::file_path_sans_ext(basename(file))
    html_path <- here::here(base_output_dir, file_basename,
                            paste0(file_basename, ".html"))

    if (!file.exists(html_path)) {
      stop(
        "HTML report not found at: ", html_path, "\n",
        "Make sure to render the report first using MBU_render_report()"
      )
    }
  } else if (grepl("\\.html$", file, ignore.case = TRUE)) {
    # It's an HTML file
    if (!file.exists(file)) {
      # Try relative to project root
      file <- here::here(file)
    }

    if (!file.exists(file)) {
      stop("HTML file not found: ", file)
    }

    html_path <- normalizePath(file)
  } else {
    # Assume it's a folder name or base name
    file_basename <- tools::file_path_sans_ext(basename(file))
    html_path <- here::here(base_output_dir, file_basename,
                            paste0(file_basename, ".html"))

    if (!file.exists(html_path)) {
      stop(
        "Could not find HTML report for: ", file, "\n",
        "Expected location: ", html_path
      )
    }
  }

  return(html_path)
}


# Null coalescing operator (if not already defined elsewhere)
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}
