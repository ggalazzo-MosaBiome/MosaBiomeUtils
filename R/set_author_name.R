#' Set or Update Author Name for Report Templates
#'
#' @description
#' Interactively set or update the author name that is automatically inserted
#' into report templates created by \code{\link{MBU_new_analysis}}. The name is
#' stored both in the current R session and permanently in your \code{.Rprofile}
#' file for persistence across sessions.
#'
#' @return Invisible \code{NULL}. Called for its side effects:
#'   \itemize{
#'     \item Sets \code{options(MosaBiomeUtils.author = "Your Name")} for the current session
#'     \item Writes/updates the setting in \code{~/.Rprofile} for future sessions
#'   }
#'
#' @details
#' \strong{How Author Names Are Used:}
#'
#' The author name is embedded in Quarto report templates and appears in:
#' \itemize{
#'   \item The report YAML header (\code{author:} field)
#'   \item The project README file
#'   \item Any generated documentation
#' }
#'
#' \strong{Storage Mechanism:}
#'
#' The author name is stored using R's options system:
#' \itemize{
#'   \item Option name: \code{MosaBiomeUtils.author}
#'   \item Retrieval: \code{getOption("MosaBiomeUtils.author")}
#'   \item Persistent storage: \code{~/.Rprofile}
#' }
#'
#' \strong{First-Time Setup:}
#'
#' When MosaBiomeUtils is first loaded, if no author name is configured,
#' you will be automatically prompted to enter your name. This function
#' allows you to change that name later.
#'
#' \strong{.Rprofile Behavior:}
#'
#' \itemize{
#'   \item If \code{~/.Rprofile} doesn't exist, it will be created
#'   \item If an author name entry already exists, it will be updated
#'   \item If no entry exists, a new line will be appended
#' }
#'
#' The entry in \code{.Rprofile} looks like:
#' \preformatted{
#' options(MosaBiomeUtils.author = "Your Name")
#' }
#'
#' @section Manual Configuration:
#'
#' You can also set the author name manually without using this function:
#'
#' \preformatted{
#' # For current session only:
#' options(MosaBiomeUtils.author = "Dr. Jane Smith")
#'
#' # Or add to your .Rprofile manually:
#' # options(MosaBiomeUtils.author = "Dr. Jane Smith")
#' }
#'
#' @examples
#' \dontrun{
#' # Change your author name (interactive prompt)
#' set_author_name()
#' # Enter your full name (e.g., John Doe): Dr. Jane Smith
#' # Author name updated to 'Dr. Jane Smith' and saved in .Rprofile.
#'
#' # Check current author name
#' getOption("MosaBiomeUtils.author")
#' # [1] "Dr. Jane Smith"
#' }
#'
#' @seealso
#' \code{\link{MBU_new_analysis}} which uses the author name in templates
#'
#' \code{\link{MBU_create_new_project}} which includes the author in project README
#'
#' @family configuration
#' @export
set_author_name <- function() {
  rprofile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")

  # Prompt for the author's name
  author_name <- readline("Enter your full name (e.g., John Doe): ")

  # Save the new name in .Rprofile
  tryCatch({
    if (!file.exists(rprofile_path)) {
      file.create(rprofile_path)
    }

    rprofile_content <- readLines(rprofile_path, warn = FALSE)
    if (any(grepl("options\\(MosaBiomeUtils.author", rprofile_content))) {
      # Update existing entry
      rprofile_content <- gsub(
        "options\\(MosaBiomeUtils.author = \".*\"\\)",
        paste0("options(MosaBiomeUtils.author = \"", author_name, "\")"),
        rprofile_content
      )
    } else {
      # Add a new entry
      rprofile_content <- c(rprofile_content, paste0("options(MosaBiomeUtils.author = \"", author_name, "\")"))
    }
    writeLines(rprofile_content, rprofile_path)

    # Set the author name for the current session
    options(MosaBiomeUtils.author = author_name)
    message("Author name updated to '", author_name, "' and saved in .Rprofile.")
  }, error = function(e) {
    stop("Failed to update .Rprofile: ", e$message)
  })
}
