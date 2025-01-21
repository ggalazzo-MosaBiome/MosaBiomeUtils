# R/zzz.R

.onAttach <- function(libname, pkgname) {
  # Check if the author name is already set in the current session
  author_name <- getOption("MosaBiomeUtils.author")
  if (is.null(author_name)) {
    if (interactive()) {
      rprofile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")

      # Ensure .Rprofile exists
      if (!file.exists(rprofile_path)) {
        file.create(rprofile_path)
      }

      # Prompt for the author's name
      cat("Welcome to MosaBiomeUtils!\n")
      author_name <- readline("Please enter your full name (e.g., John Doe): ")

      # Save the name in .Rprofile and the current session
      tryCatch({
        rprofile_content <- readLines(rprofile_path, warn = FALSE)
        if (!any(grepl("options\\(MosaBiomeUtils.author", rprofile_content))) {
          writeLines(
            c(rprofile_content, paste0("options(MosaBiomeUtils.author = \"", author_name, "\")")),
            con = rprofile_path
          )
          message("Your name has been saved in .Rprofile and will be used in templates.")
        }
        # Set the author name in the current session
        options(MosaBiomeUtils.author = author_name)
      }, error = function(e) {
        warning("Failed to update .Rprofile: ", e$message)
      })
    } else {
      # Non-interactive session: Provide a default message
      packageStartupMessage(
        "Author name not set. Please run `MosaBiomeUtils::set_author_name()` in an interactive session."
      )
      author_name <- "Unknown Author"
    }
  }

  # Set the author name in the current session (if not already set)
  options(MosaBiomeUtils.author = author_name)

  # Display a startup message with the author name
  packageStartupMessage("Welcome to MosaBiomeUtils, ", author_name, "!")
}


.onLoad <- function(libname, pkgname) {
  rprofile_path <- file.path(Sys.getenv("HOME"), ".Rprofile")
  if (file.exists(rprofile_path)) {
    source(rprofile_path)
  }

  if (is.null(getOption("MosaBiomeUtils.author"))) {
    options(MosaBiomeUtils.author = NULL)  # Initialize to NULL if still missing
  }
}
