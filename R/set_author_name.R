# R/set_author_name.R

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
