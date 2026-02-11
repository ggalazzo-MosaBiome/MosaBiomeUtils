#' Underline Selected Text with HTML Tags
#'
#' @description
#' RStudio/Positron addin that wraps the currently selected text with HTML
#' underline tags (\code{<u>} and \code{</u>}). This is useful when writing
#' Quarto or RMarkdown reports where you want to emphasize text with underlining
#' in HTML output.
#'
#' @return Invisible \code{NULL}. Called for its side effect of replacing the
#'   selected text with the HTML-wrapped version.
#'
#' @details
#' \strong{How to Use:}
#'
#' \enumerate{
#'   \item Select text in your editor (Quarto, RMarkdown, or HTML file)
#'   \item Run the addin via: \emph{Addins > Underline for HTML}
#'   \item The selected text is replaced with \code{<u> selected text </u>}
#' }
#'
#' \strong{Example Transformation:}
#'
#' \tabular{ll}{
#'   \strong{Before} \tab \code{important finding} \cr
#'   \strong{After} \tab \code{<u> important finding </u>}
#' }
#'
#' \strong{Setting Up a Keyboard Shortcut:}
#'
#' For frequent use, assign a keyboard shortcut:
#' \enumerate{
#'   \item Go to \emph{Tools > Modify Keyboard Shortcuts}
#'   \item Search for "Underline for HTML"
#'   \item Assign your preferred shortcut (e.g., \code{Ctrl+Shift+U})
#' }
#'
#' \strong{Note on HTML Rendering:}
#'
#' The \code{<u>} tag renders as underlined text in HTML output. It will not
#' affect PDF or Word output unless you have custom CSS/styling configured.
#' For cross-format underlining, consider using Quarto's native formatting
#' or custom CSS classes.
#'
#' @section Requirements:
#'
#' \itemize{
#'   \item Must be run from within RStudio or Positron IDE
#'   \item Requires the \pkg{rstudioapi} package
#'   \item Text must be selected before running
#' }
#'
#' @examples
#' \dontrun{
#' # This function is typically called via the Addins menu,
#' # not directly from the console.
#'
#' # However, you can call it programmatically:
#' underlineHtml()
#' # (Wraps currently selected text with <u> tags)
#' }
#'
#' @seealso
#' \code{\link{wrapInQuote}} for another text manipulation addin
#'
#' \code{\link{MB_style_report}} for code formatting addin
#'
#' @family addins
#' @export
underlineHtml <- function() {
  selected_text <- rstudioapi::getActiveDocumentContext()$selection[[1]]$text

  # Wrap selected text with HTML underline tags
  formatted_text <- paste0("<u> ", selected_text, "</u>")

  rstudioapi::insertText(formatted_text)
}
