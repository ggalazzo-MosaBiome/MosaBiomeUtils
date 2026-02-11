#' Wrap Vector Elements in Quotes
#'
#' @description
#' RStudio/Positron addin that converts a selection of unquoted values into
#' a properly formatted R character vector. Each element is wrapped in double
#' quotes and separated by commas with newlines for readability.
#'
#' The function automatically detects whether elements are separated by commas
#' or whitespace.
#'
#' @return Invisible \code{NULL}. Called for its side effect of replacing the
#'   selected text with the quoted, formatted version.
#'
#' @details
#' \strong{How to Use:}
#'
#' \enumerate{
#'   \item Select a list of values in your editor (comma or space-separated)
#'   \item Run the addin via: \emph{Addins > Wrap in quotes}
#'   \item The selection is replaced with properly quoted elements
#' }
#'
#' \strong{Example Transformations:}
#'
#' \emph{Comma-separated input:}
#' \preformatted{
#' apple, banana, cherry
#' }
#' \emph{Becomes:}
#' \preformatted{
#' "apple",
#' "banana",
#' "cherry"
#' }
#'
#' \emph{Space-separated input:}
#' \preformatted{
#' red green blue
#' }
#' \emph{Becomes:}
#' \preformatted{
#' "red",
#' "green",
#' "blue"
#' }
#'
#' \emph{Single element:}
#' \preformatted{
#' single_value
#' }
#' \emph{Becomes:}
#' \preformatted{
#' "single_value"
#' }
#'
#' \strong{Common Use Cases:}
#'
#' \itemize{
#'   \item Converting column names copied from a spreadsheet into R vectors
#'   \item Quickly creating character vectors from plain text lists
#'   \item Formatting factor levels or category names
#'   \item Converting command output into R code
#' }
#'
#' \strong{Delimiter Detection:}
#'
#' The function uses the following priority for detecting delimiters:
#' \enumerate{
#'   \item Comma (\code{,}) - if present, used as separator
#'   \item Whitespace (spaces, tabs) - if no commas, split on whitespace
#'   \item None - if neither, treat entire selection as single element
#' }
#'
#' \strong{Setting Up a Keyboard Shortcut:}
#'
#' For frequent use, assign a keyboard shortcut:
#' \enumerate{
#'   \item Go to \emph{Tools > Modify Keyboard Shortcuts}
#'   \item Search for "Wrap in quotes"
#'   \item Assign your preferred shortcut (e.g., \code{Ctrl+Shift+Q})
#' }
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
#' # Example workflow:
#' # 1. Copy column names from Excel: "Name Age City"
#' # 2. Paste into R script
#' # 3. Select the text
#' # 4. Run Addins > Wrap in quotes
#' # 5. Result:
#' #    "Name",
#' #    "Age",
#' #    "City"
#'
#' # You can then wrap with c() to create a vector:
#' cols <- c(
#'   "Name",
#'   "Age",
#'   "City"
#' )
#' }
#'
#' @seealso
#' \code{\link{underlineHtml}} for another text manipulation addin
#'
#' \code{\link{MB_style_report}} for code formatting addin
#'
#' @family addins
#' @export
wrapInQuote <- function() {
  selected_text <- rstudioapi::getActiveDocumentContext()$selection[[1]]$text

  # Detect the delimiter (comma, space, or others)
  if (grepl(",", selected_text)) {
    separator <- ","
  } else if (grepl("\\s", selected_text)) {
    separator <- "\\s+"
  } else {
    separator <- ""
  }

  # Split the text by the detected separator
  elements <- if (separator != "") {
    trimws(strsplit(selected_text, separator)[[1]])
  } else {
    c(selected_text)
  }

  # Format each element with quotes and a comma at the end
  formatted_text <- paste0("\"", elements, "\",", collapse = "\n")

  # Remove the trailing comma from the last element
  formatted_text <- sub(",$", "", formatted_text)

  rstudioapi::insertText(formatted_text)
}
