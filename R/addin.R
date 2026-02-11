#' Format Code with MosaBiome Style
#'
#' @description
#' RStudio/Positron addin that applies the MosaBiome code formatting style to
#' either the selected code or the entire active document. This ensures consistent
#' code style across all MosaBiome analysis reports.
#'
#' The function uses \pkg{styler} with custom transformers defined in
#' \code{\link{report_custom_transformers}}.
#'
#' @return Invisible \code{NULL}. Called for its side effect of reformatting
#'   code in the active editor.
#'
#' @details
#' \strong{Behavior:}
#'
#' \itemize{
#'   \item \strong{With Selection}: If text is selected, only the selected code
#'     is reformatted. The selection is replaced with the styled version.
#'   \item \strong{Without Selection}: If no text is selected, the entire
#'     document is reformatted.
#' }
#'
#' \strong{Style Rules Applied:}
#'
#' See \code{\link{report_custom_transformers}} for details on the formatting
#' rules. In summary:
#' \itemize{
#'   \item Tidyverse style (strict mode) as base
#'   \item Newlines after commas in function arguments
#'   \item Newlines after pipe operators (\code{\%>\%})
#'   \item Function arguments on separate lines
#' }
#'
#' \strong{How to Use:}
#'
#' \enumerate{
#'   \item Open an R or Quarto file in RStudio/Positron
#'   \item Optionally select the code you want to format
#'   \item Run via: \emph{Addins > MosaBiome Style Report}
#'   \item The code is reformatted in place
#' }
#'
#' \strong{Setting Up a Keyboard Shortcut:}
#'
#' For frequent use, assign a keyboard shortcut:
#' \enumerate{
#'   \item Go to \emph{Tools > Modify Keyboard Shortcuts}
#'   \item Search for "MosaBiome Style Report"
#'   \item Assign your preferred shortcut (e.g., \code{Ctrl+Shift+S})
#' }
#'
#' \strong{Example Transformation:}
#'
#' \emph{Before:}
#' \preformatted{
#' result <- data \%>\% filter(x>0) \%>\% mutate(y=x*2,z=y+1)
#' }
#'
#' \emph{After:}
#' \preformatted{
#' result <- data \%>\%
#'   filter(x > 0) \%>\%
#'   mutate(
#'     y = x * 2,
#'     z = y + 1
#'   )
#' }
#'
#' @section Requirements:
#'
#' \itemize{
#'   \item Must be run from within RStudio or Positron IDE
#'   \item Requires the \pkg{rstudioapi} package
#'   \item Requires the \pkg{styler} package
#' }
#'
#' @section Caution:
#'
#' \itemize{
#'   \item This modifies your code in place - ensure you have saved or can undo
#'   \item For large files, formatting may be slow
#'   \item Some complex expressions may not format as expected
#' }
#'
#' @examples
#' \dontrun{
#' # This function is typically called via the Addins menu,
#' # not directly from the console.
#'
#' # To format code programmatically, use styler directly:
#' styler::style_text(
#'   "x\%>\%y\%>\%z",
#'   transformers = report_custom_transformers()
#' )
#' }
#'
#' @seealso
#' \code{\link{report_custom_transformers}} for the style rules used
#'
#' \code{\link[styler]{style_text}} for programmatic styling
#'
#' \code{\link{wrapInQuote}} and \code{\link{underlineHtml}} for other addins
#'
#' @family addins
#' @family code formatting
#' @export
MB_style_report <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("RStudio API is not available.")
  }

  context <- rstudioapi::getActiveDocumentContext()
  selection <- context$selection[[1]]$text
  has_selection <- nchar(selection) > 0

  if (has_selection) {
    # Style just the selected code
    styled <- styler::style_text(
      text = selection,
      transformers = MosaBiomeUtils::report_custom_transformers()
    )

    rstudioapi::modifyRange(
      location = context$selection[[1]]$range,
      text = paste(styled, collapse = "\n"),
      id = context$id
    )
  } else {
    # Style the entire document
    styled <- styler::style_text(
      text = context$contents,
      transformers = MosaBiomeUtils::report_custom_transformers()
    )

    full_range <- rstudioapi::document_range(
      start = rstudioapi::document_position(1, 0),
      end = rstudioapi::document_position(length(context$contents), nchar(context$contents[length(context$contents)]))
    )

    rstudioapi::modifyRange(
      location = full_range,
      text = paste(styled, collapse = "\n"),
      id = context$id
    )
  }
}
