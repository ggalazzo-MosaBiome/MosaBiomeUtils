#' MosaBiome Custom Code Style Transformers
#'
#' @description
#' Returns a set of custom \pkg{styler} transformers that implement the MosaBiome
#' code formatting conventions. These transformers extend the tidyverse style
#' with additional rules for improved readability in statistical reports.
#'
#' @return A named list of styler transformer functions that can be passed to
#'   \code{\link[styler]{style_text}} or \code{\link[styler]{style_file}} via
#'   the \code{transformers} argument.
#'
#' @details
#' \strong{Style Rules Applied:}
#'
#' The MosaBiome style extends \code{styler::tidyverse_style(strict = TRUE)}
#' with the following additional formatting rules:
#'
#' \enumerate{
#'   \item \strong{Newlines After Commas}: Forces a newline after each comma
#'     in function calls. This improves readability for functions with multiple
#'     arguments like \code{data.frame()}, \code{mutate()}, etc.
#'
#'   \item \strong{Newlines After Pipes}: Forces a newline after each pipe
#'     operator (\code{\%>\%}). This ensures each step in a pipeline is on
#'     its own line for better readability.
#'
#'   \item \strong{Function Argument Formatting}: Places opening and closing
#'     parentheses on their own lines for function calls with multiple arguments.
#' }
#'
#' \strong{Example Transformation:}
#'
#' \emph{Before:}
#' \preformatted{
#' data \%>\% filter(x > 0) \%>\% mutate(y = x * 2, z = y + 1) \%>\% select(y, z)
#' }
#'
#' \emph{After:}
#' \preformatted{
#' data \%>\%
#'   filter(x > 0) \%>\%
#'   mutate(
#'     y = x * 2,
#'     z = y + 1
#'   ) \%>\%
#'   select(
#'     y,
#'     z
#'   )
#' }
#'
#' \strong{Usage with styler:}
#'
#' You can use these transformers directly with styler functions:
#'
#' \preformatted{
#' # Style a text string
#' styler::style_text(
#'   "x\%>\%y\%>\%z",
#'   transformers = report_custom_transformers()
#' )
#'
#' # Style a file
#' styler::style_file(
#'   "my_script.R",
#'   transformers = report_custom_transformers()
#' )
#' }
#'
#' @section Dependencies:
#'
#' Requires the \pkg{styler} package to be installed.
#'
#' @examples
#' \dontrun{
#' # Get the transformers
#' transformers <- report_custom_transformers()
#'
#' # Use with style_text
#' code <- "df %>% filter(x > 0) %>% select(a, b, c)"
#' styler::style_text(code, transformers = transformers)
#'
#' # The MB_style_report() addin uses these transformers internally
#' }
#'
#' @seealso
#' \code{\link{MB_style_report}} addin that applies these transformers
#'
#' \code{\link[styler]{tidyverse_style}} the base style being extended
#'
#' \code{\link[styler]{style_text}} for programmatic styling
#'
#' @family code formatting
#' @export
report_custom_transformers <- function() {
  transformers <- styler::tidyverse_style(strict = TRUE)

  # Force newlines after commas in function calls (e.g., data.frame, slice_max)
  transformers$line_break$add_line_break_after_commas <- function(pd) {
    idx <- which(pd$token == "COMMA")
    if (length(idx) > 0) pd$lag_newlines[idx] <- 1
    pd
  }

  # Force line break after %>%, +, ==, etc. (not before!)
  transformers$line_break$add_line_break_after_pipes <- function(pd) {
    tryCatch({
      pipe_idx <- which(trimws(pd$text) == "%>%")
      for (i in pipe_idx) {
        if (i + 1 <= nrow(pd)) {
          pd$lag_newlines[i + 1] <- 1
        }
      }
      pd
    }, error = function(e) {
      message("Styler transformer 'add_line_break_after_pipes' failed: ", conditionMessage(e))
      pd  # return unmodified pd in case of error
    })
  }

  # Function calls: each argument on a new line
  transformers$token$force_function_args_newlines <- function(pd) {
    if ("SYMBOL_FUNCTION_CALL" %in% pd$token) {
      lp <- which(pd$token == "LPAREN")
      rp <- which(pd$token == "RPAREN")
      if (length(lp) > 0) pd$lag_newlines[lp] <- 1
      if (length(rp) > 0) pd$lead_newlines[rp] <- 1
    }
    pd
  }

  transformers
}
